open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let msg_syntaxErrorDecl = "Syntax error for toMeta annotation. Annotation is of the form [@@static <statVars>] where statVars = [] | sv::statVars"
let msg_syntaxErrorUse = "Syntax error for toMeta annotation. Annotation is of the form [@static,use <statVars>] where statVars = [] | sv::statVars"

let idx = ref 0
let fresh v =
  idx := !idx + 1;
  v^"_"^(string_of_int !idx)

let applyFun = fun funTargets funName loc ->
  Exp.apply ~loc ~attrs:[]
    (Exp.ident ~loc ~attrs:[] {loc = loc; txt = Lident funName})
    (List.map (fun ft ->  ("", ft)) funTargets)

let applyMetaOCamlConstr = fun constrTarget constrName loc ->
  Exp.mk ~loc 
    ~attrs:([({txt = constrName; loc = loc}, PStr [])] @ constrTarget.pexp_attributes) 
    constrTarget.pexp_desc

let applyLift = fun liftTarget loc ->
  applyMetaOCamlConstr liftTarget "metaocaml.bracket" loc

let applyEsc = fun escTarget loc ->
  applyMetaOCamlConstr escTarget "metaocaml.escape" loc

let applyRun = fun runTarget loc ->
  applyFun [runTarget] "Runcode.run" loc

let getStagedName = fun fname statVars ->
  fname^"_"^(List.fold_left (fun acc v -> acc^v) "" statVars)

let isRecursive = fun funDef ->
  match funDef with
    {pstr_desc = Pstr_value (Nonrecursive, _)} -> false
    | {pstr_desc = Pstr_value (Recursive, _)} -> true
    | _ -> failwith "not a function definition"
    
let isStaticExp exp statVars =
  let rec aux exp = 
    match exp with
      {pexp_desc = Pexp_ifthenelse (condExp, thenExp, elseExpOpt); pexp_attributes = attrs; pexp_loc = loc} ->
          let elseStat =
            begin match elseExpOpt with
              None -> true
              | Some exp -> aux exp
            end in
          (aux condExp) && (aux thenExp) && elseStat
      | {pexp_desc = Pexp_apply (fn, argList); pexp_attributes = attrs; pexp_loc = loc} ->
          List.fold_left (fun acc (_, exp) -> acc && (aux exp)) true argList
      | {pexp_desc = Pexp_ident {txt = Lident v; loc = loc}} ->
          List.exists (fun sv -> sv=v) statVars
      | exp -> true
  in aux exp

let getVars = fun funDef ->
  let pvb_expr = 
    match funDef with
      {pstr_desc = Pstr_value (_, value_binding_list)} ->
          (List.hd value_binding_list).pvb_expr
      | _ -> failwith "not a function definition"
  in let rec aux pvb_expr =
       match pvb_expr with
         {pexp_desc = Pexp_fun (_, _, {ppat_desc = Ppat_var {txt = v}}, pvb_expr)} -> 
             v::(aux pvb_expr)
         | _ -> []
  in aux pvb_expr

let getAttrList = fun funDef ->
  match funDef with
    {pstr_desc = Pstr_value (_, value_binding_list)} ->
      (* value binding is a list for let .. and .. and .. *)
      (* TODO: handle ands *)
      (List.hd value_binding_list).pvb_attributes
    | _ -> failwith "not a function definition"

let hasToMetaAnnot = fun funDef ->
  let attrList = getAttrList funDef in
  let rec aux attrs =
    match attrs with
      [] -> false
      | ({txt = "static"}, _)::_ -> true
      | _::attrs -> aux attrs
  in aux attrList

let getStatVars = fun funDef ->
  let attrList = getAttrList funDef in
  let rec aux attrs =
    match attrs with
      [] -> []
      | ({txt = "static"}, PStr [{pstr_desc = Pstr_eval (construct, _)}])::attrs ->
          let rec aux2 construct =
            begin match construct with
              | Pexp_construct ({txt = Lident "[]"}, None) -> []
              | Pexp_construct ({txt = Lident "::"}, 
                                Some {pexp_desc = Pexp_tuple [{pexp_desc = Pexp_ident {txt = Lident v}}; construct]}) ->
                  v::(aux2 construct.pexp_desc)
              | _ -> failwith msg_syntaxErrorDecl
            end in
          let statVars = aux2 construct.pexp_desc in
          statVars::(aux attrs)
      | ({txt = "static"}, _)::attrs -> failwith msg_syntaxErrorDecl
      | _::attrs -> aux attrs
  in aux attrList

let rec removeArguments funBody n =
  if n = 0
    then funBody 
    else 
      match funBody with
        {pexp_desc = Pexp_fun (_, _, _, funBody)} -> removeArguments funBody (n-1)
        | _ -> failwith "arguments missing?"

let buildAuxCall = fun vars dynVars loc ->
  let newBodyArgs =
    List.map
      (fun v ->
           let identExp = Exp.ident ~loc ~attrs:[] {loc = loc; txt = Lident v} in
           if List.exists (fun dv -> v=dv) dynVars 
             then ("", applyLift identExp loc)
             else ("", identExp))
      vars
  in Exp.apply ~loc ~attrs:[] 
       (Exp.ident ~loc ~attrs:[] {txt = Lident "aux"; loc = loc})
       newBodyArgs
  
let rec buildAuxBody = fun funBody vars statVars dynVars funName loc ->
  let rec sub exp inEsc =
    match exp with
      {pexp_desc = Pexp_ifthenelse (condExp, thenExp, elseExpOpt); pexp_attributes = attrs; pexp_loc = loc} ->
          let (thenAux, thenExp') = buildStagedBody thenExp vars statVars dynVars funName loc in
          let (elseAux, elseExpOpt') = 
            begin match elseExpOpt with
              None -> ([], None)
              | Some elseExp -> 
                  let (aux, e) = buildStagedBody elseExp vars statVars dynVars funName loc in
                  (aux, Some e)
            end in
          Exp.ifthenelse ~loc ~attrs condExp thenExp' elseExpOpt'
      | {pexp_desc = Pexp_apply (fn, argList); pexp_attributes = attrs; pexp_loc = loc} ->
          let fname = 
            begin match fn with 
              {pexp_desc = Pexp_ident {txt = Lident fname}} -> fname
              | _ -> "_AnonFun"
            end in
          let fn' = 
            if fname = funName
              then Exp.ident ~loc ~attrs:[] {loc = loc; txt = Lident "aux"}
              else fn
          in let argList' = 
            let rec aux args =
              match args with
                [] -> []
                | (lbl, exp)::args -> (lbl, sub exp (fname = funName))::(aux args)
            in aux argList
          in let e = Exp.apply ~loc ~attrs fn' argList' in
          if fname = funName then applyEsc e loc else e
      | {pexp_desc = Pexp_ident {txt = Lident v; loc = loc}} ->
          if not inEsc && (List.exists (fun dv -> v=dv) dynVars)
            then applyEsc funBody loc
            else funBody
      | exp -> exp
  in sub funBody false
 
and buildStagedBody = fun funBody vars statVars dynVars funName loc ->
  let rec stage exp : Parsetree.expression list * Parsetree.expression =
    match exp with
      {pexp_desc = Pexp_ifthenelse (condExp, thenExp, elseExpOpt); pexp_attributes = attrs; pexp_loc = loc} ->
          if isStaticExp condExp statVars
            then
              let auxBody = buildAuxBody exp vars statVars dynVars funName loc in
              let auxCall = buildAuxCall vars dynVars loc in
              ([auxBody], auxCall)
            else
              let (condAux, condExp') = 
                let (aux, e) = stage condExp in 
                (aux, applyEsc e loc)
              in let (thenAux, thenExp') =
                let (aux, e) = stage thenExp in 
                (aux, applyEsc e loc)
              in let (elseAux, elseExpOpt') =
                begin match elseExpOpt with
                  None -> ([], None)
                  | Some elseExp -> 
                    let (elseAux, elseExp') =
                      let (aux, e) = stage elseExp in 
                      (aux, applyEsc e loc)
                    in (elseAux, Some elseExp')
                end in
              let body = Exp.ifthenelse ~loc ~attrs condExp' thenExp' elseExpOpt' in
              (condAux@thenAux@elseAux, applyLift body loc)
      | {pexp_desc = Pexp_apply (fn, argList); pexp_attributes = attrs; pexp_loc = loc} ->
          let fname = 
            begin match fn with 
              {pexp_desc = Pexp_ident {txt = Lident fname}} -> fname
              | _ -> "_AnonFun"
            end in
          let aux_stagedArgs = 
            let rec aux args =
              begin match args with
                [] -> []
                | (lbl, exp)::args -> 
                    begin match exp with
                      {pexp_desc = Pexp_ident {txt = Lident v}} ->
                          if (fname = funName) && List.exists (fun sv -> sv=v) statVars
                            then aux args
                            else (lbl, stage exp)::(aux args)
                      | exp -> (lbl, stage exp)::(aux args)
                    end
              end in
            aux argList in
          let auxs = List.flatten (List.map (fun (lbl, (aux, arg)) -> aux) aux_stagedArgs) in
          let args = List.map (fun (lbl, (aux, arg)) -> (lbl, applyEsc arg loc)) aux_stagedArgs in
          let e = Exp.apply ~loc ~attrs fn args in
          (auxs, applyLift e loc)
      | exp -> ([], applyLift exp loc)
  in stage funBody

let rec cleanEscBracket = fun attrs ->
  match attrs with
    [] -> []
    | attr::[] -> attrs
    | ({txt = "metaocaml.escape"}, _)::({txt = "metaocaml.bracket"}, _)::attrs -> cleanEscBracket attrs
    | attr::attrs -> attr::(cleanEscBracket attrs)

let cleanMetaFun = fun funBody ->
  let rec clean exp =
    match exp with
      {pexp_desc = Pexp_ifthenelse (condExp, thenExp, elseExpOpt); pexp_attributes = attrs; pexp_loc = loc} ->
          let condExp' = clean condExp in
          let thenExp' = clean thenExp in
          let elseExpOpt' = 
            begin match elseExpOpt with
              None -> None
              | Some elseExp -> Some (clean elseExp)
            end in
          let attrs' = cleanEscBracket attrs in
          Exp.ifthenelse ~loc ~attrs:attrs' condExp' thenExp' elseExpOpt'
      | {pexp_desc = Pexp_apply (fn, argList); pexp_attributes = attrs; pexp_loc = loc} ->
          let argList' = 
            let rec aux args =
              match args with
                [] -> []
                | (lbl, exp)::args -> (lbl, clean exp)::(aux args)
            in aux argList
          in let attrs' = cleanEscBracket attrs in
          Exp.apply ~loc ~attrs:attrs' fn argList'
      | {pexp_desc = desc; pexp_attributes = attrs; pexp_loc = loc} ->
          let attrs' = cleanEscBracket attrs in
          {pexp_desc = desc; pexp_attributes = attrs'; pexp_loc = loc}
  in clean funBody

let buildArgsList args body loc =
  let rec aux args =
    match args with
      [] -> body
      | arg::args -> 
          Exp.fun_ ~loc ~attrs:[] "" None 
            (Pat.var ~loc ~attrs:[] {loc = loc; txt = arg}) 
            (aux args)
  in aux args

let buildLetBoundStagedBody = fun funRec statVars dynVars funBody funName loc ->
  let recFlag = if funRec then Recursive else Nonrecursive in
  let funBody' = applyEsc funBody loc in
  let letBody = 
    Exp.let_ ~loc ~attrs:[] recFlag
      [Vb.mk ~loc ~attrs:[]
         (Pat.var ~loc ~attrs:[] {txt = funName; loc = loc})
         (buildArgsList dynVars funBody' loc)]
      (Exp.ident ~loc ~attrs:[] {txt = Lident funName; loc = loc})
  in applyLift letBody loc
  
let buildMeta = fun funRec funDef vars statVars dynVars -> 
  let strLoc = funDef.pstr_loc in
  let f = 
    match funDef.pstr_desc with
        Pstr_value (_, f) -> f
        | _ -> failwith "not a function definition"
  in let vbLoc = (List.hd f).pvb_loc in
  let funName = 
    match (List.hd f).pvb_pat with 
      {ppat_desc = Ppat_var {txt = fn}} -> fn
      | _ -> failwith "not a valid fun name pattern"
  in let stagedName = Pat.var ~loc:vbLoc ~attrs:[] {loc = vbLoc; txt = getStagedName funName statVars} in
  let funBody = removeArguments (List.hd f).pvb_expr (List.length vars) in
  let (auxs, stagedBody) = buildStagedBody funBody vars statVars dynVars funName vbLoc in
  let cleanedStagedBody = cleanMetaFun stagedBody in
  let letBoundStagedBody = buildLetBoundStagedBody 
                             funRec
                             statVars dynVars cleanedStagedBody funName vbLoc
  in let stagedBodyWithAux =
    let rec attachAuxs auxs =
      match auxs with
        [] -> letBoundStagedBody
        | aux::auxs ->
            let cleanedAux = cleanMetaFun aux in
            Exp.let_ ~loc:vbLoc ~attrs:[] Recursive
               [Vb.mk ~loc:vbLoc ~attrs:[]
                  (Pat.var ~loc:vbLoc ~attrs:[] {loc = vbLoc; txt = "aux"})
                  (buildArgsList vars cleanedAux vbLoc)]
               (attachAuxs auxs)
    in attachAuxs auxs
  in let metaFun = buildArgsList statVars stagedBodyWithAux vbLoc in
  Str.value ~loc:strLoc Nonrecursive [Vb.mk ~loc:vbLoc ~attrs:[] stagedName metaFun]

let toMeta_mapper argv =
  { default_mapper with
    structure = fun mapper structure_item_list ->
      let structure_item_mapper = fun mapper structure_item ->  
        match structure_item with
          {pstr_desc = Pstr_value (_, _)} ->
              if hasToMetaAnnot structure_item 
                then
                  let r = isRecursive structure_item in
                  let vars = getVars structure_item in 
                  let statVarVariants = getStatVars structure_item in
                  let rec aux svvs =
                    match svvs with
                      [] -> []
                      | statVars::svvs ->
                          let dynVars = List.filter (fun v -> not (List.exists (fun sv -> v=sv) statVars)) vars in
                          (buildMeta r structure_item vars statVars dynVars)::(aux svvs)
                  in (default_mapper.structure_item mapper structure_item)::(aux statVarVariants)
                else [default_mapper.structure_item mapper structure_item]
          | _ -> [default_mapper.structure_item mapper structure_item]
      in List.flatten (List.map (structure_item_mapper mapper) structure_item_list)
  }

let () = register "toMeta" toMeta_mapper
