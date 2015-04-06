open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let rec pil il =
  match il with 
    [] -> print_newline ()
    | i::il -> print_int i; print_string " "; pil il

let attachOrigFun = true

let msg_syntaxErrorDecl = "Syntax error for toMeta annotation. Annotation is of the form [@@static <statVars>] where statVars = [] | sv::statVars"
let msg_syntaxErrorUse = "Syntax error for toMeta annotation. Annotation is of the form [@static,use <statVars>] where statVars = [] | sv::statVars"

let stagedFun = ref []

let idx = ref 0
let fresh = fun v ->
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
    
let isStaticExp = fun exp statVars ->
  let rec aux exp = 
    match exp with
      {pexp_desc = Pexp_ifthenelse (condExp, thenExp, elseExpOpt); pexp_attributes = attrs; pexp_loc = loc} ->
          let elseStat =
            begin match elseExpOpt with
              None -> true
              | Some exp -> aux exp
            end in
          (aux condExp) && (aux thenExp) && elseStat
      | {pexp_desc = Pexp_match (condExp, pattExpList); pexp_attributes = attrs; pexp_loc = loc} ->
          let branchesStat = List.fold_left (fun acc {pc_rhs = rhs} -> acc && (aux rhs)) true pattExpList in
          (aux condExp) && branchesStat
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

let extractToMetaPayload = fun payload ->
  match payload with
    PStr [{pstr_desc = Pstr_eval (construct, _)}] ->
        let rec aux construct =
          begin match construct with
            | Pexp_construct ({txt = Lident "[]"}, None) -> []
            | Pexp_construct ({txt = Lident "::"}, 
                                Some {pexp_desc = 
                                        Pexp_tuple [{pexp_desc = Pexp_ident {txt = Lident v}}; 
                                      construct]}) ->
                v::(aux construct.pexp_desc)
            | _ -> failwith msg_syntaxErrorDecl
          end in
        aux construct.pexp_desc
    | _ -> failwith msg_syntaxErrorDecl

let getStatVars = fun funDef ->
  let attrList = getAttrList funDef in
  let rec aux attrs =
    match attrs with
      [] -> []
      | ({txt = "static"}, payload)::attrs ->
          let statVars = extractToMetaPayload payload in
          statVars::(aux attrs)
      | _::attrs -> aux attrs
  in aux attrList

let getStatVarIdxs = fun vars statVars ->
  let idxs = List.mapi (fun idx v -> 
                          if List.exists (fun sv -> v=sv) statVars 
                            then idx 
                            else -1) 
                       vars
  in List.filter (fun i -> i >= 0) idxs

let getUsedStagedFun = fun funBody statVars ->
  let rec getUsf funBody =
    match funBody with
      {pexp_desc = Pexp_ifthenelse (cond, thenExp, elseExpOpt)} ->
          begin match elseExpOpt with
            None -> (getUsf cond)@(getUsf thenExp)
            | Some elseExp ->  (getUsf cond)@(getUsf thenExp)@(getUsf elseExp)
          end
      | {pexp_desc = Pexp_match (condExp, pattExpList)} ->
          (getUsf condExp)@(List.flatten (List.map (fun {pc_rhs = rhs} -> getUsf rhs) pattExpList))
      | {pexp_desc = Pexp_apply (fn, argList); pexp_attributes = attrs} ->
          let fname = 
            begin match fn with 
              {pexp_desc = Pexp_ident {txt = Lident fname}} -> fname
              | _ -> "_AnonFun"
            end in
          let usedHere =
            let rec aux attrs = 
              match attrs with
                [] -> []
                | ({txt = "static.use"}, payload)::_ -> 
                    let isArgStatic = 
                      List.map 
                        (fun (_, exp) -> (isStaticExp exp statVars, exp)) 
                        argList
                    in let statArgIdxExps = 
                      List.filter 
                        (fun (i, _) -> i >= 0) 
                        (List.mapi 
                           (fun idx (isStat, arg) -> if isStat then (idx, arg) else (-1, arg))
                           isArgStatic)
                    in [(fname, statArgIdxExps)]
                | _::attrs -> aux attrs
            in aux attrs
          in let usedInArgs = List.flatten (List.map (fun (_, exp) -> getUsf exp) argList) in
          usedHere@usedInArgs
      | {pexp_desc = Pexp_construct (lid, constrExpOpt)} ->
          begin match constrExpOpt with
            None -> []
            | Some exp -> getUsf exp 
          end
      | {pexp_desc = Pexp_tuple es} ->
          List.flatten (List.map (fun e -> getUsf e) es)
      | exp -> []
  in getUsf funBody

let rec removeArguments = fun funBody n ->
  if n = 0
    then funBody 
    else 
      match funBody with
        {pexp_desc = Pexp_fun (_, _, _, funBody)} -> removeArguments funBody (n-1)
        | _ -> failwith "arguments missing?"

let recCallExists = fun funName funBody ->
  let rec aux exp =
    match exp with
      {pexp_desc = Pexp_ifthenelse (condExp, thenExp, elseExpOpt)} ->
          let existInElse = 
            begin match elseExpOpt with
              None -> false
              | Some elseExp -> aux elseExp
            end in
          (aux condExp) || (aux thenExp) || existInElse
      | {pexp_desc = Pexp_match (condExp, pattExpList)} ->
          let existInBranches = List.fold_left (fun acc {pc_rhs = rhs} -> aux rhs) false pattExpList in
          (aux condExp) || existInBranches
      | {pexp_desc = Pexp_apply (fn, argList)} ->
          let fname = 
            begin match fn with 
              {pexp_desc = Pexp_ident {txt = Lident fname}} -> fname
              | _ -> "_AnonFun"
            end in
          let existInArgs = 
            let rec aux1 args =
              match args with
                [] -> false
                | (lbl, exp)::args -> (aux exp) || (aux1 args)
            in aux1 argList
          in existInArgs || (fname = funName)
      | {pexp_desc = Pexp_construct (lid, constrExpOpt)} ->
          begin match constrExpOpt with
            None -> false
            | Some exp -> aux exp
          end
      | {pexp_desc = Pexp_tuple es} ->
          List.fold_left (fun acc e -> acc || (aux e)) false es
      | exp -> false
  in aux funBody

let containsDynArg = fun exp dynVars ->
  let rec aux exp =
    match exp with
      {pexp_desc = Pexp_ifthenelse (condExp, thenExp, elseExpOpt)} ->
          let existInElse = 
            begin match elseExpOpt with
              None -> false
              | Some elseExp -> aux elseExp
            end in
          (aux condExp) || (aux thenExp) || existInElse
      | {pexp_desc = Pexp_match (condExp, pattExpList)} ->
          let existInBranches = List.fold_left (fun acc {pc_rhs = rhs} -> aux rhs) false pattExpList in
          (aux condExp) || existInBranches
      | {pexp_desc = Pexp_apply (fn, argList)} ->
          let rec aux1 args =
            begin match args with
              [] -> false
              | (lbl, exp)::args -> (aux exp) || (aux1 args)
            end in
          aux1 argList
      | {pexp_desc = Pexp_construct (lid, constrExpOpt)} ->
          begin match constrExpOpt with
            None -> false
            | Some exp -> aux exp
          end
      | {pexp_desc = Pexp_tuple es} ->
          List.fold_left (fun acc e -> acc || (aux e)) false es
      | {pexp_desc = Pexp_ident {txt = Lident v}} ->
          List.exists (fun dv -> v=dv) dynVars
      | exp -> false
  in aux exp

let rec cleanAttrs = fun attrs ->
  match attrs with
    [] -> []
    | ({txt = "static.use"}, _)::attrs -> cleanAttrs attrs
    | ({txt = "metaocaml.escape"}, _)::({txt = "metaocaml.bracket"}, _)::attrs -> cleanAttrs attrs
    | attr::attrs -> attr::(cleanAttrs attrs)

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
          let attrs' = cleanAttrs attrs in
          Exp.ifthenelse ~loc ~attrs:attrs' condExp' thenExp' elseExpOpt'
      | {pexp_desc = Pexp_match (condExp, pattExpList); pexp_attributes = attrs; pexp_loc = loc} ->
          let condExp' = clean condExp in
          let pattExpList' =
            (List.map 
               (fun {pc_lhs = lhs; pc_guard = guard; pc_rhs = rhs} ->
                  {pc_lhs = lhs; pc_guard = guard; pc_rhs = clean rhs}) 
               pattExpList)
          in let attrs' = cleanAttrs attrs in
          Exp.match_ ~loc ~attrs:attrs' condExp' pattExpList'
      | {pexp_desc = Pexp_apply (fn, argList); pexp_attributes = attrs; pexp_loc = loc} ->
          let argList' = 
            let rec aux args =
              match args with
                [] -> []
                | (lbl, exp)::args -> (lbl, clean exp)::(aux args)
            in aux argList
          in let attrs' = cleanAttrs attrs in
          Exp.apply ~loc ~attrs:attrs' fn argList'
      | {pexp_desc = Pexp_construct (lid, constrExpOpt); pexp_attributes = attrs; pexp_loc = loc} ->
          let attrs' = cleanAttrs attrs in
          begin match constrExpOpt with
            None -> Exp.construct ~loc ~attrs:attrs' lid None
            | Some exp -> Exp.construct ~loc ~attrs:attrs' lid (Some (clean exp))
          end
      | {pexp_desc = Pexp_tuple es; pexp_attributes = attrs; pexp_loc = loc} ->
          let attrs' = cleanAttrs attrs in
          let es' = List.map (fun e -> clean e) es in
          Exp.tuple ~loc ~attrs:attrs' es'
      | {pexp_desc = desc; pexp_attributes = attrs; pexp_loc = loc} ->
          let attrs' = cleanAttrs attrs in
          {pexp_desc = desc; pexp_attributes = attrs'; pexp_loc = loc}
  in clean funBody

let buildAuxCall = fun auxName vars dynVars loc ->
  let vars' =
    List.map
      (fun v ->
         let identExp = Exp.ident ~loc ~attrs:[] {loc = loc; txt = Lident v} in
         if List.exists (fun dv -> v=dv) dynVars 
           then ("", applyLift identExp loc)
           else ("", identExp))
      vars
  in Exp.apply ~loc ~attrs:[] 
       (Exp.ident ~loc ~attrs:[] {txt = Lident auxName; loc = loc})
       vars'

let rec buildAuxBody = fun funBody vars statVars dynVars usedStagedFun funName auxName firstAuxName loc ->
  match funBody with
    {pexp_desc = Pexp_ifthenelse (condExp, thenExp, elseExpOpt); pexp_attributes = attrs; pexp_loc = loc} ->
        let (thenAux, thenExp') = buildStagedBody thenExp vars statVars dynVars 
                                    usedStagedFun funName true firstAuxName loc
        in let (elseAux, elseExpOpt') = 
          begin match elseExpOpt with
            None -> ([], None)
            | Some elseExp -> 
                let (aux, e) = buildStagedBody elseExp vars statVars dynVars 
                                 usedStagedFun funName true firstAuxName loc
                in (aux, Some e)
          end in
        let body = Exp.ifthenelse ~loc ~attrs condExp thenExp' elseExpOpt' in
        (thenAux@elseAux, body)
    | {pexp_desc = Pexp_match (condExp, pattExpList); pexp_attributes = attrs; pexp_loc = loc} ->
        let pattExpList' =
          (List.map 
             (fun {pc_lhs = lhs; pc_guard = guard; pc_rhs = rhs} ->
                let (aux, rhs') = buildStagedBody rhs vars statVars dynVars 
                                    usedStagedFun funName true firstAuxName loc 
                in (aux, {pc_lhs = lhs; pc_guard = guard; pc_rhs = rhs'})) 
             pattExpList)
        in let branchAuxs = List.flatten (List.map (fun (aux, branch) -> aux) pattExpList') in
        let branches = List.map (fun (aux, branch) -> branch) pattExpList' in
        let body = Exp.match_ ~loc ~attrs condExp branches in
        (branchAuxs, body)
    | exp -> ([], exp)

and buildStagedBody = fun funBody vars statVars dynVars usedStagedFun funName inAux firstAuxName loc ->
  let rec stage exp =
    match exp with
      {pexp_desc = Pexp_ifthenelse (condExp, thenExp, elseExpOpt); pexp_attributes = attrs; pexp_loc = loc} ->
          if isStaticExp condExp statVars
            then
              let auxName = fresh "aux" in
              let firstAuxName = if firstAuxName = "" then auxName else firstAuxName in
              let (auxAux, auxBody) = buildAuxBody exp vars statVars dynVars 
                                        usedStagedFun funName auxName firstAuxName loc
              in let auxCall = buildAuxCall auxName vars dynVars loc in
              (auxAux@[(auxName, auxBody)], auxCall)
            else
              let (condAux, condExp') = stage condExp in
              let (thenAux, thenExp') = stage thenExp in
              let (elseAux, elseExpOpt') =
                begin match elseExpOpt with
                  None -> ([], None)
                  | Some elseExp -> 
                      let (aux, e) = stage elseExp in 
                      (aux, Some (applyEsc e loc))
                end in
              let body = Exp.ifthenelse ~loc ~attrs (applyEsc condExp' loc) 
                           (applyEsc thenExp' loc) elseExpOpt'
              in (condAux@thenAux@elseAux, applyLift body loc)
      | {pexp_desc = Pexp_match (condExp, pattExpList); pexp_attributes = attrs; pexp_loc = loc} ->
          if isStaticExp condExp statVars
            then
              let auxName = fresh "aux" in
              let firstAuxName = if firstAuxName = "" then auxName else firstAuxName in
              let (auxAux, auxBody) = buildAuxBody exp vars statVars dynVars 
                                        usedStagedFun funName auxName firstAuxName loc 
              in let auxCall = buildAuxCall auxName vars dynVars loc in
              (auxAux@[(auxName, auxBody)], auxCall)
            else
              let (condAux, condExp') = 
                let (aux, e) = stage condExp in 
                (aux, applyEsc e loc)
              in let pattExpList' =
                (List.map 
                   (fun {pc_lhs = lhs; pc_guard = guard; pc_rhs = rhs} ->
                     let (aux, rhs') = stage rhs in 
                     (aux, {pc_lhs = lhs; pc_guard = guard; pc_rhs = applyEsc rhs' loc})) 
                   pattExpList)
              in let branchAuxs = List.flatten (List.map (fun (aux, branch) -> aux) pattExpList') in
              let branches = List.map (fun (aux, branch) -> branch) pattExpList' in
              let body = Exp.match_ ~loc ~attrs condExp' branches in
              (condAux@branchAuxs, applyLift body loc)
      | {pexp_desc = Pexp_apply (fn, argList); pexp_attributes = attrs; pexp_loc = loc} ->
          let fname = 
            begin match fn with 
              {pexp_desc = Pexp_ident {txt = Lident fname}} -> fname
              | _ -> "__AnonFun__"
            end in
          let fn' = 
            if inAux && (fname = funName)
              then Exp.ident ~loc ~attrs:[] {loc = loc; txt = Lident firstAuxName}
              else fn
          in let isStagedFun = List.exists 
                                 (fun ({txt = id}, _) -> id = "static.use") 
                                 attrs 
          in let aux_stagedArgs = 
            if isStagedFun 
              then
                let usvidxs =
                  try
                    let svidxs = List.filter (fun i -> i >= 0)
                                   (List.mapi
                                      (fun idx (_, exp) -> 
                                         if isStaticExp exp statVars 
                                           then idx
                                           else -1) 
                                      argList)
                    in let _ = List.find
                                 (fun (fn, sfn, idxs) -> 
                                    (fn = fname) && (idxs = svidxs)) 
                                 !stagedFun
                    in svidxs
                  with Not_found -> []
                in let rec aux args n =
                  begin match args with
                    [] -> []
                    | (lbl, exp)::args ->
                        if List.exists (fun idx -> n = idx) usvidxs
                          then aux args (n + 1)
                          else (lbl, stage exp)::(aux args (n + 1))
                  end in
                aux argList 0
              else
                let rec aux args =
                  begin match args with
                    [] -> []
                    | (lbl, exp)::args -> 
                        begin match exp with
                          {pexp_desc = Pexp_ident {txt = Lident v}} ->
                              if inAux
                                then (lbl, stage exp)::(aux args)
                                else
                                  if (fname = funName) && List.exists (fun sv -> sv=v) statVars
                                    then aux args
                                    else (lbl, stage exp)::(aux args)
                          | exp -> (lbl, stage exp)::(aux args)
                        end
                  end in
                aux argList
          in let auxs = List.flatten (List.map (fun (lbl, (aux, arg)) -> aux) aux_stagedArgs) in
          let args = 
            List.map 
              (fun (lbl, (aux, arg)) ->
                 if inAux && (fname = funName) && (containsDynArg arg dynVars) 
                   then (lbl, arg)
                   else (lbl, applyEsc arg loc)) 
              aux_stagedArgs
          in let e = Exp.apply ~loc ~attrs fn' args in
          if inAux && (fname = funName)
            then (auxs, e)
            else (auxs, applyLift e loc)
      | {pexp_desc = Pexp_construct (lid, constrExpOpt); pexp_attributes = attrs; pexp_loc = loc} ->
          let (aux, body) =
            begin match constrExpOpt with
              None -> ([], Exp.construct ~loc ~attrs lid None)
              | Some exp -> 
                  let (aux, e) = stage exp in
                  (aux, Exp.construct ~loc ~attrs lid (Some (applyEsc e loc)))
            end in
          (aux, applyLift body loc)
      | {pexp_desc = Pexp_tuple es; pexp_attributes = attrs; pexp_loc = loc} ->
          let aux_es = List.map (fun e -> stage e) es in
          let auxs = List.flatten (List.map (fun (aux, e) -> aux) aux_es) in
          let es' = List.map (fun (aux, e) -> applyEsc e loc) aux_es in
          let body = Exp.tuple ~loc ~attrs es' in
          (auxs, applyLift body loc) 
      | {pexp_desc = Pexp_ident {txt = Lident v}; pexp_attributes = attrs; pexp_loc = loc} ->
          if inAux && (List.exists (fun dv -> v=dv) dynVars)
            then ([], exp)
            else ([], applyLift exp loc)
      | exp -> ([], applyLift exp loc)
  in stage funBody

let buildArgsList = fun args body loc ->
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

let attachAuxs = fun auxs vars mainBody loc ->
  if List.length auxs > 0
    then
      let rec attach auxs =
        match auxs with
          [] -> []
          | (auxName, auxBody)::auxs ->
              let cleanedAux = cleanMetaFun auxBody in
              (Vb.mk ~loc ~attrs:[]
                 (Pat.var ~loc ~attrs:[] {txt = auxName; loc = loc})
                 (buildArgsList vars cleanedAux loc))
              ::(attach auxs)
      in let valueBindings = attach auxs in
      Exp.let_ ~loc ~attrs:[] Recursive valueBindings mainBody
    else mainBody          

let attachDecls = fun usedStagedFun mainBody loc ->
  let rec attach usfs =
    match usfs with
      [] -> mainBody
      | (ufn, ufsvidxs_ufargs)::usfs ->
        let ufsvidxs = List.map (fun (idx, arg) -> idx) ufsvidxs_ufargs in 
        try 
          let (fn, sfn, svidxs) = 
            List.find 
              (fun (fn, _, svidxs) -> (fn=ufn) && (svidxs=ufsvidxs)) 
              !stagedFun
          in let declBody = 
            applyRun (applyLift (applyEsc (applyFun 
              (List.map 
                 (fun idx -> 
                    let (i,a) = 
                      List.find 
                        (fun (i,a) -> i=idx) 
                        ufsvidxs_ufargs
                    in a)
                 svidxs)
              sfn loc) loc) loc) loc
          in Exp.let_ ~loc ~attrs:[] Nonrecursive
               [Vb.mk ~loc ~attrs:[]
                  (Pat.var ~loc ~attrs:[] {txt = fn; loc = loc})
                  declBody]
               (attach usfs)
        with Not_found -> attach usfs 
  in attach usedStagedFun

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
  let usedStagedFun = getUsedStagedFun funBody statVars in
  let (auxs, stagedBody) = buildStagedBody funBody vars statVars dynVars usedStagedFun funName false "" vbLoc in
  let cleanedStagedBody = cleanMetaFun stagedBody in
  let letBoundStagedBody = buildLetBoundStagedBody 
                             (funRec && (recCallExists funName cleanedStagedBody))
                             statVars dynVars cleanedStagedBody funName vbLoc
  in let stagedBodyWithAux = attachAuxs auxs vars letBoundStagedBody vbLoc in
  let stagedBodyWithDecl = attachDecls usedStagedFun stagedBodyWithAux vbLoc in
  let metaFun = buildArgsList statVars stagedBodyWithDecl vbLoc in
  stagedFun := (funName, getStagedName funName statVars, getStatVarIdxs vars statVars)::(!stagedFun);
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
                  in if attachOrigFun
                       then (default_mapper.structure_item mapper structure_item)::(aux statVarVariants)
                       else aux statVarVariants
                else [default_mapper.structure_item mapper structure_item]
          | _ -> [default_mapper.structure_item mapper structure_item]
      in List.flatten (List.map (structure_item_mapper mapper) structure_item_list)
  }

let () = register "toMeta" toMeta_mapper
