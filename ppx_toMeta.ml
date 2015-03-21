open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let stagedFunList = ref []

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

let isRecursive = fun funDef ->
  match funDef with
    {pstr_desc = Pstr_value (Nonrecursive, _)} -> false
    | {pstr_desc = Pstr_value (Recursive, _)} -> true
    | _ -> failwith "not a function definition"

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
              | _ -> failwith "Syntax error for toMeta annotation. Annotation is of the form [@@static <statVars>] where statVars = [] | sv::statVars"
            end in
          let statVars = aux2 construct.pexp_desc in
          statVars::(aux attrs)
      | ({txt = "static"}, _)::attrs -> failwith "Syntax error for toMeta annotation. Annotation is of the form [@@static <statVars>] where statVars = [] | sv::statVars"
      | _::attrs -> aux attrs
  in aux attrList

let getUsedStagedFun = fun funBody ->
  let rec getUsf funBody =
    match funBody with
      {pexp_desc = Pexp_ifthenelse (cond, thenExp, elseExpOpt)} ->
          begin match elseExpOpt with
            None -> (getUsf cond)@(getUsf thenExp)
            | Some elseExp ->  (getUsf cond)@(getUsf thenExp)@(getUsf elseExp)
          end
      | {pexp_desc = Pexp_match (condExp, pattExpList)} ->
          (getUsf condExp)@(List.flatten (List.map (fun {pc_rhs = rhs} -> getUsf rhs) pattExpList))
      | {pexp_desc = Pexp_apply (fn, argList)} ->
          let fname = 
            begin match fn with 
              {pexp_desc = Pexp_ident {txt = Lident fname}} -> fname
              | _ -> failwith "not a valid function identifier"
            end in
          let usedHere = if List.exists (fun (sfn, _) -> fname=sfn) !stagedFunList then [fname] else [] in
          let usedInArgs = List.flatten (List.map (fun (_,exp) -> getUsf exp) argList) in 
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

let getStagedFunStatVars = fun usfn ->
  let rec aux sfs =
    match sfs with
      [] -> failwith "used staged function doesn't exists?"
      | (sfn, sfsv)::sfs -> if sfn=usfn then sfsv else aux sfs 
  in aux !stagedFunList

let rec removeArguments funBody n =
  if n = 0
    then funBody 
    else 
      match funBody with
        {pexp_desc = Pexp_fun (_, _, _, funBody)} -> removeArguments funBody (n-1)
        | _ -> failwith "arguments missing?"

let rec expContainsVar = fun exp var ->
  match exp with
    {pexp_desc = Pexp_apply (op, es)} ->
        List.exists (fun (_, e) -> expContainsVar e var) es
    | {pexp_desc = Pexp_ident {txt = Lident v}} ->
        v = var
    | _ -> false

let isControlVarStatic = fun statVars actualBody ->
  let rec aux body =
    match body with
      {pexp_desc = Pexp_ifthenelse (condExp, thenExp, elseExpOpt)} ->
          (List.exists (fun v -> expContainsVar condExp v) statVars) 
            || aux thenExp 
            || begin match elseExpOpt with Some e -> aux e | None -> false end
      | {pexp_desc = Pexp_match (condExp, pattExpList)} ->
          (List.exists (fun v -> expContainsVar condExp v) statVars)
            || List.fold_left (||) false (List.map (fun pattExp -> aux pattExp.pc_rhs) pattExpList)
      | {pexp_desc = Pexp_apply (op, es)} ->
          List.exists (fun (_, e) -> aux e) es
      | _ -> false
  in aux actualBody

let subAuxBody = fun funBody funName statVars dynVars loc ->
  let rec sub funBody inEsc =
    match funBody with
      {pexp_desc = Pexp_ifthenelse (condExp, thenExp, elseExpOpt); pexp_attributes = attrs; pexp_loc = loc} ->
          let condExp' = sub condExp inEsc in
          let liftBranch = (List.exists (fun v -> expContainsVar condExp v) statVars) in
          let thenExp' = if liftBranch 
                           then (applyLift (sub thenExp inEsc) loc) 
                           else sub thenExp inEsc
          in let elseExpOpt' = 
            begin match elseExpOpt with
              None -> None
              | Some elseExp -> 
                  if liftBranch 
                    then Some (applyLift (sub elseExp inEsc) loc)
                    else Some (sub elseExp inEsc)
            end in
          Exp.ifthenelse ~loc ~attrs condExp' thenExp' elseExpOpt'
      | {pexp_desc = Pexp_match (condExp, pattExpList); pexp_attributes = attrs; pexp_loc = loc} ->
          let condExp' = sub condExp inEsc in
          let liftBranch = (List.exists (fun v -> expContainsVar condExp v) statVars) in
          Exp.match_ ~loc ~attrs condExp' 
            (List.map 
               (fun {pc_lhs = lhs; pc_guard = guard; pc_rhs = rhs} -> 
                  {pc_lhs = lhs; pc_guard = guard; pc_rhs = (if liftBranch 
                                                               then applyLift (sub rhs inEsc) loc
                                                               else sub rhs inEsc)}) 
               pattExpList)
      | {pexp_desc = Pexp_apply (fn, argList); pexp_attributes = attrs; pexp_loc = loc} ->
          let fname = 
            begin match fn with 
              {pexp_desc = Pexp_ident {txt = Lident fname}} -> fname
              | _ -> failwith "not a valid function identifier"
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
      | {pexp_desc = Pexp_construct (lid, constrExpOpt); pexp_attributes = attrs; pexp_loc = loc} ->
          begin match constrExpOpt with
            None -> Exp.construct ~loc ~attrs lid None
            | Some exp -> Exp.construct ~loc ~attrs lid (Some (sub exp inEsc)) 
          end
      | {pexp_desc = Pexp_tuple es; pexp_attributes = attrs; pexp_loc = loc} ->
          Exp.tuple ~loc ~attrs (List.map (fun e -> sub e inEsc) es)
      | {pexp_desc = Pexp_ident {txt = Lident v; loc = loc}} ->
          if not inEsc && (List.exists (fun dv -> v=dv) dynVars)
            then applyEsc funBody loc
            else funBody
      | exp -> exp
  in sub funBody false

let buildAuxCall = fun vars statVars dynVars loc ->
  let newBodyArgs =
    List.map
      (fun v ->
           let identExp = Exp.ident ~loc ~attrs:[] {loc = loc; txt = Lident v} in
           if List.exists (fun dv -> v=dv) dynVars 
             then ("", applyLift identExp loc)
             else ("", identExp))
      vars
  in let newBody =
    Exp.apply ~loc ~attrs:[] 
      (Exp.ident ~loc ~attrs:[] {loc = loc; txt = Lident "aux"})
      newBodyArgs
  in applyEsc newBody loc

let subUsedStagedFun = fun funBody usedStagedFun ->
  let rec sub funBody =
    match funBody with
      {pexp_desc = Pexp_ifthenelse (cond, thenExp, elseExpOpt); pexp_attributes = attrs; pexp_loc = loc} ->
          begin match elseExpOpt with
            None -> Exp.ifthenelse ~loc ~attrs cond (sub thenExp) None
            | Some elseExp -> Exp.ifthenelse ~loc ~attrs:[] cond (sub thenExp) (Some (sub elseExp))
          end
      | {pexp_desc = Pexp_match (condExp, pattExpList); pexp_attributes = attrs; pexp_loc = loc} ->
          Exp.match_ ~loc ~attrs condExp 
            (List.map 
               (fun {pc_lhs = lhs; pc_guard = guard; pc_rhs = rhs} -> 
                  {pc_lhs = lhs; pc_guard = guard; pc_rhs = sub rhs}) 
               pattExpList)
      | {pexp_desc = Pexp_apply (fn, argList); pexp_attributes = attrs; pexp_loc = loc} ->
          let fname = 
            begin match fn with 
              {pexp_desc = Pexp_ident {txt = Lident fname}} -> fname
              | _ -> failwith "not a valid function identifier"
            end in
          let (isStagedFun, sfsv) =
            let rec aux sfs = 
              match sfs with
                [] -> (false, [])
                | (sfn, sfsv)::sfs -> if fname = sfn then (true, sfsv) else aux sfs
            in aux usedStagedFun in 
          let argList' = 
            if isStagedFun
              then let rec aux args =
                     match args with
                       [] -> []
                       | (lbl, exp)::args ->
                           match exp with
                             {pexp_desc = Pexp_ident {txt = Lident v}} ->
                               if List.exists (fun sv -> sv=v) sfsv
                                 then aux args
                                 else (lbl, exp)::(aux args)
                       | exp -> (lbl, sub exp)::(aux args)
                   in aux argList
              else List.map (fun (lbl, exp) -> (lbl, sub exp)) argList
          in Exp.apply ~loc ~attrs fn argList'
      | {pexp_desc = Pexp_construct (lid, constrExpOpt); pexp_attributes = attrs; pexp_loc = loc} ->
          begin match constrExpOpt with
            None -> Exp.construct ~loc ~attrs lid None
            | Some exp -> Exp.construct ~loc ~attrs lid (Some (sub exp)) 
          end
      | {pexp_desc = Pexp_tuple es; pexp_attributes = attrs; pexp_loc = loc} ->
          Exp.tuple ~loc ~attrs (List.map (fun e -> sub e) es)
      | exp -> exp
  in sub funBody

let subRecCall = fun funBody funName statVars ->
  let rec sub funBody =
    match funBody with
      {pexp_desc = Pexp_ifthenelse (cond, thenExp, elseExpOpt); pexp_attributes = attrs; pexp_loc = loc} ->
          begin match elseExpOpt with
            None -> Exp.ifthenelse ~loc ~attrs cond (sub thenExp) None
            | Some elseExp -> Exp.ifthenelse ~loc ~attrs:[] cond (sub thenExp) (Some (sub elseExp))
          end
      | {pexp_desc = Pexp_match (condExp, pattExpList); pexp_attributes = attrs; pexp_loc = loc} ->
          Exp.match_ ~loc ~attrs condExp 
            (List.map 
               (fun {pc_lhs = lhs; pc_guard = guard; pc_rhs = rhs} -> 
                  {pc_lhs = lhs; pc_guard = guard; pc_rhs = sub rhs}) 
               pattExpList)
      | {pexp_desc = Pexp_apply (fn, argList); pexp_attributes = attrs; pexp_loc = loc} ->
          let fname = 
            begin match fn with 
              {pexp_desc = Pexp_ident {txt = Lident fname}} -> fname
              | _ -> failwith "not a valid function identifier"
            end in
          let argList' = 
            let rec aux args =
              match args with
                [] -> []
                | (lbl, exp)::args ->
                    match exp with
                      {pexp_desc = Pexp_ident {txt = Lident v}} ->
                          if fname = funName && List.exists (fun sv -> sv=v) statVars
                            then aux args
                            else (lbl, exp)::(aux args)
                      | exp -> (lbl, sub exp)::(aux args)
            in aux argList
          in Exp.apply ~loc ~attrs fn argList'
      | {pexp_desc = Pexp_construct (lid, constrExpOpt); pexp_attributes = attrs; pexp_loc = loc} ->
          begin match constrExpOpt with
            None -> Exp.construct ~loc ~attrs lid None
            | Some exp -> Exp.construct ~loc ~attrs lid (Some (sub exp)) 
          end
      | {pexp_desc = Pexp_tuple es; pexp_attributes = attrs; pexp_loc = loc} ->
          Exp.tuple ~loc ~attrs (List.map (fun e -> sub e) es)
      | exp -> exp
  in sub funBody

let buildArgList args body loc =
  let rec aux args =
    match args with
      [] -> body
      | arg::args -> 
          Exp.fun_ ~loc ~attrs:[] "" None 
            (Pat.var ~loc ~attrs:[] {loc = loc; txt = arg}) 
            (aux args)
  in aux args

let buildStagedBody = fun funRec statVars dynVars funBody funName loc ->
  let recFlag = if funRec then Recursive else Nonrecursive in
  let letBody = 
    Exp.let_ ~loc ~attrs:[] recFlag
      [Vb.mk ~loc ~attrs:[]
         (Pat.var ~loc ~attrs:[] {loc = loc; txt = funName})
         (buildArgList dynVars funBody loc)]
      (Exp.ident ~loc ~attrs:[] {loc=loc;txt=Lident funName})
  in applyLift letBody loc
  
let getStagedBody = fun funBody loc vars statVars dynVars funRec funName usedStagedFun ->
  let useAux = isControlVarStatic statVars funBody in
  let toStageBody =
    if funRec
      then if useAux
             then buildAuxCall vars statVars dynVars loc
             else subRecCall funBody funName statVars
      else funBody
  in let toStageBody' =
    if (List.length usedStagedFun) > 0 
      then subUsedStagedFun toStageBody usedStagedFun 
      else toStageBody
  in let stagedBody = buildStagedBody (funRec && (not useAux)) 
                        statVars dynVars toStageBody' funName loc
  in let fullBody = 
    if useAux
      then let auxBody = subAuxBody funBody funName statVars dynVars loc in
           let auxBody' =
             if (List.length usedStagedFun) > 0 
               then subUsedStagedFun auxBody usedStagedFun 
               else auxBody
           in Exp.let_ ~loc ~attrs:[] Recursive
             [Vb.mk ~loc ~attrs:[]
                (Pat.var ~loc ~attrs:[] {loc = loc; txt = "aux"})
                (buildArgList vars auxBody' loc)]
           stagedBody
      else stagedBody
  in let fullBodyWithDecl = 
    let rec aux usfs = 
      match usfs with
        [] -> fullBody
        | (usfn, usfsv)::usfs ->
            let declBody = 
              applyRun (applyLift (applyEsc (applyFun 
                (List.map (fun v -> Exp.ident ~loc ~attrs:[] {txt = Lident v; loc = loc}) usfsv)
                (usfn^"S") loc) loc) loc) loc
            in Exp.let_ ~loc ~attrs:[] Nonrecursive
                 [Vb.mk ~loc ~attrs:[]
                   (Pat.var ~loc ~attrs:[] {loc = loc; txt = usfn})
                   declBody]
                 (aux usfs)
    in aux usedStagedFun
  in buildArgList statVars fullBodyWithDecl loc

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
  in let stagedName = Pat.var ~loc:vbLoc ~attrs:[] {loc = vbLoc; txt = (funName^"S")} in
  let funBody = removeArguments (List.hd f).pvb_expr (List.length vars) in
  let usedStagedFun = getUsedStagedFun funBody in
  let usedStagedFunWithVars = List.map (fun sfn -> (sfn, getStagedFunStatVars sfn)) usedStagedFun in
  let stagedBody = getStagedBody funBody vbLoc vars statVars dynVars funRec funName usedStagedFunWithVars in
  stagedFunList := (funName, statVars)::(!stagedFunList);
  Str.value ~loc:strLoc Nonrecursive [Vb.mk ~loc:vbLoc ~attrs:[] stagedName stagedBody]

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
                  in aux statVarVariants
                else [default_mapper.structure_item mapper structure_item]
          | _ -> [default_mapper.structure_item mapper structure_item]
      in List.flatten (List.map (structure_item_mapper mapper) structure_item_list)
  }

let () = register "toMeta" toMeta_mapper
