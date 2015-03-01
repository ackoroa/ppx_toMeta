open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

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

let getStatVars = fun funDef ->
  let attrList = getAttrList funDef in
  let rec aux attrs =
    match attrs with
      [] -> []
      | ({txt = var}, _)::attrs -> var::(aux attrs)
  in aux attrList

let rec removeArguments funBody n =
  if n=0 
    then funBody 
    else 
      match funBody with
        {pexp_desc = Pexp_fun (_, _, _, funBody)} -> removeArguments funBody (n-1)
        | _ -> failwith "arguments missing?"

let subRecCall = fun funBody funName statVars ->
  let rec sub funBody =
    match funBody with
      {pexp_desc = Pexp_ifthenelse (cond, thenExp, elseExpOpt); pexp_loc = loc} ->
          begin match elseExpOpt with
            None -> Exp.ifthenelse ~loc ~attrs:[] cond (sub thenExp) None
            | Some elseExp -> Exp.ifthenelse ~loc ~attrs:[] cond (sub thenExp) (Some (sub elseExp))
          end
      | {pexp_desc = Pexp_apply (fn, argList); pexp_loc = loc} ->
          let fname = 
            begin match fn with 
              {pexp_desc = Pexp_ident {txt = Lident fname}} -> fname
              | _ -> failwith "not a valid function identifier"
            end in
          if fname = funName
            then 
              let fn' = Exp.ident ~loc ~attrs:[] {loc = loc; txt = Lident "f"} in
              let argList' = 
                let rec aux args =
                  match args with
                    [] -> []
                    | (lbl, exp)::args -> 
                        match exp with
                          {pexp_desc = Pexp_ident {txt = Lident v}} ->
                              if List.exists (fun sv -> sv=v) statVars
                                then aux args
                                else (lbl, exp)::(aux args)
                          | exp -> (lbl, sub exp)::(aux args)
                in aux argList
              in Exp.apply ~loc ~attrs:[] fn' argList'
            else Exp.apply ~loc ~attrs:[] fn argList
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

let buildStagedBody = fun statVars dynVars actualBody loc ->
  let letBody = 
    Exp.let_ ~loc ~attrs:[] Nonrecursive 
      [Vb.mk ~loc ~attrs:[]
         (Pat.var ~loc ~attrs:[] {loc=loc;txt="f"})
         (buildArgList dynVars actualBody loc)]
      (Exp.ident ~loc ~attrs:[] {loc=loc;txt=Lident "f"})
  in let liftedLetBody =
    Exp.apply ~loc ~attrs:[] 
      (Exp.ident ~loc ~attrs:[] {loc=loc;txt=Lident "lift"})
      [("", letBody)]
  in buildArgList statVars liftedLetBody loc

let getStagedRecBody = fun origBody vbLoc vars statVars dynVars funName ->
  let nVars = List.length vars in
  let actualBody = removeArguments origBody nVars in
  let actualBody' = subRecCall actualBody funName statVars in
  buildStagedBody statVars dynVars actualBody' vbLoc

let getStagedBody = fun origBody vbLoc vars statVars dynVars ->
  let nVars = List.length vars in
  let actualBody = removeArguments origBody nVars in
  buildStagedBody statVars dynVars actualBody vbLoc

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
  let funBody = (List.hd f).pvb_expr in
  if funRec
    then
      let stagedBody = getStagedRecBody funBody vbLoc vars statVars dynVars funName in
      Str.value ~loc:strLoc Recursive [Vb.mk ~loc:vbLoc ~attrs:[] stagedName stagedBody]
    else
      let stagedBody = getStagedBody funBody vbLoc vars statVars dynVars in
      Str.value ~loc:strLoc Nonrecursive [Vb.mk ~loc:vbLoc ~attrs:[] stagedName stagedBody]

let toMeta_mapper argv =
  { default_mapper with
    structure_item = fun mapper structure_item ->
      match structure_item with
        {pstr_desc = Pstr_value (_, _)} ->
          let r = isRecursive structure_item in
          let vars = getVars structure_item in
          let statVars = getStatVars structure_item in
          let dynVars = List.filter (fun v -> not (List.exists (fun sv -> v=sv) statVars)) vars in
          buildMeta r structure_item vars statVars dynVars
        | _ -> default_mapper.structure_item mapper structure_item
  }

let () = register "toMeta" toMeta_mapper
