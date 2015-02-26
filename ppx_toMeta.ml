open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let isFunDef = fun structure_item ->
  match structure_item with
    {pstr_desc = Pstr_value (_,_)} -> true
    | _ -> false

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

let getAtrList = fun funDef ->
  match funDef with
    {pstr_desc = Pstr_value (_, value_binding_list)} ->
      (* value binding is a list for let .. and .. and .. *)
      (* TODO: handle ands *)
      (List.hd value_binding_list).pvb_attributes
    | _ -> failwith "not a function definition"

let rec extractStatVars = fun atrList ->
  match atrList with
    [] -> []
    | ({txt = var}, _)::atrList -> var::(extractStatVars atrList)

let getStatVars = fun funDef ->
  let atrList = getAtrList funDef in
  extractStatVars atrList

let getStagedName = fun origName vbLoc ->
  match origName.ppat_desc with 
    Ppat_var {loc = nameStrLoc; txt = origNameStr} ->
        let nameStr = origNameStr^"S" in
        Pat.var ~loc:vbLoc ~attrs:[] {loc=nameStrLoc;txt=nameStr}
    | _ -> failwith "not a valid function name pattern"

let buildArgList args body loc =
  let rec aux args =
    match args with
      [] -> body
      | arg::args -> 
            Exp.fun_ ~loc ~attrs:[] "" None 
              (Pat.var ~loc ~attrs:[] {loc=loc;txt=arg}) 
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

let rec removeArguments funBody n =
  if n=0 
    then funBody 
    else 
      match funBody with
        {pexp_desc = Pexp_fun (_, _, _, funBody)} -> removeArguments funBody (n-1)
        | _ -> failwith "arguments missing?"

let subRecCall = fun funBody funName statVars ->
  (*let rec aux funBody =
    match funBody with
      {pexp_desc = Pexp_fun (lbl, opt, pat, funBody)} -> 
          {pexp_desc = Pexp_fun (lbl, opt, pat, aux funBody)}
      | _ -> failwith "not implemented"
  in *)failwith "not implemented"

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
  let f = match funDef.pstr_desc with
            Pstr_value (_,f) -> f
            | _ -> failwith "not a function definition"
  in let vbLoc = (List.hd f).pvb_loc in
  let funName = (List.hd f).pvb_pat in
  let stagedName = getStagedName funName vbLoc in
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
      if isFunDef structure_item 
        then
          let r = isRecursive structure_item in
          let vars = getVars structure_item in
          let statVars = getStatVars structure_item in
          let dynVars = List.filter (fun v -> not (List.exists (fun sv -> v=sv) statVars)) vars in
          buildMeta r structure_item vars statVars dynVars
        else
          default_mapper.structure_item mapper structure_item
  }

let () = register "toMeta" toMeta_mapper
