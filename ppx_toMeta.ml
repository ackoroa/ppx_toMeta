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

let buildMeta = fun funRec funDef statVars dynVars mapper -> 
  if funRec
    then
      failwith "not implemented"
    else
      let strLoc = funDef.pstr_loc in
      let f = match funDef.pstr_desc with
                Pstr_value (_,f) -> f
                | _ -> failwith "not a function definition"
      in let vbLoc = (List.hd f).pvb_loc in
      let funName = (List.hd f).pvb_pat in
      let funBody = (List.hd f).pvb_expr in
      Str.value ~loc:strLoc Nonrecursive [Vb.mk ~loc:vbLoc ~attrs:[] funName funBody]

let toMeta_mapper argv =
  { default_mapper with
    structure_item = fun mapper structure_item ->
      if isFunDef structure_item 
        then
          let r = isRecursive structure_item in
          let vars = getVars structure_item in
          let statVars = getStatVars structure_item in
          let dynVars = List.filter (fun v -> not (List.exists (fun sv -> v=sv) statVars)) vars in
          buildMeta r structure_item statVars dynVars mapper
        else
          default_mapper.structure_item mapper structure_item
  }

let () = register "toMeta" toMeta_mapper
