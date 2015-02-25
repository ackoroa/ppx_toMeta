open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let isFunDecl = fun structure_item ->
  match structure_item with
    {pstr_desc = Pstr_value (_,_)} -> true
    | _ -> false

let isRecursive = fun funDef ->
  match funDef with
    {pstr_desc = Pstr_value (Nonrecursive, _)} -> false
    | {pstr_desc = Pstr_value (Recursive, _)} -> true
    | _ -> failwith "not a function definition"

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

let rec printVars vs =
  match vs with
    [] -> ()
    | v::vs -> print_string v; printVars vs

let toMeta_mapper argv =
  { default_mapper with
    structure_item = fun mapper structure_item ->
      if isFunDecl structure_item 
        then
          let r = isRecursive structure_item in
          let statVars = getStatVars structure_item in
          printVars statVars;
          default_mapper.structure_item mapper structure_item
        else
          default_mapper.structure_item mapper structure_item
  }

let () = register "toMeta" toMeta_mapper
