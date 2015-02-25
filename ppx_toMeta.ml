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

(*
let rec pow x n = if n=0 then 1 else x * pow x (n-1) [@@n]
==>
[{pstr_desc =
   Pstr_value (Recursive,
    [{pvb_pat = {ppat_desc = Ppat_var {txt = "pow"}};
      pvb_expr =
       {pexp_desc =
         Pexp_fun ("", None, {ppat_desc = Ppat_var {txt = "x"}},
          {pexp_desc =
            Pexp_fun ("", None, {ppat_desc = Ppat_var {txt = "n"}},
             {pexp_desc =
               Pexp_ifthenelse
                ({pexp_desc =
                   Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "="}},
                    [("", {pexp_desc = Pexp_ident {txt = Lident "n"}});
                     ("", {pexp_desc = Pexp_constant (Const_int 0)})])},
                {pexp_desc = Pexp_constant (Const_int 1)},
                Some
                 {pexp_desc =
                   Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "*"}},
                    [("", {pexp_desc = Pexp_ident {txt = Lident "x"}});
                     ("",
                      {pexp_desc =
                        Pexp_apply
                         ({pexp_desc = Pexp_ident {txt = Lident "pow"}},
                         [("", {pexp_desc = Pexp_ident {txt = Lident "x"}});
                          ("",
                           {pexp_desc =
                             Pexp_apply
                              ({pexp_desc = Pexp_ident {txt = Lident "-"}},
                              [("",
                                {pexp_desc = Pexp_ident {txt = Lident "n"}});
                               ("",
                                {pexp_desc = Pexp_constant (Const_int 1)})])})])})])})})})};
      pvb_attributes = [({txt = "n"}, PStr [])]}])}]
=========

*)

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
