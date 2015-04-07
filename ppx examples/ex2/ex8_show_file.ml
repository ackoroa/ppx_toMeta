type file = {
  name : string;
  perm : int [@printer fun fmt -> fprintf fmt "0o%03o"];
} [@@deriving show];;
print_endline (show_file {name="hello"; perm=15});;

(* ocamlfind ocamlc -package ppx_deriving.std -o ex8 ex8_show_file.ml *)
