type ('a,'b) pair = 'a * 'b
[@@deriving show]

type point2d = (float,float) pair
[@@deriving show]

(*
ocamlfind ocamlc -package ppx_deriving.std ex9_poly_pair.ml -dsource
*)
