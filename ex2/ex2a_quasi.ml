let y = [%expr 333]
let x = [%expr 222+[%e y]]

(*

ocamlfind ocamlc -c -package ppx_tools.metaquot  -dsource  ex2a_quasi.ml

ocamlfind ocamlc -package ppx_tools.metaquot  -dsource -o ex2 ocamlcommon.cma ex2_quasi.ml

ocamlbuild -use-ocamlfind -package ppx_tools.metaquot ex2_quasi.byte
*)
