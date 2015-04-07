let x = [%expr 222+222]

(*

ocamlfind ocamlc -c -package ppx_tools.metaquot  -dsource -o ex2 ex2_quasi.ml

ocamlfind ocamlc -package ppx_tools.metaquot  -dsource -o ex2 ocamlcommon.cma ex2_quasi.ml

ocamlbuild -use-ocamlfind -package ppx_tools.metaquot ex2_quasi.byte
*)
