ocamlbuild -r -package compiler-libs.common -package ppx_tools ppx_builder.native

ocamlc -dsource -ppx ./ppx_builder.native test_builder.ml 


type t = {
  x: int;
  y[@label foo]: int;
  z[@default 3]: int;}[@@builder ]
and s = {
  a: string;
  b[@opt ]: int option;
  c: ((int)[@default 2]);}[@@builder ]
and sum =
  | A of int
  | B of string* ((string)[@label str])
  | C of ((int)[@label i][@default 0])* ((string)[@label s][@default ""])
[@@builder ]
let t ~x  ~foo:y  ?(z= 3)  () = { x; y; z }
let s ~a  ?b  ?(c= 2)  () = { a; b; c }
let sum_A x0 () = A x0
let sum_B x0 ~str:x1  () = B (x0, x1)
let sum_C ?i:(x0= 0)  ?s:(x1= "")  () = C (x0, x1)
====================================================================
let l = List.filter [%matches ? 'a'..'z'] ['a';'A';'X';'x']

let f = [%matches ? Some i when i >= 0]

===>

pls2nus@loris-laptop:~/hg/ppx_ak/frisch$ make matches-t 
ocamlc -dsource -ppx ./ppx_matches.native test_matches.ml
let l =
  List.filter (function | 'a'..'z' -> true | _ -> false) ['a'; 'A'; 'X'; 'x']
let f = function | Some i when i >= 0 -> true | _ -> false
====================================================================
make minidoc
ocamlbuild -r -package compiler-libs.common -package ppx_tools minidoc.native
Finished, 4 targets (0 cached) in 00:00:00.
====================================================================
====================================================================
====================================================================
====================================================================
====================================================================
====================================================================
====================================================================
====================================================================
====================================================================
====================================================================
====================================================================
====================================================================
====================================================================
====================================================================




