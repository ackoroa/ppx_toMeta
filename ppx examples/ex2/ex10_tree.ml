type point2d = float * float
[@@deriving show]
let x = (2.0,3.0);;

type 'a tree = Empty | Node of 'a * ('a tree) * ('a tree)
[@@deriving show]

type tree_f = point2d tree
[@@deriving show]

(* let t1 = Node(x,Empty,Empty);; *)
(* let t2 = Node(x,t1,t1);; *)
let t2 = let t1=Node(x,Empty,Empty) in  Node(x,t1,t1);;
let t3 = Node(x,t2,t2);;
let t4 = Node(x,t3,t3);;
let t5 = Node(x,t3,t4);;


(* print_endline (show_tree_f t1);; *)
print_endline (show_tree_f t2);;
print_endline (show_tree_f t3);;
print_endline (show_tree_f t4);;
print_endline (show_tree_f t5);;

print_endline (show_tree pp_point2d t2);;

(* ocamlfind ocamlc -package ppx_deriving.std -o ex ex10_tree.ml *)
