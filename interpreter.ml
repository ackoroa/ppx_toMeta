open Interpreter_defs

let rec eval1 e =
  match e with
    Int i -> i
    | Add (e1,e2) -> (eval1 e1)+(eval1 e2)
    | Sub (e1,e2) -> (eval1 e1)-(eval1 e2)
    | Mul (e1,e2) -> (eval1 e1)*(eval1 e2)
    | Div (e1,e2) -> (eval1 e1)/(eval1 e2)
[@@static [e]]

let p = Add (Int 3, Mul (Sub (Int 2, Int 3), Div (Int 4, Int 2)))

let sample = eval1 p
let sample_staged = eval1_e p


