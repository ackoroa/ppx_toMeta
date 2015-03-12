let rec f x n =
  match n with
    1 -> 1
    | _ -> if x > 0 then x * (f x (n-1)) else 1
[@@n]
