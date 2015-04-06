(* Not all arguments of fun appl is escaped *)

let geo_n n =
  let rec aux_2 x n =
    if n = 0 then .< 1  >. else .< .~x * .~(aux_2 .<.~x + 1>. (n - 1))  >. in
  .< let geo x = .~(aux_2 .< x  >. n) in geo  >.

let nestedBranchRec_x x =
  let rec aux_4 x y =
    if x = 0
    then .< (if .~y = 0 then "x" else .~(aux_4 (x + 1) .<.~y + 1>. ))  >.
    else .< "_"  >. in
  .< let nestedBranchRec y = .~(aux_4 x .< y  >.) in nestedBranchRec  >.



(* mutating variable causing problems *)

let geo_x x =
  .<
    let rec geo n = .~(.< if n = 0 then 1 else x * (geo (x + 1) (n - 1))  >.) in
    geo  >.

let nestedBranchRec_y y =
  let rec aux_3 x y = if y = 0 then .< "x"  >. else aux_3 .<.~x + 1>. (y - 1) in
  .<
    let nestedBranchRec x =
      .~(.< if x = 0 then .~(aux_3 .< x  >. y) else "_"  >.) in
    nestedBranchRec  >.
