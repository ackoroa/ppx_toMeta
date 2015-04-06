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
