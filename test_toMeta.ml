let plus x n = x + n
let plus_xStat x n = x + n [@@x]
let plus_nStat x n = x + n [@@n]
let plus_xnStat x n = x + n [@@x][@@n]

let rec pow x n = if n=0 then 1 else x * (pow x (n-1))
let rec pow_xStat x n = if n=0 then 1 else x * (pow_xStat x (n-1)) [@@x]
let rec pow_nStat x n = if n=0 then 1 else x * (pow_nStat x (n-1)) [@@n]
let rec pow_xnStat x n = if n=0 then 1 else x * (pow_xnStat x (n-1)) [@@x][@@n]

let rec q x y z = if z = 0 then 0 else x * y + q x y (z-1)
let rec q x y z = if z = 0 then 0 else x * y + q x y (z-1) [@@x]
let rec q x y z = if z = 0 then 0 else x * y + q x y (z-1) [@@y]
let rec q x y z = if z = 0 then 0 else x * y + q x y (z-1) [@@z]
let rec q x y z = if z = 0 then 0 else x * y + q x y (z-1) [@@x][@@y]
let rec q x y z = if z = 0 then 0 else x * y + q x y (z-1) [@@x][@@z]
let rec q x y z = if z = 0 then 0 else x * y + q x y (z-1) [@@y][@@z]
let rec q x y z = if z = 0 then 0 else x * y + q x y (z-1) [@@x][@@y][@@z]

let rec pow x n = 
  match n with
    0 -> 1 
    | _ -> x * (pow x (n-1)) 
let rec pow_xStat x n = 
  match n with
    0 -> 1 
    | _ -> x * (pow_xStat x (n-1)) 
[@@x]
let rec pow_nStat x n = 
  match n with
    0 -> 1 
    | _ -> x * (pow_nStat x (n-1)) 
[@@n]
let rec pow_xnStat x n = 
  match n with
    0 -> 1 
    | _ -> x * (pow_xnStat x (n-1)) 
[@@x][@@n]

let rec sum xs =
  match xs with
    [] -> 0
    | x::xs -> x + (sum xs)
let rec sum_xsStat xs =
  match xs with
    [] -> 0
    | x::xs -> x + (sum_xsStat xs)
[@@xs]

let rec double xs =
  match xs with
    [] -> []
    | x::xs -> (2*x)::(double xs)
let rec double_xsStat xs =
  match xs with
    [] -> []
    | x::xs -> (2*x)::(double_xsStat xs)
[@@xs]

let rec f x n =
  match n with
    1 -> 1
    | _ -> if x > 0 then x * (f x (n-1)) else 1
let rec f_xStat x n =
  match n with
    1 -> 1
    | _ -> if x > 0 then x * (f_xStat x (n-1)) else 1
[@@x]
let rec f_nStat x n =
  match n with
    1 -> 1
    | _ -> if x > 0 then x * (f_nStat x (n-1)) else 1
[@@n]
let rec f_xnStat x n =
  match n with
    1 -> 1
    | _ -> if x > 0 then x * (f_xnStat x (n-1)) else 1
[@@x][@@n]
