let plus x n = x + n
let plus_xStat x n = x + n [@@x]
let plus_nStat x n = x + n [@@n]
let plus_xnStat x n = x + n [@@x][@@n]

let rec pow x n = if n=0 then 1 else x * (pow x (n-1))
let rec pow_xStat x n = if n=0 then 1 else x * (pow_xStat x (n-1)) [@@x]
let rec pow_nStat x n = if n=0 then 1 else x * (pow_nStat x (n-1)) [@@n]
let rec pow_xnStat x n = if n=0 then 1 else x * (pow_xnStat x (n-1)) [@@x][@@n]

let rec powm x n = 
  match n with
    0 -> 1 
    | _ -> x * (powm x (n-1)) 
let rec powm_xStat x n = 
  match n with
    0 -> 1 
    | _ -> x * (powm_xStat x (n-1)) 
[@@x]
let rec powm_nStat x n = 
  match n with
    0 -> 1 
    | _ -> x * (powm_nStat x (n-1)) 
[@@n]
let rec powm_xnStat x n = 
  match n with
    0 -> 1 
    | _ -> x * (powm_xnStat x (n-1)) 
[@@x][@@n]

let rec double xs =
  match xs with
    [] -> []
    | x::xs -> (2*x)::(double xs)
let rec double_xsStat xs =
  match xs with
    [] -> []
    | x::xs -> (2*x)::(double_xsStat xs)
[@@xs]

let rec ff x n =
  if x = 0 then 0 else pow x n + ff (x-1) 
let rec ff x n =
  if x = 0 then 0 else pow_xStat x n + ff (x-1) 
[@@x]
let rec ff x n =
  if x = 0 then 0 else pow_nStat x n + ff (x-1) 
[@@n]
let rec ff x n =
  if x = 0 then 0 else pow_xnStat x n + ff (x-1) 
[@@x][@@n]
