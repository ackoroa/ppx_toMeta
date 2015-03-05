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

