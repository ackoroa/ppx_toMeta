let plus x n = x + n
let plus_xStat x n = x + n [@@x]
let plus_nStat x n = x + n [@@n]
let plus_xnStat x n = x + n [@@x][@@n]

let rec pow x n = if n=0 then 1 else x * (pow x (n-1))
let rec pow_xStat x n = if n=0 then 1 else x * (pow_xStat x (n-1)) [@@x]

