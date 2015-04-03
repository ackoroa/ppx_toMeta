let rec pow x n = if n=0 then 1 else x * (pow x (n-1)) 
[@@static [n]]

let rec ff y m =
  if y = 0 then 0 else pow y m [@static.use [1]] + ff (y - 1) m
[@@static [m]]
