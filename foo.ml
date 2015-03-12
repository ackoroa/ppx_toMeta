let rec pow x n = if n=0 then 1 else x * (pow x (n-1)) [@@n]

let ff x n =
  if x = 0 then 0 else pow x n [@s] + ff (x-1) 
[@@n]
