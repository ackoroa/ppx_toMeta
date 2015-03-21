let rec pow x n = if n=0 then 1 else x * (pow x (n-1)) [@@static [x]][@@static [n]]

let rec ff x n =
  if x = 0 then 0 else pow x n + ff (x-1) n
[@@static [x]][@@static [n]]
