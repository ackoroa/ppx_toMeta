let rec geo x n =
  if n = 0 then 1 else x * geo (x + 1) (n - 1)
[@@static [n]]
