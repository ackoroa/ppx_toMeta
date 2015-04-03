let rec f x y =
  if x = 0
    then if y = 0 then true else f x y
    else false
[@@static []][@@static [x]][@@static [y]][@@static [x;y]]
