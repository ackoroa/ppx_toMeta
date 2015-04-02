let f x y =
  if x = 0
    then if y = 0 then true else false
    else false
[@@static [y]] [@@static [x;y]] 
