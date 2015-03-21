let plus x n = x + n [@@static []] [@@static [x]] [@@static [n]] [@@static [x;n]]

let rec pow x n = if n=0 then 1 else x * (pow x (n-1)) [@@static []] [@@static [x]] [@@static [n]] [@@static [x;n]]

let rec powm x n = 
  match n with
    0 -> 1 
    | _ -> x * (powm x (n-1)) 
[@@static []] [@@static [x]] [@@static [n]] [@@static [x;n]]

let rec double xs =
  match xs with
    [] -> []
    | x::xs -> (2*x)::(double xs)
[@@static []] [@@static [xs]]

let rec ff x n =
  if x = 0 then 0 else pow x n [@static.use []] + ff (x-1) n
[@@static []]

let rec ff x n =
  if x = 0 then 0 else pow x n [@static.use [x]] + ff (x-1) n
[@@static [x]]

let rec ff x n =
  if x = 0 then 0 else pow x n [@static.use [n]] + ff (x-1) n
[@@static [n]] 

let rec ff x n =
  if x = 0 then 0 else pow x n [@static.use [x;n]] + ff (x-1) n
[@@static [x;n]]
