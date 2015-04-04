let plus x n = x + n [@@static []] [@@static [x]] [@@static [n]] [@@static [x;n]]

let branch x n = if x = 0 then x else n [@@static []] [@@static [x]] [@@static [n]] [@@static [x;n]]

let nestedBranch x y =
  if x = 0
    then if y = 0 then "x" else "xy"
    else "_"
[@@static []] [@@static [x]] [@@static [y]] [@@static [x;y]] 

let rec pow x n = if n=0 then 1 else x * (pow x (n-1)) 
[@@static []] [@@static [x]] [@@static [n]] [@@static [x;n]]

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

let nestedBranchm x y =
  match x with
    0 ->
        begin match y with
          0 -> "x" 
          | _ -> "xy"
        end
    | _ -> "_"
[@@static []] [@@static [x]] [@@static [y]] [@@static [x;y]] 

let rec ff y m =
  if y = 0 then 0 else pow y m [@static.use] + ff (y - 1) m
[@@static []] [@@static [y]] [@@static [m]] [@@static [y;m]]

