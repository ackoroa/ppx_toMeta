let powS n = 
  let rec aux x n = match n with | 0 -> .< 1 >. | _ -> .< .~x * .~(aux x (n-1)) >. in
   .< let f x = .~(aux .< x  >. n) in f  >.
