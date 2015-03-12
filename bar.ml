let powS n =
  let rec aux x n =
    if n = 0 then .< 1  >. else .< .~x * .~(aux x (n - 1))  >. in
  .< let f x = .~(aux .< x  >. n) in f  >.

let ffS n =
  .<let rec ff x =
      if x=0 then 0 else .~(powS .<x>. n) + ff (x-1) in ff>.


