let powS n =
  let rec aux x n =
    if n = 0 then .< 1  >. else .< .~x * .~(aux x (n - 1))  >. in
  .< let pow x = .~(aux .< x  >. n) in pow  >.

let ffS n =
  .< let pow = .~(powS n) in
     let rec ff x = if x = 0 then 0 else pow n + (ff (x - 1)) in ff  >.
