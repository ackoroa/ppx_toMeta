let powS n = 
  let rec aux x n =
    if n = 0 then lift 1 else lift ((esc x) * (esc (aux x (n-1))))
  in lift (let f x = esc (aux (lift x) n) in f)

