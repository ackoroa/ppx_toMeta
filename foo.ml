let rec pow x n = if n=0 then 1 else x * (pow x (n-1)) 
[@@static []][@@static [x]][@@static [n]][@@static [x;n]]
