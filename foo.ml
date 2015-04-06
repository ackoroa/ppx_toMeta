let rec nestedBranchRec x y =
  if x = 0
    then if y = 0 then "x" else nestedBranchRec (x + 1) (y + 1)
    else "_"
[@@static [x]]
