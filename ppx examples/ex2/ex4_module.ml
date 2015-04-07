module type S = sig
  [@@@id1]
  type t =
    | A [@id2]
    | B [@id3] of int [@id4]   [@@id5]
  [@@@id6] [@@@id7]
  [@@@id8]
  type s
  [@@id9]
end
