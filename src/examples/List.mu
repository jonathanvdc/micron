open stdlib

let showIntList xs =
  let helper comma xs =
    if isNil xs
      then "]"
      else ((if comma then ", " else "") ++ showInt (head xs) ++ helper true (tail xs))
  in "[" ++ helper false xs

let map f xs =
  if isNil xs then nil else f (head xs) :- map f (tail xs)

let square x = x * x

let main = writeLine (showIntList (map square (1 :- 2 :- 3 :- nil)))
