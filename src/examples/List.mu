open stdlib

let showIntList xs =
  let helper comma xs =
    if isNil xs
      then "]"
      else ((if comma then ", " else "") ++ showInt (head xs) ++ helper true (tail xs))
  in "[" ++ helper false xs

let map f xs =
  if isNil xs then nil else f (head xs) :: map f (tail xs)

let filter f xs =
  if isNil xs
    then nil
    else let h = head xs in
         let r = filter f (tail xs) in
         if f h then h :: r else r

let square x = x * x

let even x = x % 2 == 0

let append x y =
  if isNil x then y else head x :: append (tail x) y

let infixl(7) x :+ y = append x y

// If leq (≤) is a total order on the elements in the list,
// sort the list in ascending order.
let qsort leq xs =
  if isNil xs then nil
  else let h = head xs in
       let t = tail xs in
       let leqh x = leq x h in
       let nleqh x = not (leqh x) in
       let lesser  = filter leqh t in
       let greater = filter nleqh t in
       qsort leq lesser :+ (h :: nil) :+ qsort leq greater

let main = writeLine (showIntList (qsort (<=) (1::7::4::5::2::9::3::6::8::nil)))
