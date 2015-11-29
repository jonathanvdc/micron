
module Constant

let zero = 0
let three = 3.0
let id = let f x = x in f
let const = let f x y = x in f
let select =
    let f cond trueVal falseVal =
        if cond
            then trueVal
            else falseVal
    in f

let capt =
    let g y = 0 in
    let z = 0 in
    let f x = g in
    f 3

let compose =
    let f g h x = g (h x) in
    let g x = x in
    let h x = x in
    f g h

let recursion =
    let f x = f 0 in f
