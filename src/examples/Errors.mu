// A collection of functions that will result in
// compile-time errors.

let f x = x
let f x = 3

let g x =
    if x then 3 else x

let main = 3
