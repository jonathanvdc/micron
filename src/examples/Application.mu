module Application

open stdlib

let apply =
    let f x = 0 in
    let g x = f in
    let h x = g in
    h 0 0 0

let const42 = const 42

/// Computes the nth fibonacci number.
let fib n =
    if n == 0 || n == 1
        then 1
        else fib (n - 1) + fib (n - 2)
