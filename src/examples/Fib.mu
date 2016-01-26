module fib

open stdlib

/// Computes the nth fibonacci number.
let fib n =
    if n == 0 || n == 1
        then 1
        else fib (n - 1) + fib (n - 2)
