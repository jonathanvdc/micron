module fib

open stdlib

// Computes the nth fibonacci number.
let fib n =
    let fibAcc m acc1 acc2 =
        if m == 0
            // We're done here. Return the accumulator variable.
            then acc1
            // Update the accumulator variables, and call this function
            // recursively.
            else fibAcc (m - 1) acc2 (acc1 + acc2)
    in fibAcc n 1 1

// Computes n factorial.
let fac n =
    let facAcc m acc =
        if m == 0
            then acc
            else facAcc (m - 1) (m * acc)
    in facAcc n 1
