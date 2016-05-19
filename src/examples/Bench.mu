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

let fibSum n =
    if n >= 0
        then fib n + (fibSum (n - 1))
        else 0

let main =
    let num = 40000 in
    let msg = showInt (fibSum num) in
    writeLine msg
