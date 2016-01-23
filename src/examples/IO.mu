open stdlib

/// Computes the nth fibonacci number.
let fib n =
    if n == 0 || n == 1
        then 1
        else fib (n - 1) + fib (n - 2)

let main =
    let binder line =
        // Parse the input line as an integer.
        let num = parseInt line in
        // Index the fibonacci sequence with that
        // value, and convert it to a string.
        let msg = showInt (fib num) in
        // Print that string.
        writeLine msg in
    composeIO (writeLine "Hello, world!") (readLine >>= binder)
