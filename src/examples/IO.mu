open stdlib
open fib

let main =
    let binder line =
        // Parse the input line as an integer.
        let num = parseInt line in
        // Index the fibonacci sequence with that
        // value, and convert it to a string.
        let msg = showInt (fib num) in
        // Print that string.
        composeIO (writeLine "The fibonacci number you asked for is:") (writeLine msg) in
    composeIO (writeLine "Which fibonacci number would you like to compute?") (readLine >>= binder)
