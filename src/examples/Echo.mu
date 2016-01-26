open stdlib

// type: unit -> IO<a>
let echo u =
    // Read a line, and echo it,
    // then call this function recursively.
    readLine >>= writeLine
             >>= echo

// Main function just brings the 'echo'
// function into action.
let main = echo unit
