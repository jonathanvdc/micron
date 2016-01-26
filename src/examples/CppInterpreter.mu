open stdlib

// A simple, yet accurate C++ interpreter.

let main =
    // Reads a single line of C++ code, and executes it.
    composeIO readLine (fail "Segmentation fault (core dumped)")
