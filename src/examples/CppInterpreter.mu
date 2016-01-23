open stdlib

let main =
    composeIO readLine (fail "Segmentation fault (core dumped)")
