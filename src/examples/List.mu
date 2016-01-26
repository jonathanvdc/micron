open stdlib

// Parses the given string as a list of integers.
let parseList line =
    map parseInt (splitString line nil)

// Sorts the given list, converts it to a string, and prints it.
let writeSortedList l =
    l |> sort (<=)
      |> showIntList 
      |> writeLine

// Reads a line from stdin, parse it as a list of integers,
// and write its sorted counterpart to stdout.
let main =
    (mapIO readLine parseList) >>= writeSortedList
