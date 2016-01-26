open stdlib

let main =
  let nums = range 1 10 in
  writeLine (showIntList (map square nums));
  writeLine (showInt <| sum nums);
  writeLine (showIntList <| sort (<=) <| 1::7::4::5::2::9::3::6::8::nil)
