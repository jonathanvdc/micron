module libmicron.ConstantPattern

let (|Constant|_|) expected actual =
    if actual = expected then Some() else None
