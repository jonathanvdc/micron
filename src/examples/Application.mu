module Application

let apply =
    let f x = 0 in
    let g x = f in
    let h x = g in
    h 0 0 0
