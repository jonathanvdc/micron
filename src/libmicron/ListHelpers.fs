namespace libmicron

module ListHelpers =
    /// Split a list of Xs in two at the first point where the predicate matches. If no such
    /// element is found, the second half will be empty. For example:
    ///
    ///     splitWhen even [1; 3; 5; 6; 7; 8; 9]
    ///
    /// returns
    ///
    ///     ([1; 3; 5]; [6; 7; 8; 9])
    let splitWhen (pred : 'X -> bool) : 'X list -> 'X list * 'X list =
        let rec loop acc = function
            | x::xs when not (pred x) -> loop (x::acc) xs
            | xs -> (List.rev acc, xs)
        loop []

    /// Partition a list of Xs, ending each sublist after the next element where a predicate
    /// is met. However, instead of [a; b; end], yield a tuple ([a; b], end).
    /// Remove all elements after the final element for which the predicate holds. For example:
    ///     
    ///     cutAfter even [1; 3; 400; 5; 300; 800; 1]
    ///
    /// returns
    ///
    ///     [[1; 3], 400;  [5], 300;  [], 800]
    ///
    let rec cutAfter (pred : 'X -> bool) (xs : 'X list) : ('X list * 'X) list =
        match splitWhen pred xs with
        | (xs, []) -> []
        | (xs, y::ys) -> (xs, y) :: cutAfter pred ys