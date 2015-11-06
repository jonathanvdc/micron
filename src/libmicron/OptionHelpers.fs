namespace libmicron

module OptionHelpers = 
    /// Tries to "recover" from a valueless option
    /// by returning the alternative lazy option
    /// if and only if that is the case.
    let recover (alt : Lazy<'a option>) : 'a option -> 'a option = function
    | Some x -> Some x
    | None -> alt.Value

    /// "Coalesces" the given option value with
    /// some lazy value. If the non-lazy
    /// option has a value, then that is returned.
    /// Otherwise, the lazy value is evaluated and
    /// returned.
    let coalesce (alt : Lazy<'a>) : 'a option -> 'a = function
    | Some x -> x
    | None -> alt.Value
