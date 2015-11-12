namespace libmicron

open Flame
open Flame.Functional

/// Defines an unknown type: a type that
/// has yet to be inferred.
type UnknownType() =
    inherit FunctionalType("?", null)