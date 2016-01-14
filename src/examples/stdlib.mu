
// The micron "standard library"
module stdlib

/// A function that always returns its first argument.
let const x y = x

/// Function composition.
let compose g f x = g (f x)

// Function composition operators.
let infixl(3) f >> g = compose g f
let infixl(3) f << g = compose f g
