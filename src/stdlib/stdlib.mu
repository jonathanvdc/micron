
// The glorious micron "standard library"
module stdlib

open primops

/// A function that always returns its first argument.
let const x y = x

/// Flips the given function's first two operands.
let flip f x y = f y x

/// Function composition
let compose g f x = g (f x)

// Function composition operators
let infixl(2) f >> g = compose g f
let infixl(2) f << g = compose f g

// Forward and backward pipe
let infixl(2) x |> f = f x
let infixl(2) f <| x = f x

// Integer arithmetic
let infixl(4) x + y = addi x y
let infixl(4) x - y = subi x y
let infixl(5) x * y = muli x y
let infixl(5) x / y = divi x y
let infixl(5) x % y = modi x y

let infixl(7) x & y = andi x y
let infixl(6) x | y = ori x y
let infixl(6) x ^ y = xori x y
let infixl(8) x <<< y = shli x y
let infixl(8) x >>> y = shri x y

// Integer comparisons
let infixl(3) x == y = eqi x y
let infixl(3) x != y = neqi x y
let infixl(3) x < y = lti x y
let infixl(3) x <= y = lei x y
let infixl(3) x > y = gti x y
let infixl(3) x >= y = gei x y

// Boolean negation
let not flag = notb flag

// Boolean constants
let true = 0 == 0
let false = not true

// Boolean comparisons

// TODO: make these compiler intrinsics,
//       to enforce short-circuiting.
let infixl(9) x || y = if x then true else y
let infixl(10) x && y = if x then y else false
