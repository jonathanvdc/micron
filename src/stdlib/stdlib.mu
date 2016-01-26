
// The glorious micron "standard library"
module stdlib

open primops
open primio
open primlist

/// A function that always returns its first argument.
let const x y = x

/// Flips the given function's first two operands.
let flip f x y = f y x

/// Function composition
let compose g f x = g (f x)

// Boolean negation
let not flag = notb flag

// Function composition operators
let infixl(1) f >> g = compose g f
let infixl(1) f << g = compose f g

// Forward and backward pipe
let infixl(1) x |> f = f x
let infixl(1) f <| x = f x

// Integer arithmetic
let infixl(8) x + y = addi x y
let infixl(8) x - y = subi x y
let infixl(9) x * y = muli x y
let infixl(9) x / y = divi x y
let infixl(9) x % y = modi x y

let infixl(5) x & y = andi x y
let infixl(6) x | y = ori x y
let infixl(6) x ^ y = xori x y
let infixl(7) x <<< y = shli x y
let infixl(7) x >>> y = shri x y

// Integer comparisons
let infixl(4) x == y = eqi x y
let infixl(4) x != y = neqi x y
let infixl(4) x < y = lti x y
let infixl(4) x <= y = lei x y
let infixl(4) x > y = gti x y
let infixl(4) x >= y = gei x y

// String concatenation
let infixl(5) l ++ r = sconcat l r

// Boolean constants
// Note: these have to stay here, because
// && and || rely on them.
let true = 0 == 0
let false = not true

// TODO: make these compiler intrinsics,
//       to enforce short-circuiting.
let infixl(3) x && y = if x then y else false
let infixl(2) x || y = if x then true else y

// Conversion functions
let showInt x = conv_i4_s x
let parseInt x = conv_s_i4 x
let showChar c = conv_c_s c
let parseChar c = conv_s_c c

// IO monad functions

// Monadic bind
let infixl(1) m >>= f = bindIO m f
// Monad creation
let return value = returnIO value
// Exception monad creation
let fail errorMessage = failIO errorMessage

// Monadic map
let mapIO m f =
    let binder x = return (f x) in
    m >>= binder

// Monad composition
let composeIO m1 m2 =
    let binder m = m2 in
    m1 >>= binder

// IO functions
let writeLine message = writeLineIO message
let readLine = readLineIO

// Unit
let unit = getUnit

///////////////////////////////////////
// List functions

let infixr(1) x :: y = l_cons x y
let isNil = l_isNil
let head = l_head
let tail = l_tail
let nil = l_nil
let singleton x = x :: nil

// String manipulation functions
let splitString s cs = l_splitString
