
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
let infixr(1) f <| x = f x

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

// Misc. integer stuff
let even x = x % 2 == 0
let odd x = x % 2 == 1
let square x = x * x
let max x y = if x > y then x else y
let min x y = if x < y then x else y

// String concatenation
let infixl(5) l ~ r = sconcat l r

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
let mapIO f m =
    let binder x = return (f x) in
    m >>= binder

// Compose IO actions, throwing away the first result.
let infixl(1) m1; m2 =
    m1 >>= (let k x = m2 in k)

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
let splitString s cs = l_splitString s cs
let toCharList s = l_toCharList s
let trimString s = stringTrim s
let isEmptyString s = stringIsEmpty s

// Turn (a :: b :: c :: nil) into (f a (f b (f c z))).
let foldr f z l =
  if isNil l then z else f (head l) (foldr f z (tail l))

// if the list is empty, the result is the initial value; else
// we recurse immediately, making the new initial value the result
// of combining the old initial value with the first element.
let foldl f z l =
  if isNil l then z else foldl f (f z (head l)) (tail l)

// Reverses a list.
let reverse xs = let g t h = h :: t in foldl g nil xs

// Apply f to each element in a list.
let map f l =
  let g h t = f h :: t
  in foldr g nil l

// Filter a list using condition f.
let filter f l =
  let g h t = if f h then h :: t else t
  in foldr g nil l

// Sum a list of integers.
let sum l = foldr (+) 0 l

// Get a list's length.
let length l =
  let g x a = 1 + a
  in foldr g 0 l

// List concatenation.
let infixl(7) x ++ y = foldr (::) y x

// Create a list of n copies of some element.
let repeat n a =
  if n == 0 then nil else a :: repeat (n-1) a

// Create an ascending list of integers from a up to and including b.
let range a b =
  if a > b then nil else a :: range (a+1) b

// Return the largest/smallest element in the list.
// These functions assume the list is non-empty!
let maximum l = foldr max (head l) (tail l)
let minimum l = foldr min (head l) (tail l)

// Take the first n elements from a list.
let take n l =
  if (n <= 0 || isNil l)
    then nil
    else head l :: take (n-1) (tail l)

// Drop the first n elements from a list.
let drop n l =
  if (n <= 0 || isNil l)
    then l
    else drop (n-1) (tail l)

// If leq (≤) is a total order on the elements in the list, sort the list in ascending order.
let sort leq xs =
  if isNil xs then nil
  else let h = head xs in
       let t = tail xs in
       let isLesser x = leq x h in
       let isGreater x = not (leq x h) in
       sort leq (filter isLesser t) ++ (h :: sort leq (filter isGreater t))

// Join a list of strings with x: join x (a::b::c::nil) == a ~ x ~ b ~ x ~ c.
let join x l =
  if isNil l then ""
  else let j a b = a ~ x ~ b
       in head l ~ foldl j "" (tail l)

// Show a List<a>, using some function that shows an a.
let showListWith show l = "[" ~ join ", " (map show l) ~ "]"

// Show a list of integers.
let showIntList l = showListWith showInt l

