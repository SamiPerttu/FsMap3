/// This central module contains essential definitions that adapt F# to our taste.
/// It also contains other useful definitions that do not have their own module.
/// Every other module should open this module.
module Fuse.Common

open System
open LanguagePrimitives


/// Type alias for uint32.
type uint = uint32

/// Converts the argument to a uint32.
let inline uint x = uint32 x



// Fix the equality operator to inline structural equality tests. The implementation is from
// a blog post by Zeckul, "How to avoid boxing value types in F# equality comparisons".
// The default equality operator employs Object.Equals, which unfortunately entails boxing of value
// types in equality tests. The System.IEquatable interface was introduced in .NET 2.0 to optimize
// such tests in some situations. That is the interface our replacement operator targets.
// There are a few drawbacks to our approach:
// -It does not work with enums.
// -It does not work inside definitions or definition chains for the types being defined.
// -It does not take effect in third-party code, for example, generic functions in prominent modules
//  such as FSharp.Core.List still use Object.Equals.
// The IL code emitted for the new equality operators consists of a CALLVIRT opcode of the respective
// System.IEquatable<'a> interface. The call can be replaced by the JIT runtime with inlined code.
#nowarn "86"
let inline fastEquals<'a when 'a :> System.IEquatable<'a>> (x : 'a) (y : 'a) = x.Equals y

/// Fast equality operator that inlines structural equality tests. Does not cover dissimilar types.
let inline ( = ) x y = fastEquals x y

/// Fast inequality operator that inlines structural inequality tests. Does not cover dissimilar types.
let inline ( <> ) x y = not (fastEquals x y)

/// Legacy equality operator that employs generic equality.
let inline ( =. ) x y = FSharp.Core.Operators.(=) x y

/// Legacy inequality operator that employs generic equality.
let inline ( <>. ) x y = FSharp.Core.Operators.(<>) x y



/// Generic numeric literal G.
[<AutoOpen>]
module NumericLiteralG = 
  // Based on an F# snippet by Daniel Fabian.
  type GenericNumber =
    static member inline genericNumber(x : int, _ : int8) = int8 x
    static member inline genericNumber(x : int, _ : uint8) = uint8 x
    static member inline genericNumber(x : int, _ : int16) = int16 x
    static member inline genericNumber(x : int, _ : uint16) = uint16 x
    static member inline genericNumber(x : int, _ : int) = x
    static member inline genericNumber(x : int, _ : uint) = uint32 x
    static member inline genericNumber(x : int, _ : int64) = int64 x
    static member inline genericNumber(x : int, _ : uint64) = uint64 x
    static member inline genericNumber(x : int, _ : float32) = float32 x
    static member inline genericNumber(x : int, _ : float) = float x
    static member inline genericNumber(x : int64, _ : int64) = int64 x
    static member inline genericNumber(x : int64, _ : uint64) = uint64 x
    static member inline genericNumber(x : int64, _ : float32) = float32 x
    static member inline genericNumber(x : int64, _ : float) = float x
    static member inline genericNumber(x : string, _ : float32) = float32 x
    static member inline genericNumber(x : string, _ : float) = float x

  // There are two tricks here. Trick one allows us to use static methods from the GenericNumber class.
  // Type constraints cannot include concrete types, so we route it through a function call. Trick two
  // allows us to overload based on the return type: we add an "invisible" argument of the return type,
  // enabling type inference on it.
  let inline genericNumber n =
    let inline call(a : ^a, b : ^b, c : ^c) = ((^a or ^b or ^c) : (static member genericNumber : ^b * ^c -> ^c) (b, c))
    call(Unchecked.defaultof<GenericNumber>, n, Unchecked.defaultof<'b>)

  let inline FromZero() = GenericZero
  let inline FromOne() = GenericOne
  let inline FromInt32 n = genericNumber n
  let inline FromInt64 n = genericNumber n
  let inline FromString n = genericNumber n



// Global operator overloads.

/// Reference equality operator. Returns false if either a or b is not a reference type.
let inline ( === ) a b = obj.ReferenceEquals(a, b)

/// Reference inequality operator. Returns true if either a or b is not a reference type.
let inline ( <>= ) a b = obj.ReferenceEquals(a, b) = false

/// Duck typed dereferencing operator. Duck typing enables us to use it with other types besides reference cells.
let inline ( ! ) x = (^X : (member get_Value : unit -> _) (x))

/// Convenience duck typed infix operator that returns the value of option (or Optionval) a,
/// defaulting to b if the option does not have a value. Note that the default value b is always evaluated.
let inline ( >? ) (a : ^a) (b : ^b) = if (^a : (member get_IsSome : unit -> bool) (a)) then !a else b

/// Dot product operator. We declare this as a global, duck typed operator to make it compatible with
/// SIMD accelerated types from System.Numerics.
let inline ( *. ) (v : ^a) (u : ^a) = (^a : (static member Dot : ^a * ^a -> _) (v, u))

/// Cross product operator. We declare this as a global, duck typed operator to make it compatible with
/// SIMD accelerated types from System.Numerics.
let inline ( *+ ) (v : ^a) (u : ^a) = (^a : (static member Cross : ^a * ^a -> _) (v, u))



/// Numeric interval type definition. Most often, the "default" interval type is closed or left-closed
/// for floating point intervals and closed for integer intervals.
[<NoComparison>]
type Interval = Closed | LeftClosed | RightClosed | Open



// Some constants.

/// pi
[<Literal>]
let pi = 3.1415926535897932384626434

/// 2 * pi
[<Literal>]
let pi2 = 6.2831853071795864769252868

/// tau = 2 * pi
[<Literal>]
let tau = 6.2831853071795864769252868

/// 1/2 * pi
[<Literal>]
let pi12 = 1.5707963267948966192313217

/// 1/4 * pi
[<Literal>]
let pi14 = 0.7853981633974483096156608

/// sqrt(2)
[<Literal>]
let sqrt2 = 1.4142135623730950488016887

/// sqrt(3)
[<Literal>]
let sqrt3 = 1.7320508075688772935274463

/// sqrt(1/2)
[<Literal>]
let sqrt12 = 0.707106781186548

/// sqrt(1/3)
[<Literal>]
let sqrt13 = 0.577350269189626

/// log(2)
[<Literal>]
let ln2 = 0.6931471805599453094172321

/// log(10)
[<Literal>]
let ln10 = 2.30258509299405



/// Numeric conversions and attributes for generic functions such as Common.G.
type NumericTraits =

  static member inline convert(x : int, _ : int) = int x
  static member inline convert(x : int, _ : float) = float x
  static member inline convert(x : int, _ : float32) = float32 x
  static member inline convert(x : float, _ : int) = int x
  static member inline convert(x : float, _ : float) = float x
  static member inline convert(x : float, _ : float32) = float32 x
  static member inline convert(x : float32, _ : int) = int x
  static member inline convert(x : float32, _ : float) = float x
  static member inline convert(x : float32, _ : float32) = float32 x
  static member inline convert(a : ^a, b : ^b, c : ^c) = ((^a or ^b or ^c) : (static member convert : ^b * ^c -> ^c) (b, c))

  static member inline quotient(x : int, y : int, _ : float) = float x / float y
  static member inline quotient(x : int, y : int, _ : float32) = float32 x / float32 y
  static member inline quotient(x : int, y : float, _ : float) = float x / y
  static member inline quotient(x : int, y : float, _ : float32) = float32 x / float32 y
  static member inline quotient(x : float, y : int, _ : float) = x / float y
  static member inline quotient(x : float, y : int, _ : float32) = float32 x / float32 y
  static member inline quotient(x : float, y : float, _ : float) = x / y
  static member inline quotient(x : float, y : float, _ : float32) = float32 x / float32 y
  static member inline quotient(a : ^a, b : ^b, c : ^c, d : ^d) = ((^a or ^c or ^d) : (static member quotient : ^b * ^c * ^d -> ^d) (b, c, d))

  static member inline isIntegral(_ : int8) = ()
  static member inline isIntegral(_ : uint8) = ()
  static member inline isIntegral(_ : int16) = ()
  static member inline isIntegral(_ : uint16) = ()
  static member inline isIntegral(_ : int32) = ()
  static member inline isIntegral(_ : uint32) = ()
  static member inline isIntegral(_ : int64) = ()
  static member inline isIntegral(_ : uint64) = ()
  static member inline isIntegral(a : ^a, b : ^b) = ((^a or ^b) : (static member isIntegral : ^b -> unit) (b))

  static member inline isSigned(_ : int8) = ()
  static member inline isSigned(_ : int16) = ()
  static member inline isSigned(_ : int32) = ()
  static member inline isSigned(_ : int64) = ()
  static member inline isSigned(a : ^a, b : ^b) = ((^a or ^b) : (static member isSigned : ^b -> unit) (b))

  static member inline isUnsigned(_ : uint8) = ()
  static member inline isUnsigned(_ : uint16) = ()
  static member inline isUnsigned(_ : uint32) = ()
  static member inline isUnsigned(_ : uint64) = ()
  static member inline isUnsigned(a : ^a, b : ^b) = ((^a or ^b) : (static member isUnsigned : ^b -> unit) (b))

  static member inline minValue(_ : int8) = -0x80y
  static member inline minValue(_ : uint8) = 0uy
  static member inline minValue(_ : int16) = -0x8000s
  static member inline minValue(_ : uint16) = 0us
  static member inline minValue(_ : int32) = -0x80000000
  static member inline minValue(_ : uint32) = 0u
  static member inline minValue(_ : int64) = -0x8000000000000000L
  static member inline minValue(_ : uint64) = 0UL
  static member inline minValue(a : ^a, b : ^b) = ((^a or ^b) : (static member minValue : ^b -> ^b) (b))

  static member inline maxValue(_ : int8) = 0x7fy
  static member inline maxValue(_ : uint8) = 0xffuy
  static member inline maxValue(_ : int16) = 0x7fffs
  static member inline maxValue(_ : uint16) = 0xffffus
  static member inline maxValue(_ : int32) = 0x7fffffff
  static member inline maxValue(_ : uint32) = 0xffffffffu
  static member inline maxValue(_ : int64) = 0x7fffffffffffffffL
  static member inline maxValue(_ : uint64) = 0xffffffffffffffffUL
  static member inline maxValue(a : ^a, b : ^b) = ((^a or ^b) : (static member maxValue : ^b -> ^b) (b))


/// This function complements the generic numeric literal G by providing generic conversions of arbitrary
/// numbers. For example, (G pi) is pi as a generic number.
let inline G x = NumericTraits.convert(Unchecked.defaultof<NumericTraits>, x, GenericZero)

// Note. If the argument to Common.G is constant, then the compiler emits the constant in the original type.
// The type conversion is optimized away at runtime by the JIT compiler.

/// Generic conversion of quotients x / y. This is equivalent in effect with G x / G y.
/// For now, the arguments must be int or float and the target type float or float32.
let inline Q x y = NumericTraits.quotient(Unchecked.defaultof<NumericTraits>, x, y, GenericZero)


/// Forces type of x to be integral, or compilation fails.
let inline forceIntegral x = NumericTraits.isIntegral(Unchecked.defaultof<NumericTraits>, x)

/// Forces type of x to be signed, or compilation fails.
let inline forceSigned x = NumericTraits.isSigned(Unchecked.defaultof<NumericTraits>, x)

/// Forces type of x to be unsigned, or compilation fails.
let inline forceUnsigned x = NumericTraits.isUnsigned(Unchecked.defaultof<NumericTraits>, x)


/// The minimum value of the target type of the conversion function.
/// The conversion function typically has the same name as the type.
let inline minValue (x : _ -> 'a) = NumericTraits.minValue(Unchecked.defaultof<NumericTraits>, Unchecked.defaultof<'a>)

/// The minimum value of the argument type.
let inline minValueOf x = NumericTraits.minValue(Unchecked.defaultof<NumericTraits>, x)

/// The maximum value of the target type of the conversion function.
/// The conversion function typically has the same name as the type.
let inline maxValue (x : _ -> 'a) = NumericTraits.maxValue(Unchecked.defaultof<NumericTraits>, Unchecked.defaultof<'a>)

/// The maximum value of the argument type.
let inline maxValueOf x = NumericTraits.maxValue(Unchecked.defaultof<NumericTraits>, x)



/// Raises an exception if the condition is false.
let inline enforce condition errorMessage = if condition = false then failwith errorMessage

/// Raises an exception if the condition is false. The error message accepts printf style formatting.
let inline enforcef condition errorFormat =
  Printf.kprintf (fun errorMessage -> if condition = false then failwith errorMessage) errorFormat



// Some generic active patterns.

/// Equal to active pattern.
let (|Eq|_|) y x = if x = y then Some() else None

/// Greater than active pattern.
let (|Gr|_|) y x = if x > y then Some() else None

/// Greater than or equal active pattern.
let (|GrEq|_|) y x = if x >= y then Some() else None

/// Lesser than active pattern.
let (|Ls|_|) y x = if x < y then Some() else None

/// Lesser than or equal active pattern.
let (|LsEq|_|) y x = if x <= y then Some() else None



/// The constant function.
let inline always x _ = x

/// Inverts argument order: flip f x y = f y x.
let inline flip f x y = f y x

/// Produces a curried form from a function taking a pair as input.
let inline curry f x y = f (x, y)

/// Produces a tupled form from a function with two arguments.
let inline uncurry f (x, y) = f x y

/// Reverses a pair.
let inline rev (a, b) = b, a

/// The first element of a triple.
let inline fst3 (a, b, c) = a

/// The second element of a triple.
let inline snd3 (a, b, c) = b

/// The third element of a triple.
let inline thd3 (a, b, c) = c

/// Minimum of zero and the argument.
let inline min0 x = min 0G x

/// Maximum of zero and the argument.
let inline max0 x = max 0G x

/// Clamps x such that a <= x <= b.
let inline clamp a b x = max a (min b x)

/// Clamps x such that 0 <= x <= 1.
let inline clamp01 x = max 0G (min 1G x)

/// Clamps x such that -1 <= x <= 1.
let inline clamp11 x = max -1G (min 1G x)

/// Remaps the range [-1, 1] to [0, 1]. Preserves interval type.
let inline map11to01 (x : 'a) : 'a = (x + 1G) * Q 1 2

/// Remaps the range [0, 1] to [-1, 1]. Preserves interval type.
let inline map01to11 (x : 'a) : 'a = x * 2G - 1G

/// Minimum function for 3 arguments.
let inline min3 a b c = min a (min b c)

/// Maximum function for 3 arguments.
let inline max3 a b c = max a (max b c)

/// Minimum function for 4 arguments.
let inline min4 a b c d = min (min a b) (min c d)

/// Maximum function for 4 arguments.
let inline max4 a b c d = max (max a b) (max c d)

/// Maximum of zero and the argument for signed integers. A special version using bit manipulation tricks.
let inline maxi0 x = forceSigned x; x &&& ~~~(x >>> -1)

/// Minimum of zero and the argument for signed integers. A special version using bit manipulation tricks.
let inline mini0 x = forceSigned x; x &&& (x >>> -1)

/// Maximum function for signed integers. A special version using bit manipulation tricks.
/// Has limited numerical range: requires that a - b is representable.
let inline maxi a b = forceSigned a; maxi0 (a - b) + b

/// Minimum function for signed integers. A special version using bit manipulation tricks.
/// Has limited numerical range: requires that a - b is representable.
let inline mini a b = forceSigned a; mini0 (a - b) + b

/// Clamps signed integer x to [a, b] (a <= b). A special version using bit manipulation tricks.
/// Has limited numerical range: requires that a - b, a - x and b - x are representable.
let inline clampi a b x = forceSigned a; maxi a (mini b x)

/// Sign function for signed integers that uses bit manipulation tricks.
let inline signi x = forceSigned x; ((-x >>> -1) &&& 1G) + (x >>> -1)

/// Absolute function for signed integers. A special version using bit manipulation tricks.
/// The smallest signed value cannot be negated; the standard function raises an exception,
/// whereas we return it unchanged.
let inline absi x = forceSigned x; let y = x >>> -1 in (x &&& ~~~y) - (x &&& y)

/// Negative indicator function for signed integers. Returns 1 if x < 0, and 0 otherwise.
let inline negativei x = forceSigned x; (x >>> -1) &&& 1G

/// Positive indicator function for signed integers. Returns 1 if x > 0, and 0 otherwise.
let inline positivei x = forceSigned x; (-x >>> -1) &&& 1G

/// Non-zero indicator function for signed integers. Returns 0 if x = 0, and 1 otherwise.
let inline nonzeroi x = forceSigned x; ((x ||| -x) >>> -1) &&& 1G

/// Protected integer division. Returns 0 if the denominator is zero or if the numerator is the minimum of its type.
/// The latter is necessary to guard against exceptions when dividing the smalled signed value.
let inline pdiv x y = forceIntegral x; if x = minValueOf x || y = 0G then 0G else x / y

/// Square function.
let inline squared x = x * x

/// Cube function.
let inline cubed x = x * x * x

/// Cubic root.
let inline cbrt x = x ** Q 1 3

/// Linear interpolation. Interpolates between a (when x = 0) and b (when x = 1).
// Note: we like this formulation better than a + x * (b - a) because, for floating point types,
// it is guaranteed to evaluate to b when x = 1.
let inline lerp (a : 'a) (b : 'a) x : 'a = (1G - x) * a + x * b

/// Clamped linear interpolation.
let inline lerp01 a b x = lerp a b (clamp01 x)

/// Exponential interpolation. a, b > 0.
let inline xerp a b x = exp(lerp (log a) (log b) x)

/// Clamped exponential interpolation. a, b > 0.
let inline xerp01 a b x = xerp a b (clamp01 x)

/// Recovers the linear interpolation amount from the interpolated value. b <> a.
let inline delerp a b v = (v - a) / (b - a)

/// Recovers a linear interpolation amount and clamps it to the unit interval. b <> a.
let inline delerp01 a b v = clamp01 (delerp a b v)

/// Recovers the exponential interpolation amount from the interpolated value. a, b, v > 0.
let inline dexerp a b v = log(v / a) / log(b / a)

/// Recovers the exponential interpolation amount from the interpolated value and clamps it to the unit interval.
/// a, b, v > 0.
let inline dexerp01 a b v = clamp01 (dexerp a b v)

/// Catmull-Rom cubic spline interpolation, which is a form of cubic Hermite spline. Interpolates between
/// p1 (returns p1 when x = 0) and p2 (returns p2 when x = 1) while using the previous (p0) and next (p3)
/// points to define slopes at the endpoints. The maximum overshoot is 1/8th of the range of the arguments.
let inline cubicSpline (p0 : 'a) (p1 : 'a) (p2 : 'a) (p3 : 'a) (x : 'a) : 'a =
  p1 + Q 1 2 * x * (p2 - p0 + x * (2G * p0 - 5G * p1 + 4G * p2 - p3 + x * (3G * (p1 - p2) + p3 - p0)))

/// Signed integer Euclidean modulo function, which returns only positive values
/// (operator % returns negative values as well). b > 0.
let inline emod a b =
  assert (b > 0G)
  forceSigned a
  let m = a % b
  m + ((m >>> -1) &&& b)

/// Floating point Euclidean modulo function, which returns only positive values
/// (operator % returns negative values as well). b > 0.
let inline emodf a b =
  assert (b > 0G)
  let c = a / b
  (c - floor c) * b

/// Returns the remainder when x is rounded down, which equals the fractional part for positive numbers.
/// Same as Euclidean modulo of unity.
let inline fract x = x - floor x

/// Binary logarithm.
let inline log2 x = log x * Q 1 ln2

/// Logarithm to an arbitrary base (b > 1).
let inline logb b =
  assert (b > 1G)
  let Z = 1G / log b in fun x -> Z * log x

/// Binary exponential.
let inline exp2 x = exp(x * G ln2)

/// Base 10 exponential.
let inline exp10 x = exp(x * G ln10)

/// 7th order Taylor approximation of the exp function at the origin. expTaylor(-2.759) > 0, expTaylor(-2.7591) < 0.
let inline expTaylor (x : 'a) : 'a =
  1G + x * (1G + Q 1 2 * x * (1G + Q 1 3 * x * (1G + Q 1 4 * x * (1G + Q 1 5 * x * (1G + Q 1 6 * x * (1G + Q 1 7 * x))))))

/// 7th order Taylor approximation of the sine function at the origin. Quite good accuracy in [-pi/2, pi/2].
let inline sinTaylor (x : 'a) : 'a =
  let x2 = squared x
  x * (1G - x2 * Q 1 6 * (1G - x2 * Q 1 20 * (1G - x2 * Q 1 42)))

/// Periodic sin function approximation based on Common.sinTaylor. ~4 times faster than sin.
let inline sinFast (x : 'a) : 'a =
  let phi = x - G pi12
  let k = floor (phi * Q 1 tau)
  let a = phi - k * G tau
  sinTaylor (abs (a - G pi) - G pi12)

/// Periodic sin function approximation based on Common.sinTaylor with a period of unity.
let inline sinrFast (x : 'a) : 'a = sinFast(x * G tau)

/// Periodic cos function approximation based on Common.sinTaylor. ~4 times faster than cos.
let inline cosFast (x : 'a) : 'a =
  let k = floor (x * Q 1 tau)
  let a = x - k * G tau
  sinTaylor (abs (a - G pi) - G pi12)

/// Periodic cos approximation based on Common.sinTaylor with a period of unity.
let inline cosrFast (x : 'a) : 'a = cosFast(x * G tau)

/// Sine function scaled to [0, 1].
let inline sin0 (x : 'a) : 'a = (sin x + 1G) * Q 1 2

/// Sine function with a period of unity. The argument is thus phase in revolutions.
let inline sinr (x : 'a) : 'a = sin(x * G tau)

/// Sine function scaled to [0, 1] with a period of unity. The argument is thus phase in revolutions.
let inline sinr0 (x : 'a) : 'a = (sinr x + 1G) * Q 1 2

/// Cosine function scaled to [0, 1].
let inline cos0 (x : 'a) : 'a = (cos x + 1G) * Q 1 2

/// Cosine function with a period of unity. The argument is thus phase in revolutions.
let inline cosr (x : 'a) : 'a = cos(x * G tau)

/// Cosine function scaled to [0, 1] with a period of unity. The argument is thus phase in revolutions.
let inline cosr0 (x : 'a) : 'a = (cosr x + 1G) * Q 1 2

/// Triangle wave. The argument is phase in radians. The shape approximates the sine function.
let inline tri (x : 'a) : 'a =
  let x = fract(x * Q 1 tau - Q 1 4) in abs (x - Q 1 2) * 4G - 1G

/// Triangle wave with a period of unity. The shape approximates the sine function.
let inline trir (x : 'a) : 'a = tri(x * G tau)

/// Square wave (non-bandlimited) scaled to [0, 1] with a period of unity. The argument is thus phase in revolutions.
let inline sqrr0 (x : 'a) : 'a = floor (fract (x + G(0.25)) * 2G)

/// Square wave (non-bandlimited) with a period of unity. The argument is thus phase in revolutions.
/// The shape approximates the sine function.
let inline sqrr (x : 'a) : 'a = sqrr0 x * 2G - 1G

/// The average of the two arguments.
let inline average (x : 'a) (y : 'a) : 'a = (x + y) * Q 1 2

/// The average of the three arguments.
let inline average3 (x : 'a) (y : 'a) (z : 'a) : 'a = (x + y + z) * Q 1 3

/// Antisymmetric pow function. Raises the absolute of the value (x) to the given power (y) and then restores the sign.
let inline apow x y = if x < 0G then -(-x ** y) else x ** y

/// The logistic function. Fades smoothly between 0 and 1. The tails converge exponentially.
/// logistic(-oo) = 0.0, logistic(0) = 0.5, logistic(oo) = 1.0.
let inline logistic (x : 'a) : 'a = 1G / (1G + exp(-x))

/// Derivative of the logistic function.
let inline logisticd (x : 'a) : 'a = let y = logistic x in y * (1G - y)

/// The softsign function. Fades smoothly between -1 and 1. Resembles tanh. The tails converge polynomially.
let inline softsign (x : 'a) : 'a = x / (1G + abs x)

/// Derivative of the softsign function.
let inline softsignd (x : 'a) : 'a = 1G / squared(1G + abs x)

/// Rounds (non-negative) x up to a multiple of n. For integral types. x >= 0, n > 0.
let inline ceilmod (n : 'a) (x : 'a) : 'a = forceIntegral x; ((x + n - 1G) / n) * n

/// Truncates x toward zero to a multiple of n. For integral types.
let inline truncmod (n : 'a) (x : 'a) : 'a = forceIntegral x; (x / n) * n

/// Kronecker delta function. Returns 1 if the arguments are equal and 0 otherwise.
let inline kronecker a b = if a = b then 1G else 0G

/// If-then-else as a function.
let inline ifThenElse condition thenValue elseValue = if condition then thenValue else elseValue

/// Not-a-number values can be troublesome because they propagate in arithmetic operations.
/// This function returns whether x is NaN.
let inline isNaN x = x <> x

/// Returns whether a floating point number is finite, that is, not NaN
/// (which is neither finite nor non-finite) nor either of the infinities.
let inline isFinite x = G -infinity < x && x < G infinity

/// Exponentiation a ** b to a non-negative integer power in O(log b). Always returns 1 if b = 0.
/// This is much faster than the standard pown function, apparently because the latter is generic.
let inline pow (a : 'a) b : 'a =
  assert (b >= 0)
  let mutable a, b, x = a, b, 1G
  while b > 0 do
    if b &&& 1 = 1 then x <- x * a
    a <- a * a
    b <- b >>> 1
  x


/// Linear congruential generator MINSTD. Cycles through all int values > 0 if the initial seed is > 0.
/// Thus, its period is 2^31 - 1.
let minstd (n : int) = int <| (int64 n * 48271L) % 2147483647L

/// Linear congruential generator sourced from Numerical Recipes. Cycles through all uint values.
/// Thus, its period is 2^32.
let minstdu (n : uint) = n * 1664525u + 1013904223u

/// Linear congruential generator by Donald Knuth. Cycles through all uint64 values. Thus, its period is 2^64.
let minstd64u (n : uint64) = n * 6364136223846793005UL + 1442695040888963407UL

/// 32-bit Xorshift generator suggested by George Marsaglia.
/// Cycles through all uint values > 0 if the initial seed is > 0. Thus, its period is 2^32 - 1.
let xorshift32u (x : uint) =
  let x = x ^^^ (x <<< 13)
  let x = x ^^^ (x >>> 17)
  x ^^^ (x <<< 5)

/// 64-bit Xorshift generator suggested by George Marsaglia.
/// Cycles through all uint64 values > 0 if the initial seed is > 0. Thus, its period is 2^64 - 1.
let xorshift64u (x : uint64) =
  let x = x ^^^ (x <<< 13)
  let x = x ^^^ (x >>> 7)
  x ^^^ (x <<< 17)


/// Encodes x in [0, 63[ to a Base64 style byte character.
let encodeBase64 x =
  "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-+"B.[x]

/// Decodes a Base64 style byte character from Common.encodeBase64 to an int in [0, 63[.
let decodeBase64 (x : byte) =
  if x >= '0'B && x <= '9'B then int (x - '0'B)
  elif x >= 'a'B && x <= 'z'B then int (x - 'a'B) + 10
  elif x >= 'A'B && x <= 'Z'B then int (x - 'A'B) + 36
  elif x = '-'B then 62
  elif x = '+'B then 63
  else failwith "Common.decodeBase64: Invalid character."


/// Dictionary order comparison between (a1, b1) and (a2, b2). This can be faster than
/// constructing tuples because tuples are a reference type.
let inline compare2 a1 b1 a2 b2 = let c = compare a1 a2 in if c <> 0 then c else compare b1 b2
  
/// A null reference.
let inline nullRef<'a when 'a : not struct> = Unchecked.defaultof<'a>

/// A null value. Works with all types, producing a zero initialized value or a null reference.
let inline nullValue<'a> = Unchecked.defaultof<'a>



/// Sends a task to the .NET thread pool for execution.
let inline scheduleTask (f : unit -> unit) =
  System.Threading.ThreadPool.QueueUserWorkItem(System.Threading.WaitCallback(fun _ -> f())) |> ignore

/// Sends a function call with a single argument to the .NET thread pool for execution.
let inline scheduleCall (f : 'a -> unit) (argument : 'a) =
  System.Threading.ThreadPool.QueueUserWorkItem(System.Threading.WaitCallback(fun _ -> f argument)) |> ignore

/// Popular alias for MailboxProcessor.
type Agent<'T> = MailboxProcessor<'T>

type MailboxProcessor<'T> with
  member inline this.messages = this.CurrentQueueLength
  member inline this.receive() = this.Receive()
  member inline this.receive(timeout) = this.Receive(timeout)
  member inline this.post(msg) = this.Post(msg)



/// Returns the label of a discriminated union case. Throws an exception if the argument is not
/// a discriminated union value.
let unionLabel (x : 'a) =
  match FSharp.Reflection.FSharpValue.GetUnionFields(x, typeof<'a>) with
  | case, _ -> case.Name

/// Creates a (parameterless) discriminated union case from its label or returns None if there is no such case.
let createUnionFromLabel (unionType : System.Type) (label : string) =
  FSharp.Reflection.FSharpType.GetUnionCases unionType
  |> Array.tryFind (fun case -> case.Name = label)
  |> Option.map (fun case -> FSharp.Reflection.FSharpValue.MakeUnion(case, [||]))



/// Regular expression active pattern. Captures matched groups into a list.
/// Note that .NET maintain a cache of compiled regular expressions that accelerates use cases like this.
let (|Regex|_|) pattern input =
  let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
  if m.Success then Some(List.tail [for group in m.Groups -> group.Value]) else None



/// An IDisposable object expression for when you have a task that needs to be executed when control leaves
/// local scope - even if an exception is raised. Example: use __ = usable (fun _ -> importantCleanup()).
let usable (task : _ -> unit) = { new IDisposable with member this.Dispose() = task() }



// Helper class for the signum function.
type Signum =
  static member inline Signum (x : int8,    _ : Signum) = signi x
  static member inline Signum (x : int16,   _ : Signum) = signi x
  static member inline Signum (x : int32,   _ : Signum) = signi x
  static member inline Signum (x : int64,   _ : Signum) = signi x
  static member inline Signum (x : float32, _ : Signum) = float32 (sign x)
  static member inline Signum (x : float,   _ : Signum) = float (sign x)
  static member inline Signum (x : uint8,   _ : Signum) = uint8 (nonzeroi (int x))
  static member inline Signum (x : uint16,  _ : Signum) = uint16 (nonzeroi (int x))
  static member inline Signum (x : uint32,  _ : Signum) = uint32 (nonzeroi (int x))
  static member inline Signum (x : uint64,  _ : Signum) = uint64 (nonzeroi (int64 x))

  static member inline Invoke (x : 'Num) : 'Num =
    let inline invoke(a : ^a, b : ^b) = ((^a or ^b) : (static member Signum : _ * _ -> _) (b, a))
    invoke(Unchecked.defaultof<Signum>, x)

/// Generic sign function for inbuilt types that returns the sign of the argument as the same type.
let inline signum x = Signum.Invoke x



/// An empty structure.
[<NoEquality; NoComparison; Struct>]
type EmptyStruct = struct end



/// Pair as a value type. Use this in lieu of a tuple when speed is essential.
type Pair<'a, 'b> = struct
  val a : 'a
  val b : 'b
  new(a, b) = { a = a; b = b }
  member inline this.tuple = (this.a, this.b)
  member inline this.fst = this.a
  member inline this.snd = this.b
  member inline this.x = this.a
  member inline this.y = this.b
  override this.ToString() = sprintf "(%A, %A)" this.a this.b
end

/// Active pattern that deconstructs pairs. Does not incur memory allocation.
let inline (|Pair|) (pair : Pair<_, _>) = (pair.a, pair.b)



/// Associative pair as a value type. Equality and comparison operators are defined
/// via the first element only, which must support them.
[<CustomEquality; CustomComparison>]
type Apair<'a, 'b> when 'a : comparison and 'a :> IEquatable<'a> = struct
  val a : 'a
  val b : 'b
  new(a, b) = { a = a; b = b }
  member inline this.tuple = (this.a, this.b)
  member inline this.fst = this.a
  member inline this.snd = this.b
  member inline this.x = this.a
  member inline this.y = this.b
  override this.ToString() = sprintf "(%A, %A)" this.a this.b
  override this.Equals(that) =
    match that with
    | :? Apair<'a, 'b> as that -> this.a = that.a
    | _ -> false
  override this.GetHashCode() = hash this.a
  interface System.IComparable with
    member this.CompareTo(that) =
        match that with
        | :? Apair<'a, 'b> as that -> compare this.a that.a
        | _ -> failwith "Apair.compare: Cannot compare dissimilar types."
  interface System.IEquatable<Apair<'a, 'b>> with
    member this.Equals(that : Apair<'a, 'b>) = this.a = that.a
end



/// Triple as a value type.
type Triple<'a, 'b, 'c> = struct
  val a : 'a
  val b : 'b
  val c : 'c
  new(a, b, c) = { a = a; b = b; c = c }
  member inline this.tuple = (this.a, this.b, this.c)
  member inline this.fst = this.a
  member inline this.snd = this.b
  member inline this.trd = this.c
  member inline this.x = this.a
  member inline this.y = this.b
  member inline this.z = this.c
  override this.ToString() = sprintf "(%A, %A, %A)" this.a this.b this.c
end

/// Active pattern that deconstructs triples. Does not incur memory allocation.
let inline (|Triple|) (triple : Triple<_, _, _>) = (triple.a, triple.b, triple.c)



/// Quadruplet as a value type.
type Quad<'a, 'b, 'c, 'd> = struct
  val a : 'a
  val b : 'b
  val c : 'c
  val d : 'd
  new(a, b, c, d) = { a = a; b = b; c = c; d = d }
  member inline this.tuple = (this.a, this.b, this.c, this.d)
  member inline this.fst = this.a
  member inline this.snd = this.b
  member inline this.trd = this.c
  member inline this.fth = this.d
  override this.ToString() = sprintf "(%A, %A, %A, %A)" this.a this.b this.c this.d
end

/// Active pattern that deconstructs quadruplets. Does not incur memory allocation.
let inline (|Quad|) (quad : Quad<_, _, _, _>) = (quad.a, quad.b, quad.c, quad.d)



/// Creates a None option of the specified type.
let inline none<'a> : 'a option = None



// Some option extensions.
type Option<'T> with
  member inline this.value = this.Value
  member inline this.isSome = this.IsSome
  member inline this.isNone = this.IsNone
  /// Applies a function to the value of the option, if it exists.
  member inline this.apply(f : 'T -> unit) = if this.isSome then f this.value
  /// Transforms the option with the given value map.
  member inline this.map(f : 'T -> 'U) = match this with | Some(v) -> Some(f v) | None -> None
  /// Filters option values with a predicate, transforming to None those that fail it.
  member inline this.filter(predicate : 'T -> bool) =
    match this with
    | Some(v) -> if predicate v then this else None
    | None -> None
  /// Returns true iff the option has a value that matches the predicate.
  member inline this.isSomeAnd(predicate : 'T -> bool) = this.isSome && predicate !this
  /// Returns true iff the option has no value or if the value matches the predicate.
  member inline this.isNoneOr(predicate : 'T -> bool) = this.isNone || predicate !this



/// Returns current UTC time in seconds AD with 0.1 ms resolution. The supposed accuracy is 10 ms.
/// This call is fast on PC - less than 30 ns on an Ivy Bridge.
let timeNow() = 1.0e-7 * float System.DateTime.UtcNow.Ticks

/// Returns an int seed from the current time.
let timeSeed() = let t = System.DateTime.UtcNow.Ticks in int (t >>> 32) ^^^ int t



/// A counter iterates a function on a stored value.
/// The Atom module contains a lock-free version of this as Atom.SharedCounter.
[<ReferenceEquality>]
type Counter<'a> =
  {
    mutable value : 'a
    f : 'a -> 'a
  }
  // Enable use of the ! operator.
  member inline this.Value = this.value
  /// Returns the current value while iterating to the next value.
  member inline this.tick =
    let v = this.value
    this.value <- this.f this.value
    v

/// Creates an incrementing counter.
let inline createCounter value0 = { Counter.value = value0; f = (+) 1G }

/// Creates a counter with an arbitrary function.
let createIterate value0 f = { Counter.value = value0; f = f }

/// Creates a MINSTD linear congruential generator (seed > 0) that cycles through the positive ints.
let createLcg seed =
  enforce (seed > 0) "Common.createLcg: Seed must be greater than zero."
  { Counter.value = seed |> minstd |> minstd; f = minstd }


/// Converts a number to normalized scientific notation as (mantissa, exponent).
/// Note that comparison operations on results do not obey arithmetic ordering -
/// proper arithmetic ordering would require a separate sign field (or a mixed sign radix).
let inline scientific x =
  if x = 0.0 then Pair(0.0, 0.0) else let e = floor(log10 (abs x)) in Pair(x / exp10 e, e)



/// Calls the (stateful) generator until the return value fulfills the predicate. Returns the accepted value.
let inline doFind (generator : unit -> 'a) (predicate : 'a -> bool) =
  let rec loop() = let v = generator() in if predicate v then v else loop()
  loop()


/// A do .. while control structure. Keeps calling the body as long as it fulfills the condition.
let inline doWhile (body : unit -> _) (condition : _ -> bool) =
  let mutable loop = true
  while loop do
    loop <- body() |> condition



// Lazy extensions.
type Lazy<'T> with
  member inline this.value = this.Value



// List extensions.
type List<'a> with
  /// The size of the list.
  member inline this.size = this.Length
  member inline this.isEmpty = this.IsEmpty
  member inline this.head = this.Head
  member inline this.tail = this.Tail



// String extensions.
type String with
  /// The length of the string.
  member inline this.size = this.Length
  /// The index of the last character in the string.
  member inline this.last = this.Length - 1



// Array type extensions.
type ``[]``<'a> with
  /// The size of the array.
  member inline a.size = a.Length
  /// The index of the last item in the array.
  member inline a.last = a.Length - 1
  /// Fills the array with items from an indexed function.
  member inline a.fill(f : int -> _) = for i = 0 to a.last do a.[i] <- f i
  /// Fills the array with a constant value.
  member inline a.fill(x) = Array.fill a 0 a.size x
  /// Swaps two array items.
  member inline a.swap(i, j) = let tmp = a.[i] in a.[i] <- a.[j] ; a.[j] <- tmp
  /// Modifies array items with a function, performing an in-place map operation.
  member inline a.modify(f) = for i = 0 to a.last do a.[i] <- f a.[i]
  /// Scrubs the whole array with the default value of the item type.
  member inline a.scrub() = Array.fill a 0 a.size nullValue
  /// Item access.
  member inline a.at(i) = a.[i]



/// Type that enables sharing of empty arrays.
type EmptyArray<'a>() =
  static member val item : 'a[] = [| |]


// Array module extensions.
module Array =
  /// Creates an empty array.
  let inline createEmpty<'a> = EmptyArray<'a>.item
  /// Fills the whole array with a value.
  let inline setAll v (a : 'a array) = Array.fill a 0 a.Length v
  /// Scrubs the whole array with the default value of the item type.
  let inline scrub (a : 'a array) = setAll Unchecked.defaultof<'a> a
  /// Modifies array items with a function, performing an in-place map operation.
  let inline modify f (a : 'a[]) = for i = 0 to a.last do a.[i] <- f a.[i]
  /// Swaps two array items.
  let inline swap i j (a : 'a array) = a.swap(i, j)
  /// Creates a singleton array.
  let inline createSingle x = [| x |]
  /// Reduces the array using the given projection and binary operator.
  let inline reduceBy projection binop (a : 'a[]) =
    enforce (a.size > 0) "Array.reduceBy: Empty array."
    let mutable x = projection a.[0]
    for i = 1 to a.last do x <- binop x (projection a.[i])
    x



type System.Type with 

  /// Returns a nicely formatted name of the type. From an F# snippet by Tomas Petricek.
  member this.niceName =
    let builder = System.Text.StringBuilder()
    let rec build (t : System.Type) =
      if t.IsGenericType then 
        // Remove the `1 part from generic names.
        let tick = t.Name.IndexOf('`')
        let name = t.Name.Substring(0, tick) 
        Printf.bprintf builder "%s" t.Name
        Printf.bprintf builder "<"
        // Print generic type arguments recursively.
        let args = t.GetGenericArguments()
        for i = 0 to args.last do
          if i <> 0 then Printf.bprintf builder ", "
          build args.[i]
        Printf.bprintf builder ">"
      else
        // Print ordinary type name.
        Printf.bprintf builder "%s" t.Name
    build this
    builder.ToString()

