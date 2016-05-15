/// This central module contains common definitions.
module FsMap3.Common

open System
open LanguagePrimitives


/// Type alias for uint32.
type uint = uint32

/// Converts the argument to a uint32.
let inline uint x = uint32 x



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



/// Numeric interval type definition. Most often, the "default" interval type is closed or left-closed
/// for floating point intervals and closed for integer intervals.
[<NoComparison>]
type Interval = Closed | LeftClosed | RightClosed | Open



// Global operator overloads.

/// Reference equality operator. Returns false if either a or b is not a reference type.
let inline ( === ) a b = obj.ReferenceEquals(a, b)

/// Reference inequality operator. Returns true if either a or b is not a reference type.
let inline ( <>= ) a b = obj.ReferenceEquals(a, b) = false

/// Dot product operator. We declare this as a global, duck typed operator to make it compatible with
/// SIMD accelerated types from System.Numerics.
let inline ( *. ) (v : ^a) (u : ^a) = (^a : (static member Dot : ^a * ^a -> _) (v, u))

/// Cross product operator. We declare this as a global, duck typed operator to make it compatible with
/// SIMD accelerated types from System.Numerics.
let inline ( *+ ) (v : ^a) (u : ^a) = (^a : (static member Cross : ^a * ^a -> _) (v, u))

/// Duck typed dereferencing operator. Duck typing enables us to use it with other types besides reference cells.
let inline ( ! ) x = (^X : (member get_Value : unit -> _) (x))

/// Convenience duck typed infix operator that returns the value of option (or Optionval) a,
/// defaulting to b if the option does not have a value. Note that the default value b is always evaluated.
let inline ( >? ) (a : ^a) (b : ^b) = if (^a : (member get_IsSome : unit -> bool) (a)) then !a else b



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

/// Generic conversion of quotients x / y. This is equal to G x / G y.
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



/// Stops the program if the condition is false.
let inline enforce condition errorMessage = if condition = false then failwith errorMessage

/// Stops the program if the condition is false. The error message accepts printf style formatting.
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
let inline clamp01 x = max GenericZero (min GenericOne x)

/// Clamps x such that -1 <= x <= 1.
let inline clamp11 x = max -GenericOne (min GenericOne x)

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
let inline signi x = forceSigned x; ((-x >>> -1) &&& GenericOne) + (x >>> -1)

/// Absolute function for signed integers. A special version using bit manipulation tricks.
/// The smallest signed value cannot be negated; the standard function raises an exception,
/// whereas we return it unchanged.
let inline absi x = forceSigned x; let y = x >>> -1 in (x &&& ~~~y) - (x &&& y)

/// Negative indicator function for signed integers. Returns 1 if x < 0, and 0 otherwise.
let inline negativei x = forceSigned x; (x >>> -1) &&& GenericOne

/// Positive indicator function for signed integers. Returns 1 if x > 0, and 0 otherwise.
let inline positivei x = forceSigned x; (-x >>> -1) &&& GenericOne

/// Non-zero indicator function for signed integers. Returns 0 if x = 0, and 1 otherwise.
let inline nonzeroi x = forceSigned x; ((x ||| -x) >>> -1) &&& GenericOne

/// Square function.
let inline squared x = x * x

/// Cube function.
let inline cubed x = x * x * x

/// Cubic root.
let inline cbrt x = x ** Q 1 3

/// Linear interpolation. Interpolates between a (when x = 0) and b (when x = 1).
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
  assert (b > GenericZero)
  forceSigned a
  let m = a % b
  m + ((m >>> -1) &&& b)

/// Floating point Euclidean modulo function, which returns only positive values
/// (operator % returns negative values as well). b > 0.
let inline emodf a b =
  assert (b > GenericZero)
  let c = a / b
  (c - floor c) * b

/// Returns the remainder when x is rounded down, which equals the fractional part for positive numbers.
/// Same as Euclidean modulo of unity.
let inline fract x = x - floor x

/// Binary logarithm.
let inline log2 x = log x * Q 1 ln2

/// Logarithm to an arbitrary base (b > 1).
let inline logb b = let Z = 1G / log b in fun x -> Z * log x

/// Binary exponential.
let inline exp2 x = exp(x * G ln2)

/// Base 10 exponential.
let inline exp10 x = exp(x * G ln10)

/// Inverse hyperbolic sine.
let inline asinh (x : 'a) : 'a = log (x + sqrt (1G + squared x))

/// Inverse hyperbolic cosine.
let inline acosh (x : 'a) : 'a = log (x + (x + 1G) * sqrt ((x - 1G) / (x + 1G)))

/// Inverse hyperbolic tangent.
let inline atanh (x : 'a) : 'a = Q 1 2 * log ((1G + x) / (1G - x))

/// 7th order Taylor approximation of the exp function at the origin. expTaylor(-2.759) > 0, expTaylor(-2.7591) < 0.
let inline expTaylor (x : 'a) : 'a =
  1G + x * (1G + Q 1 2 * x * (1G + Q 1 3 * x * (1G + Q 1 4 * x * (1G + Q 1 5 * x * (1G + Q 1 6 * x * (1G + Q 1 7 * x))))))

/// 7th order Taylor approximation of the sine function at the origin. Quite good accuracy in [-pi/2, pi/2].
let inline sinTaylor (x : 'a) : 'a =
  let x2 = squared x
  x * (1G - x2 * Q 1 6 * (1G - x2 * Q 1 20 * (1G - x2 * Q 1 42)))

/// Periodic sin approximation based on Common.sinTaylor. ~4 times faster than sin.
let inline sinFast (x : 'a) : 'a =
  let phi = x - G pi12
  let k = floor (phi * Q 1 tau)
  let a = phi - k * G tau
  sinTaylor (abs (a - G pi) - G pi12)

/// Periodic sin approximation based on Common.sinTaylor with a period of unity.
let inline sinrFast (x : 'a) : 'a = sinFast(x * G tau)

/// Periodic cos approximation based on Common.sinTaylor. ~4 times faster than cos.
let inline cosFast (x : 'a) : 'a =
  let k = floor (x * Q 1 tau)
  let a = x - k * G tau
  sinTaylor (abs (a - G pi) - G pi12)

/// Periodic cos approximation based on Common.sinTaylor with a period of unity.
let inline cosrFast (x : 'a) : 'a = cosFast(x * G tau)

/// Sine function scaled to [0, 1].
let inline sin0 (x : 'a) : 'a = (sin x + 1G) * Q 1 2

/// Sine function with a period of unity. The argument is thus phase in cycles.
let inline sinr (x : 'a) : 'a = sin(x * G tau)

/// Sine function scaled to [0, 1] with a period of unity. The argument is thus phase in cycles.
let inline sinr0 (x : 'a) : 'a = (sinr x + 1G) * Q 1 2

/// Cosine function scaled to [0, 1].
let inline cos0 (x : 'a) : 'a = (cos x + 1G) * Q 1 2

/// Cosine function with a period of unity. The argument is thus phase in cycles.
let inline cosr (x : 'a) : 'a = cos(x * G tau)

/// Cosine function scaled to [0, 1] with a period of unity. The argument is thus phase in cycles.
let inline cosr0 (x : 'a) : 'a = (cosr x + 1G) * Q 1 2

/// Triangle wave. The argument is phase in radians.
/// The shape approximates the sine function.
let inline tri (x : 'a) : 'a =
  let x = fract(x * Q 1 tau - Q 1 4) in abs (x - Q 1 2) * 4G - 1G

/// Triangle wave with a period of unity. The shape approximates the sine function.
let inline trir (x : 'a) : 'a = tri(x * G tau)

/// The average of the two arguments.
let inline average (x : 'a) (y : 'a) : 'a = (x + y) * Q 1 2

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
let inline ceilmod (x : 'a) (n : 'a) : 'a = forceIntegral x; ((x + n - 1G) / n) * n

/// Truncates x toward zero to a multiple of n. For integral types.
let inline truncmod (x : 'a) (n : 'a) : 'a = forceIntegral x; (x / n) * n

/// This quadratic response function has a second order discontinuity at the origin but
/// has the property f(x) = 1.0 / f(-x). f(x) > 0 for all x. Like the exponential function, f(0) = f'(0) = 1.
let inline qesp (x : 'a) : 'a = if x < 0G then 1G / (1G - x + squared x) else 1G + x + squared x

/// This smooth response function is second order continuous. It has asymmetrical magnitude curves:
/// linear when x < 0 and quadratic when x > 0. f(x) > 0 for all x. Like the exponential function, f(0) = f'(0) = 1.
let inline esp (x : 'a) : 'a = if x < 0G then 1G / (1G - x) else 1G + x + squared x

/// Kronecker delta function. Returns 1 if the arguments are equal and 0 otherwise.
let inline kronecker a b = if a = b then 1G else 0G

/// Not-a-number values can be troublesome because they propagate in arithmetic operations.
/// This function returns whether x is NaN.
let inline isNaN x = x <> x

/// Returns whether a floating point number is finite, that is, not NaN
/// (which is neither finite nor non-finite) nor either of the infinities.
let inline isFinite x = G -infinity < x && x < G infinity

/// An accurate logarithm of the gamma function (n! ~ exp(lgamma(n + 1))).
let lgamma x =
  let i = 1.0 / x
  let i3 = cubed i
  (x - 0.5) * log x - x + 0.918938533204673 + 0.0833333333333333333 * i - (0.002777777777777777 - 0.000793650793650794 * i * i + 0.000595238095238095 * i3 * i) * i3

/// Logarithm of the complete beta function.
let lbeta x y = lgamma x + lgamma y - lgamma (x + y)

/// Logarithm of the binomial coefficient (choose n k).
let lchoose n k = lgamma (float (n + 1)) - lgamma (float (k + 1)) - lgamma (float (n - k + 1))

/// Probability mass function of the binomial distribution B(n, p), with value i in [0, n].
let binomialPmf n p i =
  if p <= 0.0 then kronecker i 0
  elif p >= 1.0 then kronecker i n
  else lchoose n i + float i * log p + float (n - i) * log (1.0 - p) |> exp

/// Gaussian CDF approximation by W Bryc (2002). Returns probability that the standard normal distribution has value <= x.
/// The largest absolute error is 0.000019. The largest relative error, 0.5%, occurs in the tail, when abs x > 11.8.
let gaussianCdf x =
  let inline cdf z = (z * z + 5.575192695 * z + 12.77436324) / (2.506628274631 * z * z * z + 14.38718147 * z * z + 31.53531977 * z + 25.54872648) * exp(-0.5 * z * z)
  if x > 0.0 then 1.0 - cdf x else cdf -x

/// Binomial coefficient: (choose n k), where n >= k, is the number of k-element subsets in an n-element set.
let choose n k =
  let k = if k < n / 2 then k else n - k
  // Use int64 for a bit of headroom.
  let mutable result = 1L
  for i = 1 to k do
    // The division is always without remainder here.
    result <- result * int64 (n - k + i) / int64 i
  int result

/// Binary entropy: the amount of information in a binary random variable with the given p.
/// The result is returned in bits and ranges from 0 to 1.
let entropy (p : float) =
  if p > 0.0 && p < 1.0 then (p * log p + (1.0 - p) * log(1.0 - p)) / -ln2 else 0.0

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

/// Sum of a geometric series with n terms. Terms are defined by first term a0 and the ratio between successive terms;
/// term i is a0 * ratio ** (i - 1), 1 <= i <= n.
let inline geometricSum n (a0 : 'a) (ratio : 'a) : 'a =
  if ratio <> 1G then a0 * (1G - pow ratio n) / (1G - ratio) else a0 * G n

/// Sum of an arithmetic series with n terms. Terms are defined by first term a0 and the difference between successive
/// terms; term i is a0 + delta * (i - 1), 1 <= i <= n.
let inline arithmeticSum n (a0 : 'a) (delta : 'a) : 'a =
  G n * (2G * a0 + delta * (G (n - 1))) / 2G



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



/// Returns the label of a discriminated union case. Throws an exception if the argument is not
/// a discriminated union value.
let unionLabel (x : 'a) =
  match FSharp.Reflection.FSharpValue.GetUnionFields(x, typeof<'a>) with
  | case, _ -> case.Name



/// Regular expression active pattern. Captures matched groups into a list.
/// Note that .NET maintain a cache of compiled regular expressions that accelerates use cases like this.
let (|Regex|_|) pattern input =
  let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
  if m.Success then Some(List.tail [for group in m.Groups -> group.Value]) else None



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



/// Option style value type. Interface here should be kept as compatible as possible with the standard option type.
/// This type is useful in avoiding allocations when the parameter type is a value type as well.
type Optionval<'T> = struct
  val isSome : bool
  val value : 'T
  new(isSome, value) = { isSome = isSome; value = value }
  member inline this.Value = this.value
  member inline this.isNone = not this.isSome
  member inline this.IsSome = this.isSome
  /// Applies a function to the value of the option, if it exists.
  member inline this.apply(f : 'T -> unit) = if this.isSome then f this.value
  /// Transforms the option with the given value map.
  member inline this.map(f : 'T -> 'U) = if this.isSome then Optionval(true, f this.value) else Optionval(false, Unchecked.defaultof<'U>)
  /// Filters option values with a predicate, transforming to Noneval those that fail it.
  member inline this.filter(predicate : 'T -> bool) = if this.isSome && predicate !this then this else Optionval(false, Unchecked.defaultof<'T>)
  /// Returns true iff the option has a value that matches the predicate.
  member inline this.isSomeAnd(predicate : 'T -> bool) = this.isSome && predicate !this
  /// Returns true iff the option has no value or if the value matches the predicate.
  member inline this.isNoneOr(predicate : 'T -> bool) = this.isNone || predicate !this

  override this.ToString() = if this.isSome then sprintf "Someval(%A)" this.value else "Noneval"
end


/// Constructs an Optionval structure with no value.
let inline Noneval<'T> = Optionval(false, Unchecked.defaultof<'T>)


/// Construct an Optionval structure with a value.
let inline Someval(value) = Optionval(true, value)


/// Constructs an Optionval structure that is logically Noneval but has a fallback value that can be accessed.
/// For low level optimization.
let inline Fallbackval(value) = Optionval(false, value)


/// Active pattern for matching option structures.
let (|Someval|Noneval|) (x : _ Optionval) = if x.isSome then Someval(x.value) else Noneval



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
  { Counter.value = seed; f = minstd }



/// Calls the (stateful) generator until the return value fulfills the predicate. Returns the accepted value.
let inline doFind (generator : unit -> 'a) (predicate : 'a -> bool) =
  let rec loop() = let v = generator() in if predicate v then v else loop()
  loop()


/// A do .. while control structure. Keeps calling the body as long as it fulfills the condition.
let inline doWhile (body : unit -> _) (condition : _ -> bool) =
  let mutable loop = true
  while loop do
    loop <- body() |> condition



// List extensions.
type List<'a> with
  member inline this.size = this.Length



// String extensions.
type String with
  member inline this.size = this.Length
  member inline this.last = this.Length - 1



// Array type extensions.
type ``[]``<'a> with
  /// The size of the array.
  member inline a.size = a.Length
  /// The index of the last element in the array.
  member inline a.last = a.Length - 1
  /// Fills the array with items from an indexed function.
  member inline a.fill(f : int -> _) = for i = 0 to a.last do a.[i] <- f i
  /// Fills the array with a constant value.
  member inline a.fill(x) = Array.fill a 0 a.size x
  /// Swaps two array items. Does not check that the indices differ.
  member inline a.swap(i, j) = let tmp = a.[i] in a.[i] <- a.[j] ; a.[j] <- tmp
  /// Modifies array items according to a function - performing, effectively, an in-place map operation.
  member inline a.modify(f) = for i = 0 to a.last do a.[i] <- f a.[i]
  /// Sums the array using the given projection.
  member inline a.sumBy(projection) = Array.sumBy projection a
  /// Reduces the array using the given projection and binary operator.
  member inline a.reduceBy(projection, binop) =
    enforce (a.size > 0) "Array.reduceBy: Empty array."
    let mutable x = projection a.[0]
    for i = 1 to a.last do x <- binop x (projection a.[i])
    x
  /// Calls the function for each item in the array.
  member inline a.iter(f) = for i = 0 to a.last do f a.[i]
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
  let inline setAll (a : 'a array) (v : 'a) = Array.fill a 0 a.size v
  /// Scrubs the whole array with the default value of the item type.
  let inline scrub (a : 'a array) = setAll a Unchecked.defaultof<'a>
  /// Swaps two array items. Does not check that the indices differ.
  let inline swap (a : 'a array) i j = a.swap(i, j)
