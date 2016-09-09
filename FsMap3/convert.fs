/// Some numerical conversion functions.
module Fuse.Convert

open Common


// INT -> FLOAT CONVERSIONS.
type INT =
  static member inline R = float (maxValue int) - float (minValue int) + 1.0
  static member inline m = float (minValue int)
  static member inline M = float (maxValue int)

/// Converts an int to the given interval type in 0 ... 1.
let inline float01 (interval : Interval) a =
  // Note: thanks to inlining, if the interval type is constant, the match statement will be optimized away.
  match interval with
  | LeftClosed  -> (float a - INT.m) / INT.R
  | Closed      -> (float a - INT.m) / (INT.R - 1.0)
  | Open        -> (float a - INT.m + 1.0) / (INT.R + 1.0)
  | RightClosed -> (float a - INT.m + 1.0) / INT.R

/// Converts an int to the given interval type in -1 ... 1.
let inline float11 (interval : Interval) a =
  match interval with
  | LeftClosed  -> float a / (INT.M + 1.0)
  | Closed      -> (float a + 0.5) / (INT.M + 0.5)
  | Open        -> (float a + 0.5) / (INT.M + 1.5)
  | RightClosed -> (float a + 1.0) / (INT.M + 1.0)



// UINT -> FLOAT CONVERSIONS. ALL CASES HAVE BEEN CHECKED FOR CORRECTNESS.

type UINT =
  static member inline R = float (maxValue uint) + 1.0
  static member inline M = float (maxValue uint)
  static member inline H = float (maxValue uint) * 0.5

/// Converts a uint32 to the given interval type in the unit range.
let inline float01u (interval : Interval) (a : uint) =
  match interval with
  | LeftClosed  -> float a / UINT.R
  | Closed      -> float a / UINT.M
  | Open        -> (float a + 1.0) / (UINT.R + 1.0)
  | RightClosed -> (float a + 1.0) / UINT.R

/// Converts a uint32 to the given interval type between -1 and 1.
let inline float11u (interval : Interval) (a : uint) =
  match interval with
  | LeftClosed  -> (float a - UINT.H - 0.5) / (UINT.H + 0.5)
  | Closed      -> (float a - UINT.H) / UINT.H
  | Open        -> (float a - UINT.H) / (UINT.H + 1.0)
  | RightClosed -> (float a - UINT.H + 0.5) / (UINT.H + 0.5)



/// Converts a pair of int32s to a pair of independently distributed Gaussians in N(0, 1)
/// with the Box-Muller transform. The maximum value returned is ~ 6.66043689.
let gaussian (a : int) (b : int) =
  let R = sqrt(-2.0 * log(float01 RightClosed a))
  let theta = pi2 * float01 LeftClosed b
  Pair(R * cos theta, R * sin theta)


/// Converts an int32 to a sample from the geometric distribution Geom(p)
/// where p is the probability of success (p > 0).
let geometric (p : float) (a : int) =
  if p >= 1.0 then
    0
  else
    int (log(float01 RightClosed a) / log(1.0 - p))


/// Converts a pair of int32s to a uniformly distributed unit 3-vector
/// using the Lambert cylindrical equal-area projection.
let unitVec3 (a : int) (b : int) =
  // Due to accuracy and distribution issues, we do not attempt to generate vectors very close to Z axis poles.
  let z = float11 Open a
  let theta = G tau * float01 LeftClosed b
  let r = sqrt(1.0 - squared z)
  Vec3(r * cos theta, r * sin theta, z)


/// Converts a pair of int32s to a uniformly distributed unit 3-vector
/// using the Lambert cylindrical equal-area projection.
let unitVec3f (a : int) (b : int) =
  // Due to accuracy and distribution issues, we do not attempt to generate vectors very close to Z axis poles.
  let z = float11 Open a
  let theta = G tau * float01 LeftClosed b
  let r = sqrt(1.0 - squared z)
  Vec3f(r * cos theta |> float32, r * sin theta |> float32, z |> float32)


/// Converts a trio of int32s to a uniformly distributed unit 4-vector (returned as a double precision quaternion).
let unitQuaternion (a : int) (b : int) (c : int) =
  // Algorithm sourced from Shoemake, K., Uniform Random Rotations, Graphics Gems III, 1992.
  let u1 = float01 Closed a
  let u2 = float01 LeftClosed b
  let u3 = float01 LeftClosed c
  let sqrtU1 = sqrt(u1)
  let sqrt1U1 = sqrt(1.0 - u1)
  Quaternion(sqrt1U1 * sinr(u2), sqrt1U1 * cosr(u2), sqrtU1 * sinr(u3), sqrtU1 * cosr(u3))


/// Converts a trio of int32s to a uniformly distributed unit 4-vector (returned as a single precision quaternion).
let unitQuaternionf (a : int) (b : int) (c : int) =
  // Algorithm sourced from Shoemake, K., Uniform Random Rotations, Graphics Gems III, 1992.
  let u1 = float01 Closed a
  let u2 = float01 LeftClosed b
  let u3 = float01 LeftClosed c
  let sqrtU1 = sqrt(u1)
  let sqrt1U1 = sqrt(1.0 - u1)
  Quaternionf(sqrt1U1 * sinr(u2) |> float32, sqrt1U1 * cosr(u2) |> float32, sqrtU1 * sinr(u3) |> float32, sqrtU1 * cosr(u3) |> float32)



/// Converts an uint32 to a sample from the Poisson distribution Pois(1).
/// The maximum returned value is 12, with a frequency of 1 in ~1 billion.
let poisson1 (a : uint) =
  // End-of-line comments are the corresponding CDF values.
  if a < 0x5e2d58d9u then 0 // 0.3678794412
  elif a < 0xbc5ab1b1u then 1 // 0.7357588823
  elif a < 0xeb715e1eu then 2 // 0.9196986029
  elif a < 0xfb239797u then 3 // 0.9810118431
  elif a < 0xff1025f6u then 4 // 0.9963401532
  elif a < 0xffd90f3cu then 5 // 0.9994058152
  elif a < 0xfffa8b72u then 6 // 0.9999167589
  elif a < 0xffff540cu then 7 // 0.9999897508
  elif a < 0xffffed1fu then 8 // 0.999998874797402
  elif a < 0xfffffe21u then 9 // 0.999999888574522
  elif a < 0xffffffd5u then 10 // 0.999999989952234
  elif a < 0xfffffffcu then 11 else 12 // 0.999999999168389


/// Converts a uint32 to a sample from the Poisson distribution Pois(lambda).
/// This should be used with smallish lambda (distribution mean) only.
let poisson (lambda : float) =
  let p0 = exp(-lambda)
  // This is a straightforward algorithm that does inversion sampling. 
  fun (a : int) ->
    let x = float01 Closed a
    let mutable k   = 0
    let mutable p   = p0
    let mutable sum = p0
    while x > sum do
      k <- k + 1
      p <- p * lambda / float k
      // Add a small constant to ensure that the loop terminates.
      sum <- sum + p + 1.0e-9
    k


/// An unrolled Poisson converter. Samples a truncated Pois(lambda) distribution: the maximum returned value is 9.
/// This is nearly twice as fast as Convert.poisson.
let poissonUnrolled (lambda : float) =
  let p0 = exp(-lambda)
  fun (a : uint) ->
    let x = float01u Closed a
    let mutable p = p0
    let mutable sum = p0
    if x <= sum then 0 else
      p <- p * lambda
      sum <- sum + p
      if x <= sum then 1 else
        p <- p * lambda * Q 1 2
        sum <- sum + p
        if x <= sum then 2 else
          p <- p * lambda * Q 1 3
          sum <- sum + p
          if x <= sum then 3 else
            p <- p * lambda * Q 1 4
            sum <- sum + p
            if x <= sum then 4 else
              p <- p * lambda * Q 1 5
              sum <- sum + p
              if x <= sum then 5 else
                p <- p * lambda * Q 1 6
                sum <- sum + p
                if x <= sum then 6 else
                  p <- p * lambda * Q 1 7
                  sum <- sum + p
                  if x <= sum then 7 else
                    p <- p * lambda * Q 1 8
                    sum <- sum + p
                    if x <= sum then 8 else 9
  
