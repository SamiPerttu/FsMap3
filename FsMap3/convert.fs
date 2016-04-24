/// Some numerical conversion functions.
module FsMap3.Convert

open Common


// INT -> FLOAT CONVERSIONS.
type INT =
  static member inline R = float (maxValue int) - float (minValue int) + 1.0
  static member inline m = float (minValue int)
  static member inline M = float (maxValue int)

/// Converts an int to the given interval type in 0 ... 1.
let inline float01 (interval : Interval) a =
  match interval with
  | LeftClosed -> (float a - INT.m) / INT.R
  | Closed -> (float a - INT.m) / (INT.R - 1.0)
  | Open -> (float a - INT.m + 1.0) / (INT.R + 1.0)
  | RightClosed -> (float a - INT.m + 1.0) / INT.R

/// Converts an int to the given interval type in -1 ... 1.
let inline float11 (interval : Interval) a =
  match interval with
  | LeftClosed -> float a / (INT.M + 1.0)
  | Closed -> (float a + 0.5) / (INT.M + 0.5)
  | Open -> (float a + 0.5) / (INT.M + 1.5)
  | RightClosed -> (float a + 1.0) / (INT.M + 1.0)



// UINT -> FLOAT CONVERSIONS.
type UINT =
  static member inline R = float (maxValue uint) + 1.0
  static member inline M = float (maxValue uint)
  static member inline H = float (maxValue uint) * 0.5

/// Converts a uint32 to the given interval type in the unit range.
let inline float01u (interval : Interval) (a : uint) =
  // Note: thanks to inlining, if the interval type is constant, the match statement will be optimized away.
  match interval with
  | LeftClosed -> float a / UINT.R
  | Closed -> float a / UINT.M
  | Open -> (float a + 1.0) / (UINT.R + 1.0)
  | RightClosed | _ -> (float a + 1.0) / UINT.R

/// Converts a uint32 to the given interval type between -1 and 1.
let inline float11u (interval : Interval) (a : uint) =
  match interval with
  | LeftClosed -> (float a - UINT.H - 0.5) / (UINT.H + 0.5)
  | Closed -> (float a - UINT.H) / UINT.H
  | Open -> (float a - UINT.H) / (UINT.H + 1.0)
  | RightClosed | _ -> (float a - UINT.H + 0.5) / (UINT.H + 0.5)



/// Converts a pair of uint32 arguments to a uniformly distributed unit 3-vector
/// using the Lambert cylindrical equal-area projection.
let unitVec3 (a : int) (b : int) =
  // Due to accuracy and distribution issues, we do not attempt to generate vectors very close to Z axis poles.
  let z = float11 Open a
  let theta = G tau * float01 LeftClosed b
  let r = sqrt(1.0 - squared z)
  Vec3f(r * cos theta |> float32, r * sin theta |> float32, z |> float32)


/// Converts a trio of uint32 arguments to a uniformly distributed unit 4-vector (returned as a quaternion).
let unitQuaternion (a : int) (b : int) (c : int) =
  // Algorithm sourced from Shoemake, K., Uniform Random Rotations, Graphics Gems III, 1992.
  let u1 = float01 Closed a
  let u2 = float01 LeftClosed b
  let u3 = float01 LeftClosed c
  let sqrtU1 = sqrt(u1)
  let sqrt1U1 = sqrt(1.0 - u1)
  Quaternionf(sqrt1U1 * sinr(u2) |> float32, sqrt1U1 * cosr(u2) |> float32, sqrtU1 * sinr(u3) |> float32, sqrtU1 * cosr(u3) |> float32)


/// Converts a uint32 to a sample from the geometric distribution Geom(p)
/// where p is the probability of success (p > 0).
let geometric (p : float) (a : int) =
  if p >= 1.0 then
    0
  else
    int (log(float01 RightClosed a) / log(1.0 - p))


/// Converts a uint32 to a sample from the Poisson distribution Pois(lambda).
let poisson (lambda : float) =
  let p0 = exp(-lambda)
  fun (a : int) ->
    let x = float01 Closed a
    let mutable k = 0
    let mutable p = p0
    let mutable sum = p0
    while x > sum do
      k <- k + 1
      p <- p * lambda / float k
      sum <- sum + p
    k


