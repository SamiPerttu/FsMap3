﻿/// Fade curves interpolate between 0 and 1 in [0, 1].
[<RequireQualifiedAccess; ReflectedDefinition>]
module Fuse.Fade

open Common


(*
Note. The continuity property mentioned in some comments here applies when
a fade is used piecewise, e.g., as a bump, staircase or threshold function.
For the property to hold, in addition to being continuous, the corresponding
derivative(s) must vanish at the endpoints.
*)

/// Linear fade.
let inline line x = x

/// 2nd power fade.
let inline power2 (x : 'a) : 'a = squared x

/// 3rd power fade.
let inline power3 (x : 'a) : 'a = cubed x

/// 4th power fade.
let inline power4 (x : 'a) : 'a = squared (squared x)

/// 5th power fade.
let inline power5 (x : 'a) : 'a = x * squared (squared x)

/// 6th power fade.
let inline power6 (x : 'a) : 'a = cubed (squared x)

/// Power curve fade. The first argument is the order of the curve (p > 0).
let inline power (p : 'a) (x : 'a) : 'a = abs x ** p

/// Discontinuous step fade. Value jumps from zero to one halfway through.
let inline step (x : 'a) : 'a = if x < Q 1 2 then 0G else 1G

/// Sine fade based on a Taylor series approximation.
let inline sine (x : 'a) : 'a = sinTaylor ((x - Q 1 2) * G 3.13981162410392) * G 0.5000782624053935 + Q 1 2

/// First order continuous, smooth cubic fade.
let inline smooth1 (x : 'a) : 'a = x * x * (3G - 2G * x)

/// Second order continuous, smooth quintic polynomial fade recommended by Ken Perlin.
let inline smooth2 (x : 'a) : 'a = x * x * x * (x * (x * 6G - 15G) + 10G)

/// Third order continuous, super smooth 7th order polynomial fade.
let inline smooth3 (x : 'a) : 'a = let x2 = x * x in x2 * x2 * (35G - 84G * x + (70G - 20G * x) * x2)

/// A cubic fade that starts and ends at a slope but has a plateau in the middle.
let inline shelf (x : 'a) : 'a = ((4G * x - 6G) * x + 3G) * x

/// Exponential fade over the given number of magnitudes (M > 0). In other words, the slope grows geometrically.
/// The slope at x = 1 is exp10(M) times the slope at x = 0.
let inline geometric (M : 'a) : 'a -> 'a = let d = exp10(-M) in fun x -> (exp10((x - 1G) * M) - d) / (1G - d)

/// Smooth sigmoidal fade with sharpness a in unit range (0 = linear fade, 1 = nearly step fade).
let inline sigmoid (a : 'a) : 'a -> 'a =
  let power = xerp 1G 1000G a
  fun x ->
    if x < Q 1 2 then
      (x * 2G) ** power * Q 1 2
    else
      1G - ((1G - x) * 2G) ** power * Q 1 2

/// A quarter circle fade that slopes upwards. Inverse function of Fade.downarc.
let inline uparc (x : 'a) : 'a = 1G - sqrt(max 0G (1G - squared x))

/// A quarter circle fade that slopes downwards. Inverse function of Fade.uparc.
let inline downarc (x : 'a) : 'a = sqrt(max 0G (2G * x - squared x))

/// Skewed fade, climbs faster initially (positive skew) if bias > 0 or slower (negative skew) if bias < 0.
/// Linear fade when bias = 0.
let inline skew (bias : 'a) : 'a -> 'a =
  if bias >= 0G then
    let p = exp2 bias in fun x -> 1G - max 0G (1G - x) ** p
  else
    let p = exp2 -bias in fun x -> x ** p

/// A number of sine waves as a fade (cycles > 0), oscillating between 0 and 1.
/// As a peculiarity, Fade.wave _ 0.5 = 0.5. Fade.wave 1 is equivalent to Fade.sine.
let inline wave (cycles : int) (x : 'a) : 'a = sinr0(x * (G cycles - Q 1 2) - Q 1 4)

/// Another wavy fade (cycles > 0). The waves in this one are scaled to maintain their proportions.
/// As a peculiarity, Fade.worm _ 0.5 = 0.5. Fade.worm 1 is equivalent to Fade.sine.
let inline worm (cycles : int) (x : 'a) : 'a =
  let scale = Q 1 2 / (G cycles - Q 1 2)
  (1G - scale) * x + scale * wave cycles x



// Fade function combinators.

/// Reverses the shape of a fade function.
let inline reverse (f : 'a -> 'a) (x : 'a) =
  1G - (f (1G - x))

/// Saturates a fade function so that it reaches unity value at x = 1 - a (a in [0, 1]).
/// The fade is unmodified when a = 0. The fade is squished away completely when a = 1.
let inline saturate (a : 'a) (f : 'a -> 'a) (x : 'a) : 'a = let b = 1G - a in if x < b then f (x / b) else 1G

/// Creates a fade from an arbitrary function with argument range [x0, x1]. Function values are translated such
/// that the endpoint values become 0 and 1. Any values outside the range are clamped.
let inline zoom (x0 : 'a) (x1 : 'a) (f : 'a -> 'a) : 'a -> 'a =
  let y0 = f x0
  let y1 = f x1
  lerp x0 x1 >> f >> delerp01 y0 y1

/// Mixes between g and h according to M in [0, 1]: minimum if M = 0, average if M = 0.5, maximum if M = 1.
let inline mix (M : 'a) (f : 'a -> 'a) (g : 'a -> 'a) (x : 'a) : 'a =
  let a = f x
  let b = g x
  lerp (min a b) (max a b) M

/// Fades linearly from f to g.
let inline ferp f g x =
  lerp (f x) (g x) x

/// Turns a fade into a fade with n smaller copies of itself.
let inline clone (n : int) (f : 'a -> 'a) (x : 'a) : 'a =
  let x = x * G n
  let ix = floor x
  let fx = f(x - ix)
  (fx + ix) / G n

/// Concatenates a fade with a mirrored copy of itself.
let inline mirror (f : 'a -> 'a) (x : 'a) : 'a =
  if x < Q 1 2 then
    Q 1 2 * f (x * 2G)
  else
    1G - Q 1 2 * f (2G - x * 2G)



/// Ramp fade that starts smoothly from a slope of zero and ends linearly.
/// The width of the ramp-up is w (w in [0, 1]). This becomes a linear fade when w = 0.
let inline ramp (w : 'a) (x : 'a) : 'a =
  if w = 0G then
    x
  else
    let x' = x / w
    let y' = if x' < 1G then cubed x' * (1G - Q 1 2 * x') else x' - Q 1 2
    y' / (Q 1 2 + (1G - w) / w)

/// Smooth line fade where the curves at endpoints have total width w.
/// Becomes a linear fade when w = 0. Second order continuous when w > 0.
let inline smoothLine (w : 'a) (x : 'a) : 'a = mirror (ramp w) x



// Fade function transformations into other kinds of functions.

/// Turns a fade into a wave function with a period of unity. The wave phase is similar to sinr.
/// Stitches together the wave from 4 copies of the fade.
let inline sinefyr (f : 'a -> 'a) (x : 'a) : 'a =
  let x = x * 4G
  let ix = floor x
  match int ix &&& 3 with | 0 -> f(x - ix) | 1 -> f(1G - x + ix) | 2 -> -f(x - ix) | _ -> -f(1G - x + ix)

/// Turns a fade into a wave function with a period of tau. The wave phase is similar to sin.
/// Stitches together the wave from 4 copies of the fade.
let inline sinefy (f : 'a -> 'a) (x : 'a) = sinefyr f (x * Q 1 tau)

/// Turns a fade into a wave function with a period of unity. The wave phase is similar to cosr.
/// Stitches together the wave from 2 copies of the fade.
let inline cosifyr (f : 'a -> 'a) (x : 'a) : 'a =
  let x = x * 2G
  let ix = floor x
  match int ix &&& 1 with | 0 -> 1G - 2G * f(x - ix) | _ -> 2G * f(x - ix) - 1G

/// Turns a fade into a wave function with a period of tau. The wave phase is similar to cos.
/// Stitches together the wave from 2 copies of the fade.
let inline cosify (f : 'a -> 'a) (x : 'a) = cosifyr f (x * Q 1 tau)

/// Turns a fade into a bump centered at 0, with a radius of support and height of 1.
let inline bump (f : 'a -> 'a) (x : 'a) : 'a = f (max 0G (1G - abs x))

/// Creates, from a fade, a staircase function with an overall slope of unity and one step per unit.
let inline staircase (f : 'a -> 'a) (x : 'a) : 'a =
  let z = floor x
  z + f (x - z)

/// Mirrors fade f into a threshold function g centered at origin: g(-1) = -1, g(0) = 0, g(1) = 1.
let inline threshold (f : 'a -> 'a) (x : 'a) : 'a =
  if x < -1G then -1G elif x < 0G then -f -x elif x < 1G then f x else 1G



// Utility stuff.

// Approximates the area of a fade function by sampling. Takes 100 evenly spaced samples.
let inline area (f : 'a -> 'a) : 'a =
  let n = 100
  let mutable sum = 0G
  for i = 1 to n do
    sum <- sum + f (Q (i - 1) (n - 1))
  sum / G n

