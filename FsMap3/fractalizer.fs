/// Fractalizers are map combinators that compute an output by repeated sampling of maps.
module Fuse.Fractalizer

open Common
open Mangle
open Basis3
open Map3


/// Displaces v0 repeatedly by f. The initial displacement amount is scaled by roughness each step.
/// Establishes a coordinate frame as well, which is used to orient the displacements.
/// Samples from f, scaled by twist radians, twist the frame. Returns the final twisted map value.
/// The results often resemble creases when visualized.
let walk (roughness : float32) (displace : float32) twist n (f : Map3) (v0 : Vec3f) =
  let mutable v = v0
  let mutable y = Vec3f.zero
  let mutable D = displace
  let mutable T = Quaternionf.one
  for i = 1 to n do
    y <- f v
    let L2 = y.length2
    if L2 * twist > 1.0e-9f then
      let L = sqrt L2
      T <- Quaternionf(y / L, twist * L) * T
    y <- y * T
    v <- v + D * y
    D <- D * roughness
  y



/// Iterates a map neighborhood. c is the neighborhood size.
let iterate n c (f : Map3) (v0 : Vec3f) =
  let mutable i = 0
  let mutable v = Vec3f.zero
  while i < n do
    v <- f (v0 + c * v)
    i <- i + 1
  v



/// Basic fractalizer. Samples many octaves of a basis.
/// Roughness is the attenuation for each doubling of frequency (typically < 1).
/// Lacunarity (lacunarity > 0) is the wavelength scaling of successive octaves.
let fractal (roughness : float32)
            (lacunarity : float32)
            (mix : MixOp)
            (initialSeed : int)
            (initialFrequency : float32)
            (octaves : int)
            (basis : Basis3) =

  // Get roughness per octave from roughness.
  let r = roughness ** (-log2 lacunarity)
  // Instantiate bases.
  let basis = Array.init octaves (fun i -> basis (initialSeed + i) (initialFrequency / pow lacunarity i))

  fun (v : Vec3f) ->
    let mutable value = Mix.start
    let mutable F     = initialFrequency
    let mutable w     = 1.0f
    for i = 0 to basis.last do
      value <- mix value w 1.0f (basis.[i] (v + Vec3f.fromSeed(mangle32 (i + manglef (float F)))))
      w <- w * r
      F <- F / lacunarity
    Mix.result value



/// Fractalizes basis g. Displaces successive samples with the walk operator.
let fractald (roughness : float32)
             (lacunarity : float32)
             (highpass : float32)
             (mix : MixOp)
             (walk : WalkOp)
             (initialSeed : int)
             (initialFrequency : float32)
             (octaves : int)
             (basis : Basis3) =

  assert (roughness > 0.0f && lacunarity > 0.0f && highpass >= 0.0f && highpass < 1.0f && initialFrequency > 0.0f)

  // Get roughness per octave from roughness.
  let r = roughness ** (-log2 lacunarity)
  // Set the initial weight so the most important octave has unity weight.
  let w0 = if r <= 1.0f then 1.0f else 1.0f / pow r (octaves - 1)
  let basis = Array.init octaves (fun i -> basis (initialSeed + i) (initialFrequency / pow lacunarity i))

  fun (v : Vec3f) ->
    let mutable F     = initialFrequency
    let mutable w     = w0
    let mutable d     = WalkState.start
    let mutable value = Mix.start
    let mutable h     = 1.0f - highpass

    for i = 0 to basis.last do
      let a = basis.[i] (v + d.read(F))
      value <- mix value w h a
      F <- F / lacunarity
      w <- w * r
      h <- sqrt h
      d <- walk d F 1G a

    Mix.result value



/// Fractalizes basis g. Displacement is applied starting from the most detailed octave.
/// This tends to produce a billowy appearance, as opposed to creases.
let fractaldi roughness lacunarity lowpass mix walk initialSeed initialFrequency octaves g =
  fractald roughness (1.0f / lacunarity) lowpass mix walk initialSeed (initialFrequency / pow lacunarity (octaves - 1)) octaves g



/// Evaluates a lowpass, variable displaced and twisted version of a fractalized basis.
/// The aforementioned parameters for each point are drawn from the variable map.
let fractalv roughness
             lacunarity
             (mix : MixOp)
             (walk : WalkOp)
             minDisplace
             maxTwist
             minOctaves
             maxOctaves
             firstOctave
             octaveDirection
             initialSeed
             initialFrequency
             (variable : Map3)
             (basis : Basis3) =

  let basis = Array.init (int maxOctaves) (fun i -> basis (initialSeed + i) (initialFrequency / pow lacunarity i))
  let r     = roughness ** (-log2 lacunarity)

  fun (v : Vec3f) ->
    let u        = variable v
    let octaves  = lerp01 minOctaves maxOctaves (map11to01 u.x)
    let displace = lerp01 minDisplace 1.0f (map11to01 u.y)

    let R =
      let L = u.length
      let twist = maxTwist * L
      if twist > 0.0f then
        Quaternionf(u / L, twist)
      else
        Quaternionf.one

    let mutable d     = WalkState.start
    let mutable value = Mix.start

    for i = 0 to basis.last do

      let octave = emod (firstOctave + octaveDirection * i) basis.size
      if float32 octave < octaves then

        // Fade out the final octave.
        let alpha = delerp01 octaves (octaves - 1.0f) (float32 octave)
        if alpha > 0.0f then
          let F = initialFrequency / pow lacunarity octave
          let w = pow r octave
          let y = basis.[octave] (v + d.read(F))
          value <- mix value w alpha y
          d <- walk d F alpha (alpha * displace * y * R)

    Mix.result value

