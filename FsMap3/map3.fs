/// Map3 functions and combinators.
module FsMap3.Map3

open Common
open Mangle
open Potential
open Basis3
open Walk


/// Maps in 3-space form the foundation of the texture engine.
/// The canonical range of a map is [-1, 1] for each component.
type Map3 = Vec3f -> Vec3f



/// Shapes the components of a vector with a function.
let shape (f: float32 -> float32) : Map3 = fun v -> v.map(f)


/// Shapes the components of a vector with the antisymmetric extension of a fade function. Inputs are clamped.
let shapef (fade : float32 -> float32) : Map3 =
  let inline g x = Fade.threshold fade x
  fun v -> Vec3f(g v.x, g v.y, g v.z)


/// Shapes the magnitude of a vector with a function.
let shape3 (f : float32 -> float32) (v : Vec3f) =
  let length = v.length
  if length > 0G then f length / length * v else Vec3f.zero
    

/// The zero map.
let inline zero (v : Vec3f) = Vec3f.zero


/// The identity map.
let inline identity (v : Vec3f) = v


/// A constant map.
let inline constant (c : Vec3f) (_ : Vec3f) = c


/// Product of the arguments. Scales v by u.
let inline scale u (v : Vec3f) = u * v


/// Sum of the arguments. Translates v by u.
let inline translate u (v : Vec3f) = u + v


/// Flat XY plane unit square pattern that arranges successive depth slices
/// of the XY unit square in an n-by-n grid.
let sliceChart (n : int) (v : Vec3f) =
  let tiles = float32 n
  let x = fract v.x
  let y = fract v.y
  let z = floor (x * tiles) / squared tiles + floor (y * tiles) / tiles
  let x = fract (x * tiles)
  let y = fract (y * tiles)
  Vec3f(x, y, z)


/// Flattens a map to 2 dimensions by fixing the X value.
let flattenX (x : float32) (v : Vec3f) = Vec3f(x, v.y, v.z)


/// Flattens a map to 2 dimensions by fixing the Y value.
let flattenY (y : float32) (v : Vec3f) = Vec3f(v.x, y, v.z)


/// Flattens a map to 2 dimensions by fixing the Z value.
let flattenZ (z : float32) (v : Vec3f) = Vec3f(v.x, v.y, z)


/// Averages components. If they are colors, then the result is a grayscale map. All components are assigned equal weight.
let grayscale (v : Vec3f) = Vec3f(v.sum / 3.0f)


/// Rotates v with u.
/// Vector direction in u is the rotation axis and vector length times amount is the angle in radians.
let rotate amount (u : Vec3f) (v : Vec3f) =
  let length = u.length
  if length > 1.0e-9f then
    let axis = u / length
    v * Quaternionf(axis, amount * length)
  else
    v 


/// More complex rotation operator that has similarities with layering.
/// u and v are compared; if their distance is below width, then v is rotated using the vector (u - v)
/// shaped with the fade function.
let rotatef width amount (fade : float32 -> float32) (u : Vec3f) (v : Vec3f) =
  let r = v - u
  let d = r.length
  if d > 1.0e-9f && d < width then
    let axis = r / d
    v * Quaternionf(axis, amount * d * fade(1G - d / width))
  else
    v


/// Component softmin when a < 0, softmax when a > 0, otherwise a linear mix.
let softmix (a : float32) (v : Vec3f) (u : Vec3f) =
  let inline soft b c = let wb, wc = exp(b * a), exp(c * a) in (b * wb + c * wc) / (wb + wc)
  Vec3f.bimap(v, u, soft)


/// Vector softmin when a < 0, softmax when a > 0, otherwise a linear mix.
let softmix3 (a : float32) (v : Vec3f) (u : Vec3f) =
  let wf = exp(a * v.sum)
  let wg = exp(a * u.sum)
  let wZ = 1G / (wf + wg)
  let inline soft sf sg = (sf * wf + sg * wg) * wZ
  Vec3f.bimap(v, u, soft)


/// Scatters vector components by a wavified fade function.
let scatterf (amount : Vec3f) (offset : Vec3f) (fade : float32 -> float32) =
  let amount = amount * 0.5f
  let wavef = Fade.sinefyr fade
  fun (v : Vec3f) -> (v * amount + offset).map(wavef)


/// Scatters vector components by feeding them to a wave function with an offset.
let scatter (wave : float32 -> float32) (offset : Vec3f) =
  fun (v : Vec3f) -> (v + offset).map(wave)


/// Displaces map g by samples from map f scaled by a.
let inline displace a (f : Map3) (g : Map3) (v : Vec3f) = g (v + a * f v)


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


/// Displaces v0 by a series of functions. The initial displacement amount is scaled by roughness each step.
/// Establishes a coordinate frame as well, which is used to orient the displacements.
/// Samples from f, scaled by twist radians, twist the frame. Returns the final twisted map value.
/// The results often resemble creases when visualized.
let walki (roughness : float32) (displace : float32) twist n (f : int -> Map3) =
  // Precompute the function series.
  let a = Array.init n f
  fun (v0 : Vec3f) ->
    let mutable v = v0
    let mutable y = Vec3f.zero
    let mutable D = displace
    let mutable T = Quaternionf.one
    for i = 0 to n - 1 do
      y <- a.[i] v
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


/// Iterates neighborhoods of a series of maps.
let iteratei n c (f : int -> Map3) =
  // Precompute the function series.
  let a = Array.init n f
  fun (v0 : Vec3f) ->
    let mutable i = 0
    let mutable v = Vec3f.zero
    while i < n do
      v <- a.[i] (v0 + c * v)
      i <- i + 1
    v


/// Combines two maps with a binary operation.
let bimap (binop : Vec3f -> Vec3f -> Vec3f) (f : Map3) (g : Map3) (v : Vec3f) =
  binop (f v) (g v)


/// Combines two maps with a binary operation while applying wavelength scaled, shaped displacement from f to g.
let bimapd (displaceShape : Map3) (binop : Vec3f -> Vec3f -> Vec3f) (f : Map3) (g : Map3) (v : Vec3f) =
  let u = f v
  binop u (g (v + displaceShape u))


/// Applies a binary operation to two basis functions.
let binaryBasis (binop : Vec3f -> Vec3f -> Vec3f) (frequencyFactor : float32) (f : Basis3) (g : Basis3) (frequency : float32) =
  let f = f (frequency * frequencyFactor)
  let g = g frequency
  fun (v : Vec3f) ->
    binop (f v) (g v)


/// Applies a binary operation to two basis functions. Before the binary operation, g is displaced by f
/// with the displacement scaled by wavelength.
let binaryBasisd (binop : Vec3f -> Vec3f -> Vec3f) (displaceShape : Map3) (frequencyFactor : float32) (f : Basis3) (g : Basis3) (frequency : float32) =
  let frequency' = frequency * frequencyFactor
  let f = f frequency'
  let g = g frequency
  fun (v : Vec3f) ->
    let u = f v
    binop u (g (v + displaceShape u / frequency'))


/// Applies a displacement scaled by wavelength from f to g.
let displaceBasis (response : Map3) (frequencyFactor : float32) (f : Basis3) (g : Basis3) (frequency : float32) =
  let frequency' = frequency * frequencyFactor
  let f = f frequency'
  let g = g frequency
  fun (v : Vec3f) ->
    let d = response (f v)
    g (v + d / frequency')


/// Shapes a basis with a map.
let shapeBasis (f : Map3) (g : Basis3) frequency =
  let g = g frequency
  fun (v : Vec3f) -> f (g v)


/// Layers a ("application") on b ("base") in a manner similar to Mix.layer.
let layer width (fade : float32 -> float32) (a : Vec3f) (b : Vec3f) =
  let w = fade (max 0G (1G - (abs (b - a)).sum / width))
  (b + a * w) / (1G + w)


/// Reflects components by shaping them with a wave function (e.g., sinr, trir).
/// Parameter a is the number of reflections.
let reflect (wave : float32 -> float32) (a : float32) = shape (fun x -> wave(x * a))


/// Reflects components by shaping them with a fade function fashioned into a periodic function.
/// Parameter a is the number of reflections.
let reflectf (fade : float32 -> float32) (a : float32) =
  let wave = Fade.sinefyr fade
  shape(fun x -> wave (a * 0.5f * x))


/// Reflects a vector proportional to its magnitude using a fade function fashioned into a periodic function.
/// Parameter a is the number of reflections.
let reflect3f (fade : float32 -> float32) (a : float32) =
  let wave = Fade.sinefyr fade
  fun (v : Vec3f) ->
    let m = v.length
    if m > 0G then
      v * (wave(a * 0.5f * m) / m)
    else Vec3f.zero


/// Saturates components. Amount a > 0 (typically, 1 < a < 10 for normalized inputs).
let overdrive (a : float32) =
  let Z = 1.0f / tanh a
  shape (fun x -> Z * tanh(x * a))


/// Saturates the input while retaining component proportions. Amount a > 0 (typically, 1 < a < 10 for normalized inputs).
let overdrive3 (a : float32) (v : Vec3f) =
  // Use the 8-norm as a smooth proxy for the largest magnitude component.
  let m = v.map(squared >> squared >> squared).sum |> sqrt |> sqrt |> sqrt
  if m > 0.0f then
    (tanh(m * a) / m) * v
  else
    Vec3f.zero


/// Crushes components with the specified number of levels per unit,
/// using the fade function to transition between levels.
let crush (fade : float32 -> float32) (levels : float32) =
  shape (fun x -> Fade.staircase fade (x * levels) / levels)


/// Crushes components proportional to the maximum component with the specified number of levels per unit,
/// using the fade function to transition between levels.
let crush3 (fade : float32 -> float32) (levels : float32) (v : Vec3f) =
  let m = (abs v).maximum * levels
  if m > 1.0e-6f then
    let c = (Fade.staircase fade m) / m
    v * c
  else
    Vec3f.zero


/// Makes a map divergence free. The result is incompressible when interpreted
/// as a velocity field, and thus ideal for realistic particle simulations.
/// In an incompressible field there are no sinks for particles to settle into.
/// For instance, both air and water are nearly incompressible fluids.
let curl (f : Map3) (v : Vec3f) =
  let epsilon = 1.0e-4f
  let u = f v
  let ux = (f (v + Vec3f(epsilon, 0G, 0G)) - u) / epsilon
  let uy = (f (v + Vec3f(0G, epsilon, 0G)) - u) / epsilon
  let uz = (f (v + Vec3f(0G, 0G, epsilon)) - u) / epsilon
  Vec3f(uy.z - uz.y, uz.x - ux.z, ux.y - uy.x)


/// Discovers component ranges of a map by stratified sampling inside the unit cube.
/// Always returns a non-empty range for each component, for convenience.
let range (f : Map3) =
  let dimension = 16
  let sampleZ (z : int) =
    let mutable minV = Vec3f(infinityf)
    let mutable maxV = Vec3f(-infinityf)
    let rnd = Rnd(z)
    for x = 0 to dimension - 1 do
      for y = 0 to dimension - 1 do
        let v = f ((Vec3f(G x, G y, G z) + rnd.vec3f()) / G dimension)
        minV <- Vec3f.minimize minV v
        maxV <- Vec3f.maximize maxV v
    Pair(minV, maxV)
  let mutable minV = Vec3f(infinityf)
  let mutable maxV = Vec3f(-infinityf)
  for (Pair(v0, v1)) in Array.Parallel.map sampleZ [| 0 .. dimension - 1 |] do
    minV <- Vec3f.minimize minV v0
    maxV <- Vec3f.maximize maxV v1
  let epsilon = 1.0e-3f
  (minV - abs minV * epsilon - Vec3f(epsilon), maxV + abs maxV * epsilon + Vec3f(epsilon))


/// Normalizes each component of a map to [-1, 1]. Estimates the range by sampling.
/// Clamps values outside the estimated range.
let normalize (f : Map3) =
  let mR, MR = range f
  let mM = average mR MR
  let mS = Vec3f(2G) / (MR - mR)
  fun v -> ((f v - mM) * mS).map(clamp11)

  
/// Normalizes each component of a basis to [-1, 1]. Estimates the range by sampling.
/// Clamps values outside the estimated range.
let normalizeBasis (f : Basis3) : Basis3 =
  // Instantiate the basis with an arbitrary high frequency.
  let mR, MR = range (f 200.0f)
  let mM = average mR MR
  let mS = Vec3f(2G) / (MR - mR)
  fun frequency v -> ((f frequency v - mM) * mS).map(clamp11)


/// Produces a permutation-reflection of components from a seed value, which is wrapped to the range [0, 48[.
let inline permute (seed : int) : Map3 =
  let sx = float32 (seed &&& 1) * 2.0f - 1.0f
  let sy = float32 (seed &&& 2) - 1.0f
  let sz = float32 (seed &&& 4) * 0.5f - 1.0f
  match emod (seed >>> 3) 6 with
  | 0 -> fun v -> Vec3f(sx * v.x, sy * v.y, sz * v.z)
  | 1 -> fun v -> Vec3f(sx * v.x, sy * v.z, sz * v.y)
  | 2 -> fun v -> Vec3f(sx * v.y, sy * v.x, sz * v.z)
  | 3 -> fun v -> Vec3f(sx * v.y, sy * v.z, sz * v.x)
  | 4 -> fun v -> Vec3f(sx * v.z, sy * v.x, sz * v.y)
  | _ -> fun v -> Vec3f(sx * v.z, sy * v.y, sz * v.x)


/// Normalizes a map and colors it with a pseudo-random palette. The output range is [-1, 1].
let color (seed : int) (f : Map3) =
  normalize f >> permute (mangle32 seed) >> Dna.generate(seed, ColorDna.genPalette 32)


/// Variable shapes v with u. Components of u provide parameters for
/// saturation (X), monoization (Y) and scattering (Z).
let variableShape (u : Vec3f) (v : Vec3f) =
  // Monoization.
  let mono = abs u.y
  let v = lerp v (Vec3f(v.average)) mono
  // Saturation.
  let S = 0.01f + squared u.x * 20.0f
  let Z = 1G / softsign S
  let v = v.map(fun x -> Z * softsign(x * S))
  // Scattering.
  v + u * abs u.z


/// Returns a single Fourier style basis function direction.
let fourierDirection (seed : int) (f : int) =
  let d = mangle12UnitVec3 seed
  let x = int (d.x * (float32 f + 0.5f))
  let y = int (d.y * (float32 f + 0.5f))
  let y = if abs y > f - abs x then sign y * (f - abs x) else y
  let z = (f - abs x - abs y) * (if d.z > 0.0f then 1 else -1)
  permute (mangle32d seed) (Vec3f(float32 x, float32 y, float32 z))


/// Produces a single Fourier style basis wave with frequency f (rounded down) measured along a cube diagonal.
/// Each distinct frequency has a unique appearance. Tiles the unit cube.
let fourierWave (f : float32) =
  let h = manglef64 f
  let f = int f
  let vi = fourierDirection (int (h >>> 32)) f
  let pv = Vec3f.fromSeed(int h)
  fun (v : Vec3f) ->
    let phi = v.x * vi.x + v.y * vi.y + v.z * vi.z
    (pv + Vec3f(phi)).sinr


/// Slightly more complicated basis function. Still quite fast. Tiles the unit cube.
let packetw (wavef : float32 -> float32) (f : float32) =
  let h = manglef64 f
  let f = int f
  let h2 = mangle64 h
  let h3 = mangle64 h2
  let vx = fourierDirection (int (h >>> 32)) f
  let vy = Vec3f(vx.y, -vx.z, vx.x)
  let vz = Vec3f(vx.z, -vx.x, -vx.y)
  let px = Vec3f.fromSeed(int h) * 2.0f - Vec3f(1.0f)
  let py = Vec3f.fromSeed(int (h2 >>> 32)) * 2.0f - Vec3f(1.0f)
  let pz = Vec3f.fromSeed(int h2) * 2.0f - Vec3f(1.0f)
  let po = Vec3f.fromSeed(int h3)
  fun (v : Vec3f) ->
    let phix = v *. vx
    let phiy = v *. vy
    let phiz = v *. vz
    px * wavef(phix + po.x) + py * wavef(phiy + po.y) + pz * wavef(phiz + po.z)


/// Packet basis function with the default, sine, wave function.
let packet (f : float32) = packetw sinr f


/// Packet basis function using a wave function derived from a fade function.
let packetf (fade : float32 -> float32) (f : float32) = packetw (Fade.sinefyr fade) f

    
/// Basic fractalizer. Samples many octaves of basis g.
/// Roughness is the attenuation for each doubling of frequency (typically < 1).
/// Lacunarity (lacunarity > 0) is the wavelength scaling of successive octaves.
let fractal (roughness : float32) (lacunarity : float32) (initialFrequency : float32) (octaves : int) (g : Basis3) =
  // Roughness per octave.
  let r = roughness ** (-log2 lacunarity)
  // Set initial weight to make the weights sum to unity.
  let w0 = 1.0f / Mat.geometricSum octaves 1.0f r
  // Instantiate basis functions.
  let g = Array.init octaves (fun i -> g (initialFrequency / pow lacunarity i))
  fun (v : Vec3f) ->
    let mutable result = Vec3f.zero
    let mutable F = initialFrequency
    let mutable w = w0
    for i = 0 to g.last do
      result <- result + w * g.[i] (v + Vec3f.fromSeed(mangle32 (i + manglef (float F))))
      w <- w * r
      F <- F / lacunarity
    result


/// Fractalizes basis g. Displaces successive samples with the walk operator.
let fractald (roughness : float32) (lacunarity : float32) (highpass : float32) (mix : MixOp) (walk : WalkOp) (initialFrequency : float32) (octaves : int) (g : Basis3) =
  assert (roughness > 0.0f && lacunarity > 0.0f && highpass >= 0.0f && highpass < 1.0f && initialFrequency > 0.0f)
  let r = roughness ** (-log2 lacunarity)
  // Set the initial weight so the most important octave has unity weight.
  let w0 = if r <= 1.0f then 1.0f else 1.0f / pow r (octaves - 1)
  let g = Array.init octaves (fun i -> g (initialFrequency / pow lacunarity i))
  fun (v : Vec3f) ->
    let mutable F = initialFrequency
    let mutable w = w0
    let mutable d = Vec3f.zero
    let mutable value = Mix.start
    let mutable h = 1.0f - highpass
    for i = 0 to g.last do
      let a = g.[i] (v + d)
      value <- mix value (w * h) a
      F <- F / lacunarity
      w <- w * r
      h <- sqrt h
      d <- walk d F a
    Mix.result value


/// Fractalizes basis g. Displacement is applied starting from the most detailed octave.
/// This tends to produce a billowy appearance, as opposed to creases.
let fractaldi roughness lacunarity lowpass mix walk initialFrequency octaves g =
  fractald roughness (1.0f / lacunarity) lowpass mix walk (initialFrequency / pow lacunarity (octaves - 1)) octaves g


/// Creates a lowpass, variable displaced and twisted version of a fractalized map.
/// The aforementioned parameters for each point are drawn from map h.
let multid roughness lacunarity minDisplace maxDisplace minTwist maxTwist initialFrequency minOctaves maxOctaves (mix : MixOp) (h : Map3) (g : Basis3) =
  enforce (0.0f < lacunarity && lacunarity < 1.0f) "Map3.multid: 0 < lacunarity < 1."

  let g = Array.init (int maxOctaves) (fun i -> g (initialFrequency / pow lacunarity i))
  let r = roughness ** (-log2 lacunarity)
  // Set the initial weight so the most important octave has unity weight.
  let w0 = if r <= 1.0f then 1.0f else 1.0f / pow r (int maxOctaves - 1)

  fun (v : Vec3f) ->
    let u = (h v).map(map11to01 >> clamp01)
    let octaves = lerp minOctaves maxOctaves u.x
    let displace = xerp minDisplace maxDisplace u.y
    let twist = lerp minTwist maxTwist u.z

    let mutable w = w0
    let mutable F = initialFrequency
    let mutable v = v
    let mutable rotation = Quaternionf.one
    let mutable value = Mix.start

    for i = 0 to int octaves - 1 do
      // Fade out smoothly the final octave.
      let P = Fade.smooth2(delerp01 (octaves - 1.0f) (octaves - 2.0f) (float32 i))
      if P > 0.0f then
        let y = g.[i] (v + Vec3f.fromSeed(mangle32 (i + manglef (float F))))
        let L2 = y.length2
        if L2 * twist > 1.0e-9f then
          let L = sqrt L2
          rotation <- Quaternionf(Vec3f(y.y, y.z, -y.x) / L, L * twist) * rotation
        v <- v + displace / F * y * rotation
        value <- mix value (w * P) y
      F <- F / lacunarity
      w <- w * r

    Mix.result value

