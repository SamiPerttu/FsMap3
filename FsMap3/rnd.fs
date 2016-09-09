// Random number generation.
namespace Fuse

open Common


// Constants for the random number generator.
type RndConfig =
  static member inline F = 196314165u
  static member inline A = 907633515u


/// Pseudo-random entropy source using a LFSR (linear feedback shift register).
/// It has 16 bytes of RNG state and a period of about 2^113.
type Rnd(?seed) =

  // For more information, see
  // L'Ecuyer, P., Tables of Maximally-Equidistributed Combined LFSR Generators.
  // Mathematics of Computation, 68, 225 (1999), pp. 261--269.

  let mutable sd = seed >? 1
  let mutable z1 = 0u
  let mutable z2 = 0u
  let mutable z3 = 0u
  let mutable z4 = 0u

  let next() =
    let b1 = (((z1 <<<  6) ^^^ z1) >>> 13)
    z1 <- (((z1 &&& 4294967294u) <<< 18) ^^^ b1)
    let b2 = (((z2 <<<  2) ^^^ z2) >>> 27)
    z2 <- (((z2 &&& 4294967288u) <<< 2) ^^^ b2)
    let b3 = (((z3 <<< 13) ^^^ z3) >>> 21)
    z3 <- (((z3 &&& 4294967280u) <<< 7) ^^^ b3)
    let b4 = (((z4 <<<  3) ^^^ z4) >>> 12)
    z4 <- (((z4 &&& 4294967168u) <<< 13) ^^^ b4)
    z1 ^^^ z2 ^^^ z3 ^^^ z4

  let init() =
    z1 <- uint32 sd * RndConfig.F + RndConfig.A
    // The constraints are designed to avoid low-entropy states.
    if z1 < 2u then z1 <- z1 ^^^ 587186514u
    // Using the seed a second time should be enough to make every seed result in a distinct state
    // (although, I haven't verified this).
    z2 <- ((z1 + uint32 sd) * RndConfig.F) + RndConfig.A
    if z2 < 8u then z2 <- z2 ^^^ 142871867u
    z3 <- (z2 * RndConfig.F) + RndConfig.A
    if z3 < 16u then z3 <- z3 ^^^ 653748255u
    z4 <- (z3 * RndConfig.F) + RndConfig.A
    if z4 < 128u then z4 <- z4 ^^^ 613278213u

    // Burn-in.
    for i = 1 to 4 do next() |> ignore

  do init()

  member this.seed = sd
  member this.state1 = z1
  member this.state2 = z2
  member this.state3 = z3
  member this.state4 = z4

  /// Resets the random number generator to its initial state, optionally with a new seed.
  member this.reset(?seed) =
    sd <- seed >? sd
    init()

  /// Mixes the state of the generator with data from another Rnd.
  member this.mix(rnd : Rnd) =
    // We use the same constraints for state as during initialization.
    z1 <- z1 ^^^ rnd.uint()
    if z1 < 2u then z1 <- z1 ^^^ 587186514u
    z2 <- z2 ^^^ rnd.uint()
    if z2 < 8u then z2 <- z2 ^^^ 142871867u
    z3 <- z3 ^^^ rnd.uint()
    if z3 < 16u then z3 <- z3 ^^^ 653748255u
    z4 <- z4 ^^^ rnd.uint()
    if z4 < 128u then z4 <- z4 ^^^ 613278213u

  /// Mixes the state of the generator using the given seed.
  member this.mix(seed : int) = this.mix(Rnd(seed))

  /// Copies the state of the generator from another generator.
  /// The initial seed is copied as well.
  member this.copyFrom(rnd : Rnd) =
    sd <- rnd.seed
    z1 <- rnd.state1
    z2 <- rnd.state2
    z3 <- rnd.state3
    z4 <- rnd.state4

  /// Generates a uint.  
  member this.uint() = next()
    
  /// Generates an int.
  member this.int() = next() |> int

  /// Generates an int.
  member this.tick = next() |> int

  /// Generates a uint.
  member this.utick = next()

  /// Generates an int64.
  member this.int64() = (int64 (next()) <<< 32) ||| int64 (next())

  /// Generates a uint64.
  member this.uint64() = (uint64 (next()) <<< 32) ||| uint64 (next())

  /// Generates an unsigned integer in [0, limit[ (limit > 0).
  /// Does not guarantee an exactly uniform distribution - the higher the limit, the greater the maximum error.
  member this.uint(limit) =
    enforce (limit > 0u) "Rnd.uint: Limit must be greater than zero."
    this.uint() % limit

  /// Generates an unsigned integer in [minimum, maximum] (minimum <= maximum).
  /// Does not guarantee an exactly uniform distribution - the larger the range, the greater the potential error.
  member this.uint(minimum, maximum) =
    enforce (minimum <= maximum) "Rnd.uint: Empty range."
    let range = maximum - minimum + 1u
    if range <> 0u then minimum + this.uint(range) else this.uint()

  /// Generates an integer in [0, limit[ (limit > 0).
  /// Does not guarantee an exactly uniform distribution - the higher the limit, the greater the potential error.
  member this.int(limit : int) =
    enforce (limit > 0) "Rnd.int: Limit must be greater than zero."
    int <| this.uint() % uint limit

  /// Generates an integer in [minimum, maximum] (minimum <= maximum).
  /// Does not guarantee an exactly uniform distribution - the larger the range, the greater the potential error.
  member this.int(minimum, maximum) =
    enforce (minimum <= maximum) "Rnd.int: Empty range."
    let range = uint maximum - uint minimum + 1u
    if range <> 0u then
      minimum + int (this.uint(range))
    else
      // The full integer range was specified.
      this.int()

  /// Generates an unsigned integer in [0, limit[ (limit > 0).
  /// Uses rejection sampling to ensure an exactly uniform distribution.
  member this.exact(limit : uint) =
    enforce (limit > 0u) "Rnd.exact: Limit must be greater than zero."
    if limit < 2u then 0u else
      let q = uint (0x100000000L % int64 limit)
      let rec loop() =
        let v = this.uint()
        if v < q then loop() else v
      loop() % limit

  /// Generates an integer in [0, limit[ (limit > 0).
  /// Uses rejection sampling to ensure an exactly uniform distribution.
  member this.exact(limit : int) =
    enforce (limit > 0) "Rnd.exact: Limit must be greater than zero."
    int <| this.exact(uint limit)

  /// Generates an integer in [minimum, maximum] (minimum <= maximum).
  /// Uses rejection sampling to ensure an exactly uniform distribution.
  member this.exact(minimum, maximum) =
    enforce (minimum <= maximum) "Rnd.exact: Empty range."
    let range = uint maximum - uint minimum + 1u
    if range <> 0u then
      minimum + int (this.exact(range))
    else
      // Full integer range was specified.
      this.int()

  /// Generates a float in the left-closed interval [0, 1[.    
  member this.float() = Convert.float01 LeftClosed this.tick

  /// Generates a float in the left-closed interval [0, a[.
  member this.float(a) = a * Convert.float01 LeftClosed this.tick

  /// Generates a float in the closed interval [a, b].
  member this.float(a, b) = Convert.float01 Closed this.tick |> lerp a b

  /// Generates a float in the desired interval type in unit range.
  member this.float(interval) = Convert.float01 interval this.tick

  /// Generates a float in the desired interval type between 0 and a
  /// (note that if a < 0 then left-closed becomes right-closed and vice versa).
  member this.float(interval, a) = a * Convert.float01 interval this.tick

  /// Generates a float in the desired interval type between a and b
  /// (note that if b < a then left-closed becomes right-closed and vice versa).
  member this.float(interval, a, b) = Convert.float01 interval this.tick |> lerp a b

  /// Generates a float32 in the left-closed interval [0, 1[.    
  member this.float32() = Convert.float01 LeftClosed this.tick |> float32

  /// Generates a float32 in the left-closed interval [0, a[.
  member this.float32(a) = Convert.float01 LeftClosed this.tick |> float32 |> (*) a

  /// Generates a float32 in the closed interval [a, b].
  member this.float32(a, b) = Convert.float01 Closed this.tick  |> float32 |> lerp a b

  /// Generates a float32 in the desired interval type in unit range.
  member this.float32(interval) = Convert.float01 interval this.tick |> float32

  /// Generates a float32 in the desired interval type between 0 and a
  /// (note that if a < 0 then left-closed becomes right-closed and vice versa).
  member this.float32(interval, a) = Convert.float01 interval this.tick |> float32 |> (*) a

  /// Generates a float32 in the desired interval type between a and b
  /// (note that if b < a then left-closed becomes right-closed and vice versa).
  member this.float32(interval, a, b) = Convert.float01 interval this.tick |> float32 |> lerp a b

  /// Returns true with probability p.
  member this.boolean(p) = this.float() < p

  /// Flips a fair coin. Returns true or false.
  member this.flip = this.uint() &&& 128u = 0u

  /// Chooses a with frequency fa and b with frequency fb (fa, fb >= 0).
  member this.choose(fa, a, fb, b) =
    assert(fa >= 0.0 && fb >= 0.0)
    if this.float(fa + fb) < fa then a else b

  /// Chooses a with frequency fa, b with frequency fb, and c with frequency fc (fa, fb, fc >= 0).
  member this.choose(fa, a, fb, b, fc, c) =
    assert(fa >= 0.0 && fb >= 0.0 && fc >= 0.0)
    let x = this.float(fa + fb + fc)
    if x < fa then a elif x < fa + fb then b else c

  /// Chooses a with frequency fa, b with frequency fb, c with frequency fc,
  /// and d with frequency fd (fa, fb, fc, fd >= 0).
  member this.choose(fa, a, fb, b, fc, c, fd, d) =
    assert(fa >= 0.0 && fb >= 0.0 && fc >= 0.0)
    let x = this.float(fa + fb + fc + fd)
    if x < fa then a elif x < fa + fb then b elif x < fa + fb + fc then c else d

  /// Chooses an element from a finite sequence in a single pass. The probability of an element
  /// being chosen is proportional to its frequency, which is obtained from the given function.
  /// Zero frequencies are allowed, as long as at least one element has a non-zero frequency.
  member this.chooseFrom(sequence : #seq<'a>, frequencyf : 'a -> float) =
    let foldf (Pair(x, F : float) as state) (x' : 'a) =
      let f = frequencyf x'
      if f > 0.0 then
        let F' = F + f
        Pair((if this.float(F') < F then x else x'), F')
      else state
    let (Pair(x, F)) = Seq.fold foldf (Pair(Unchecked.defaultof<'a>, 0.0)) sequence
    enforce (F > 0.0) "Rnd.chooseFrom: All frequencies are zero."
    x

  /// Chooses an element from a range of integers, given a frequency function.
  /// If all frequencies are zero, returns a random choice.
  member this.chooseFrom(i0, i1, frequencyf : int -> float) =
    enforce (i0 <= i1) "Rnd.chooseFrom: Empty range."
    let mutable F = 0.0
    let mutable x = i0
    for i = i0 to i1 do
      let f = frequencyf i
      if f > 0.0 then
        let F' = F + f
        x <- if this.float(F') < F then x else i
        F <- F'
    if F > 0.0 then x else this.exact(i0, i1)

  /// Samples the Gaussian distribution N(0, 1).
  member this.gaussian() = (Convert.gaussian this.tick this.tick).fst

  /// Returns a pair of standard normal Gaussians for the price of one.
  member this.gaussianPair() = Convert.gaussian this.tick this.tick

  /// Samples the geometric distribution Geom(p) where p is the probability of success (p > 0).
  /// The mean of the samples is (1 - p) / p. Thus, p = 1 / (1 + mean).
  member this.geometric(p) = Convert.geometric p this.tick

  /// Samples the Poisson distribution Pois(lambda) using Knuth's iterative algorithm (lambda > 0).
  /// The running time is O(lambda). Not suitable for large values of lambda.
  member this.poisson(lambda) =
    let L = exp(-lambda)
    let mutable p = 1.0
    let mutable k = 0
    while p > L do
      p <- p * this.float(Interval.Closed)
      k <- k + 1
    k - 1

  /// Optimized method for sampling the Poisson distribution Pois(1). The maximum returned value is 12.
  member this.poisson1() = Convert.poisson1 this.utick

  /// Generates a float that is exponentially (by which we mean log-linearly) distributed
  /// in the closed interval between minimum and maximum (minimum, maximum > 0).
  /// For example, exp(1.0, 4.0) generates values in [1, 2] and [2, 4] with equal probability.
  member this.exp(minimum, maximum) =
    assert (minimum > 0G && maximum > 0G)
    exp(this.float(log minimum, log maximum)) 

  /// Generates a float32 that is exponentially (by which we mean log-linearly) distributed
  /// in the closed interval between minimum and maximum (minimum, maximum > 0).
  /// For example, exp(1.0, 4.0) generates values in [1, 2] and [2, 4] with equal probability.
  member this.exp(minimum, maximum) =
    assert (minimum > 0G && maximum > 0G)
    exp(this.float32(log minimum, log maximum)) 

  /// Samples the exponential probability distribution with the given mean.
  /// Note that this is different from Rnd.exp.
  member this.exponential(mean) = -log(this.float(RightClosed)) / mean

  /// Samples the Rayleigh distribution with the given deviation.
  member this.rayleigh(deviation) = sqrt(-0.5 / deviation * log(this.float(RightClosed)))

  /// Samples the Weibull distribution with shape parameter k > 0 and scale parameter lambda > 0.
  member this.weibull(k, lambda) = -lambda * log(-this.float(RightClosed)) ** (1.0 / k)
    
  /// Returns a 3-vector with each component uniformly distributed in the unit interval.
  member this.vec3() = Vec3(this.float(), this.float(), this.float())

  /// Returns a float32 3-vector with each component uniformly distributed in the unit interval.
  member this.vec3f() = Vec3f(this.float32(), this.float32(), this.float32())

  /// Returns a 3-vector with each component transformed from the unit interval with the supplied function.
  member this.vec3(f : float -> float) = Vec3(f <| this.float(), f <| this.float(), f <| this.float())

  /// Returns a 3-vector with each component transformed from the unit interval with the supplied function.
  member this.vec3f(f : float32 -> float32) = Vec3f(f <| this.float32(), f <| this.float32(), f <| this.float32())

  /// Returns a 3-vector with each component transformed from the unit interval of the given type
  /// with the supplied function.
  member this.vec3(interval : Interval, f : float -> float) = Vec3(f <| this.float(interval), f <| this.float(interval), f <| this.float(interval))

  /// Returns a 3-vector with each component transformed from the unit interval of the given type
  /// with the supplied function.
  member this.vec3f(interval : Interval, f : float32 -> float32) = Vec3f(f <| this.float32(interval), f <| this.float32(interval), f <| this.float32(interval))

  /// Returns a 2-vector with the components uniformly distributed in the unit interval.
  member this.vec2() = Vec2(this.float(), this.float())

  /// Shuffles a sequence. Returns the result as an array.
  member this.shuffle(sequence : #seq<_>) =
    let a = Array.ofSeq sequence
    for i = 0 to a.size - 2 do
      let j = i + this.int(a.size - i)
      if j > i then a.swap(i, j)
    a

  /// Returns a unit length 2-vector.
  member this.unitVec2() = Vec2.direction(this.float(pi2))

  /// Returns a unit length 3-vector.
  member this.unitVec3() = Convert.unitVec3 this.tick this.tick

  /// Returns a unit length 3-vector.
  member this.unitVec3f() = Convert.unitVec3f this.tick this.tick

  /// Returns a unit length 4-vector as a quaternion.
  member this.unitQuaternion() = Convert.unitQuaternion this.tick this.tick this.tick

