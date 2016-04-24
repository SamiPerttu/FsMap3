// Random number generation.
namespace FsMap3

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

  /// Generates a float32 in the left-closed interval [0, 1[.    
  member this.float() = Convert.float01 LeftClosed this.tick

  /// Generates a float32 in the left-closed interval [0, a[.
  member this.float(a) = a * Convert.float01 LeftClosed this.tick

  /// Generates a float32 in the closed interval [a, b].
  member this.float(a, b) = Convert.float01 Closed this.tick |> lerp a b

  /// Generates a float32 in the desired interval type in unit range.
  member this.float(interval) = Convert.float01 interval this.tick

  /// Generates a float32 in the desired interval type between 0 and a
  /// (note that if a < 0 then left-closed becomes right-closed and vice versa).
  member this.float(interval, a) = a * Convert.float01 interval this.tick

  /// Generates a float in the desired interval type between a and b
  /// (note that if b < a then left-closed becomes right-closed and vice versa).
  member this.float(interval, a, b) = Convert.float01 interval this.tick |> lerp a b

  /// Generates a float32 in the left-closed interval [0, 1[.    
  member this.float32() = Convert.float01 LeftClosed this.tick |> float32

  /// Generates a float32 in the left-closed interval [0, a[.
  member this.float32(a) = a * float32 (Convert.float01 LeftClosed this.tick)

  /// Generates a float32 in the closed interval [a, b].
  member this.float32(a, b) = Convert.float01 Closed this.tick  |> float32 |> lerp a b

  /// Generates a float32 in the desired interval type in unit range.
  member this.float32(interval) = Convert.float01 interval this.tick |> float32

  /// Generates a float32 in the desired interval type between 0 and a
  /// (note that if a < 0 then left-closed becomes right-closed and vice versa).
  member this.float32(interval, a) = a * float32 (Convert.float01 interval this.tick)

  /// Generates a float in the desired interval type between a and b
  /// (note that if b < a then left-closed becomes right-closed and vice versa).
  member this.float32(interval, a, b) = Convert.float01 interval this.tick |> float32 |> lerp a b

  /// Returns true with probability p.
  member this.boolean(p) = this.float() < p

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

  /// Generates a float that is exponentially (by which we mean log-linearly) distributed
  /// in the closed interval between minimum and maximum (minimum, maximum > 0).
  /// For example, exp(1.0, 4.0) generates values in [1, 2] and [2, 4] with equal probability.
  member this.exp(minimum, maximum) =
    assert (minimum > 0G && maximum > 0G)
    exp(this.float(log minimum, log maximum)) 

  /// Generates a float32 that is exponentially (by which we mean log-linearly) distributed
  /// in the closed interval between minimum and maximum (minimum, maximum > 0).
  /// For example, exp(1.0, 4.0) generates values in [1, 2] and [2, 4] with equal probability.
  member this.expf(minimum, maximum) =
    assert (minimum > 0G && maximum > 0G)
    exp(this.float32(log minimum, log maximum)) 

  /// Returns a 3-vector with each component uniformly distributed in the unit interval.
  member this.vec3f() = Vec3f(this.float32(), this.float32(), this.float32())

  /// Returns a 3-vector with each component transformed from the unit interval with the supplied function.
  member this.vec3f(f : float32 -> float32) = Vec3f(f <| this.float32(), f <| this.float32(), f <| this.float32())

  /// Returns a 3-vector with each component transformed from the unit interval of the given type
  /// with the supplied function.
  member this.vec3f(interval : Interval, f : float32 -> float32) = Vec3f(f <| this.float32(interval), f <| this.float32(interval), f <| this.float32(interval))

  /// Returns a unit length 3-vector.
  member this.unitVec3f() = Convert.unitVec3 this.tick this.tick

  /// Shuffles a sequence and returns it as an array.
  member this.shuffle(sequence : #seq<_>) =
    let a = Array.ofSeq sequence
    let n = a.size
    for i = 0 to n - 2 do
      let j = i + this.int(n - i)
      if j > i then a.swap(i, j)
    a

