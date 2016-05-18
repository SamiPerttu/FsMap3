// Pseudo-random permutations in constant space.
namespace FsMap3

open Common

(*
Here we provide a solution to the problem of generating pseudo-random permutations
of arbitrary size sets (well, up to some integral type maximum) in constant space.

Given set size N, we take the next power of two, call it B.
Next we create a permutation on B and discard any samples outside N.
Our filtering efficiency is at least 50%.

There are well known permutations for any power of two. All of the following are
defined modulo B. Given current index I in [0, B[ and a constant X:
 -Addition: I + X.
 -Exclusive or: I xor X.
 -Quadratic probing: X + (I + I * I) / 2.
 -Multiplication: I * X, where X is coprime with B, i.e., odd.

We combine these stages in a fixed pipeline. During initialization, values of X are
randomized from a seed value.
*)


/// Produces a pseudo-random permutation over [0, size[, given a seed value or Rnd.
/// If the size argument is provided to the constructor, the permutation will be initialized and ready to use.
type Permutation(?size : int, ?rnd : Rnd) =
  let mutable N = 0
  let mutable B = 0u
  let mutable I = 0u
  let mutable V = 0
  let mutable n = 0
  let mutable x1 = 0u
  let mutable x2 = 0u
  let mutable x3 = 0u
  let mutable x4 = 0u

  let initialize (rnd : Rnd) size =
    enforce (size > 0) "Permutation.initialize: Empty set."
    N <- size
    B <- Bits.ceilPower2 (uint32 N)
    let M = B - 1u
    I <- rnd.uint() &&& M
    V <- 0
    n <- 0
    x1 <- rnd.uint() &&& M
    x2 <- rnd.uint()
    x3 <- rnd.uint() ||| 1u
    x4 <- rnd.uint() &&& M

  do size.apply(initialize (rnd >? Rnd()))

  new(size, seed) = Permutation(size, Rnd(seed))

  /// The size of the permuted set.
  member this.size = N

  /// The current permuted value in [0, this.size[.
  member this.value = V

  /// The current permuted value in [0, this.size[.
  member this.Value = V

  /// Current number of values generated from the permutation (in [0, this.size]).
  member this.generated = n

  /// Initializes the permutation with the given seed. Subsequent calls to Permutation.next will generate the permutation.
  member this.reset(size, seed : int) = initialize (Rnd(seed)) size

  /// Initializes the permutation with the given RNG. Subsequent calls to Permutation.next will generate the permutation.
  member this.reset(size, rnd : Rnd) = initialize rnd size

  /// Generates the next value in the permutation and returns true, or
  /// returns false if the full permutation has been generated already.
  member this.next() =
    if n < N then
      // TODO: Check the transition matrix distribution and optimize the stages accordingly.
      let m = uint64 (B - 1u)
      // Stage 1: XOR.
      let v = uint64 (I ^^^ x1)
      // Stage 2: quadratic probe.
      let v = (((v + v * v) >>> 1) + uint64 x2) &&& m
      // Stage 3: multiplication.
      let v = (v * uint64 x3) &&& m
      // Stage 4: second XOR.
      let v = (v ^^^ uint64 x4)
      V <- int v
      // Since I cycles through B, we can employ the multiplication trick here by plugging in any odd increment.
      I <- (I + 0x5555u) &&& uint m
      if V >= N then
        this.next()
      else
        n <- n + 1
        true
    else
      false

