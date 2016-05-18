/// Hashing functions. We call this "mangling" to avoid overloading the term "hashing" too much.
module FsMap3.Mangle

open Common
open Bits


/// Bob Jenkins' 32-bit hash. Feeds the input through a pseudo-random permutation.
let mangle32 (i : int) =
  let u = uint32 i
  let u = (u + 0x7ed55d16u) + (u <<< 12)
  let u = (u ^^^ 0xc761c23cu) ^^^ (u >>> 19)
  let u = (u + 0x165667b1u) + (u <<< 5)
  let u = (u + 0xd3a2646cu) ^^^ (u <<< 9)
  let u = (u + 0xfd7046c5u) + (u <<< 3)
  let u = (u ^^^ 0xb55a4f09u) ^^^ (u >>> 16)
  int u


/// Another 32-bit hash by Bob Jenkins. Feeds the input through a pseudo-random permutation.
let mangle32b (i : int) =
  let u = uint32 i
  let u = u - (u <<< 6)
  let u = u ^^^ (u >>> 17)
  let u = u - (u <<< 9)
  let u = u ^^^ (u <<< 4)
  let u = u - (u <<< 3)
  let u = u ^^^ (u <<< 10)
  let u = u ^^^ (u >>> 15)
  int (u ^^^ 0xb689b9adu) // XOR removes fixed point at 0


/// 32-bit hash by Austin Appleby. This is the final mixing hash of MurmurHash3.
let mangle32c (i : int) =
  let u = uint32 i
  let u = u ^^^ (u >>> 16)
  let u = u * 0x85ebca6bu
  let u = u ^^^ (u >>> 13)
  let u = u * 0xc2b2ae35u
  let u = u ^^^ (u >>> 16)
  int u


/// Yet another Bob Jenkins 32-bit hash. Feeds the input through a pseudo-random permutation.
let mangle32d (i : int) =
  let u = uint32 i
  let u = (u + 0x7fb9b1eeu) + (u <<< 12)
  let u = (u ^^^ 0xab35dd63u) ^^^ (u >>> 19)
  let u = (u + 0x41ed960du) + (u <<< 5)
  let u = (u + 0xc7d0125eu) ^^^ (u <<< 9)
  let u = (u + 0x071f9f8fu) + (u <<< 3)
  let u = (u ^^^ 0x55ab55b9u) ^^^ (u >>> 16)
  int u


/// Thomas Wang's 32-bit hash.
let mangle32e (i : int) =
  let u = uint32 i
  let u = ~~~u + (u <<< 15)
  let u = u ^^^ (u >>> 12)
  let u = u + (u <<< 2)
  let u = u ^^^ (u >>> 4)
  let u = u + (u <<< 3) + (u <<< 11)
  let u = u ^^^ (u >>> 16)
  int u


/// Fast, inlined Bob Jenkins 32-bit hash. Any collection of at least 11 low order bits is well mixed.
let inline mangle32fast (i : int) =
  let u = uint32 i
  let u = (u ^^^ 0xdeadbeefu) + (u <<< 4)
  let u = u ^^^ (u >>> 10)
  let u = u + (u <<< 7)
  let u = u ^^^ (u >>> 13)
  int u


/// Another fast, inlined Bob Jenkins 32-bit hash. It is well mixed if you use at least 17 low order bits.
let inline mangle32cheap (i : int) =
  let u = uint i ^^^ (uint i >>> 4)
  let u = (u ^^^ 0xdeadbeefu) + (u <<< 5)
  int (u ^^^ (u >>> 11))


/// Thomas Wang's 64-bit hash.
let mangle64 (i : int64) =
  let u = uint64 i
  let u = ~~~u + (u <<< 21)
  let u = u ^^^ (u >>> 24)
  let u = (u + (u <<< 3)) + (u <<< 8)
  let u = u ^^^ (u >>> 14)
  let u = (u + (u <<< 2)) + (u <<< 4)
  let u = u ^^^ (u >>> 28)
  let u = u + (u <<< 31)
  int64 u


/// 64-bit hash from a value convertible to float.
let inline manglef64 x = mangle64 (System.BitConverter.DoubleToInt64Bits (float x))


/// Replacement for hash<float>. This is many times faster than hash<float>.
let inline manglef (x : float) = let h = manglef64 x in int (h ^^^ (h >>> 32))


/// Replacement for hash<float32>. This is many times faster than hash<float>.
let inline manglef32 (x : float32) = manglef (float x)

let inline bitsf64 x = System.BitConverter.DoubleToInt64Bits (float x)
let mangleVec3f64 (v : Vec3f) = manglef64 (bitsf64 v.x + manglef64(bitsf64 v.y + manglef64(bitsf64 v.z)))


/// Bob Jenkins' "one-at-a-time" stream hash. If hashing strings, hash one character at a time.
let inline stream32 (hash : int) data =
  let u = uint (hash + int data)
  let u = u + (u <<< 10)
  int (u ^^^ (u >>> 6))

/// Final avalanche for Mangle.stream32. Call this after processing all data to enhance mixing.
let stream32end (hash : int) =
  let u = uint hash
  let u = u + (u <<< 3)
  let u = u ^^^ (u >>> 11)
  int (u + (u <<< 15))

/// Hashes a string using Mangle.stream32.
let mangleString (s : string) =
  let mutable h = 0
  for i = 0 to s.last do h <- stream32 h s.[i]
  stream32end h



type MCONF =
  static member inline DBITS = 12
  static member inline QBITS = 12

// A stratified set of random direction 3-vectors.
let directionArray =
  enforce (MCONF.DBITS % 2 = 0) "Mangle.directionArray: MCONF.DBITS must be even."
  let b = MCONF.DBITS / 2
  let m = (1 <<< b) - 1
  let M = (1 <<< 32 - b) - 1
  Array.init (1 <<< MCONF.DBITS) id
  |> Rnd(1).shuffle
  |> Array.map (fun i ->
    let x = ((i &&& m) <<< 32 - b) ||| (mangle32 i &&& M)
    let y = (((i >>> b) &&& m) <<< 32 - b) ||| (mangle32b i &&& M)
    Convert.unitVec3 x y
    )

// A stratified set of random 3-rotations.
let rotationArray =
  enforce (MCONF.QBITS % 3 = 0) "Mangle.rotationArray: MCONF.QBITS must be divisible by 3."
  let b = MCONF.QBITS / 3
  let m = (1 <<< b) - 1
  let M = (1 <<< 32 - b) - 1
  Array.init (1 <<< MCONF.QBITS) id
  |> Rnd(2).shuffle
  |> Array.map (fun i ->
    let x = ((i &&& m) <<< 32 - b) ||| (mangle32c i &&& M)
    let y = (((i >>> b) &&& m) <<< 32 - b) ||| (mangle32 i &&& M)
    let z = (((i >>> b * 2) &&& m) <<< 32 - b) ||| (mangle32b i &&& M)
    Convert.unitQuaternion x y z
    )


/// Hashes the lowest 12 bits of an integer to a unit 3-vector.
let inline mangle12UnitVec3 i = directionArray.[i &&& ((1 <<< MCONF.DBITS) - 1)]

/// Hashes the lowest 12 bits of an integer to a 3-rotation, returned as a single precision quaternion.
let inline mangle12Rotation i = rotationArray.[i &&& ((1 <<< MCONF.QBITS) - 1)]



/// 128-bit stream hash, with a 256-bit state.
/// This is the "short hash" version of SpookyHash by Bob Jenkins.
type Hash128 =
  {
    mutable a : uint64
    mutable b : uint64
    mutable c : uint64
    mutable d : uint64
    mutable phase : int // number of 32-bit words processed in the current 256-bit block
    mutable seed : int
  }

  // Hash result accessors. These are well mixed after a call to Hash128.hashEnd.

  /// 64-bit hash A.
  member inline this.a64 = int64 this.a
  /// 64-bit hash B.
  member inline this.b64 = int64 this.b
  /// 32-bit hash A.
  member inline this.a32 = int this.a
  /// 32-bit hash B.
  member inline this.b32 = int this.b
  /// 32-bit hash C.
  member inline this.c32 = int (this.a >>> 32)
  /// 32-bit hash D.
  member inline this.d32 = int (this.b >>> 32)

  /// Resets the hash. If the seed is not specified, then the existing seed will be used.
  member this.reset(?seed : int) =
    this.seed <- seed >? this.seed
    this.a <- uint64 this.seed
    this.b <- uint64 this.seed
    // The magic constant(s) below should be odd, and not be a very regular mix of 0s and 1s.
    this.c <- 0xdeadbeefdeadbeefUL
    this.d <- 0xdeadbeefdeadbeefUL
    this.phase <- 0

  /// Hashes an int.
  member this.hash(x : int) =
    match this.phase with
    | 0 -> this.a <- this.a + uint64 x
    | 1 -> this.a <- this.a + (uint64 x <<< 32)
    | 2 -> this.b <- this.b + uint64 x
    | 3 -> this.b <- this.b + (uint64 x <<< 32)
    | 4 -> this.c <- this.c + uint64 x
    | 5 -> this.c <- this.c + (uint64 x <<< 32)
    | 6 -> this.d <- this.d + uint64 x
    | _ -> this.d <- this.d + (uint64 x <<< 32)
           this.mix()
    this.phase <- (this.phase + 1) &&& 7

  /// Hashes an uint.
  member inline this.hash(x : uint) = this.hash(int x)

  /// Hashes an int64.
  member this.hash(x : int64) =
    // If necessary, we pad the state with a zero word to avoid having to split x into 32-bit halves.
    match this.phase with
    | 0 ->     this.a <- this.a + uint64 x
               this.phase <- 2
    | 1 | 2 -> this.b <- this.b + uint64 x
               this.phase <- 4
    | 3 | 4 -> this.c <- this.c + uint64 x
               this.phase <- 6
    | 5 | 6 -> this.d <- this.d + uint64 x
               this.mix()
               this.phase <- 0
    | _ ->     this.mix()
               this.a <- this.a + uint64 x
               this.phase <- 2

  /// Hashes an uint64.
  member this.hash(x : uint64) = this.hash(int64 x)

  /// Call this after hashing all input for final mixing.
  member this.hashEnd() =
    this.d <- this.d ^^^ this.c;  this.c <- rotateLeft this.c 15;  this.d <- this.d + this.c
    this.a <- this.a ^^^ this.d;  this.d <- rotateLeft this.d 52;  this.a <- this.a + this.d
    this.b <- this.b ^^^ this.a;  this.a <- rotateLeft this.a 26;  this.b <- this.b + this.a
    this.c <- this.c ^^^ this.b;  this.b <- rotateLeft this.b 51;  this.c <- this.c + this.b
    this.d <- this.d ^^^ this.c;  this.c <- rotateLeft this.c 28;  this.d <- this.d + this.c
    this.a <- this.a ^^^ this.d;  this.d <- rotateLeft this.d 9;   this.a <- this.a + this.d
    this.b <- this.b ^^^ this.a;  this.a <- rotateLeft this.a 47;  this.b <- this.b + this.a
    this.c <- this.c ^^^ this.b;  this.b <- rotateLeft this.b 54;  this.c <- this.c + this.b
    this.d <- this.d ^^^ this.c;  this.c <- rotateLeft this.c 32;  this.d <- this.d + this.c
    this.a <- this.a ^^^ this.d;  this.d <- rotateLeft this.d 25;  this.a <- this.a + this.d
    this.b <- this.b ^^^ this.a;  this.a <- rotateLeft this.a 63;  this.b <- this.b + this.a

  // This mixing operation is called every 256 bits.
  member private this.mix() =
    this.c <- rotateLeft this.c 50;  this.c <- this.c + this.d;  this.a <- this.a ^^^ this.c
    this.d <- rotateLeft this.d 52;  this.d <- this.d + this.a;  this.b <- this.b ^^^ this.d
    this.a <- rotateLeft this.a 30;  this.a <- this.a + this.b;  this.c <- this.c ^^^ this.a
    this.b <- rotateLeft this.b 41;  this.b <- this.b + this.c;  this.d <- this.d ^^^ this.b
    this.c <- rotateLeft this.c 54;  this.c <- this.c + this.d;  this.a <- this.a ^^^ this.c
    this.d <- rotateLeft this.d 48;  this.d <- this.d + this.a;  this.b <- this.b ^^^ this.d
    this.a <- rotateLeft this.a 38;  this.a <- this.a + this.b;  this.c <- this.c ^^^ this.a
    this.b <- rotateLeft this.b 37;  this.b <- this.b + this.c;  this.d <- this.d ^^^ this.b
    this.c <- rotateLeft this.c 62;  this.c <- this.c + this.d;  this.a <- this.a ^^^ this.c
    this.d <- rotateLeft this.d 34;  this.d <- this.d + this.a;  this.b <- this.b ^^^ this.d
    this.a <- rotateLeft this.a 5;   this.a <- this.a + this.b;  this.c <- this.c ^^^ this.a
    this.b <- rotateLeft this.b 36;  this.b <- this.b + this.c;  this.d <- this.d ^^^ this.b

  member this.copyFrom(hash : Hash128) =
    this.a <- hash.a
    this.b <- hash.b
    this.c <- hash.c
    this.d <- hash.d
    this.phase <- hash.phase
    this.seed <- hash.seed

  /// Creates a Hash128 with the given seed. Any integer is fine for a seed.
  /// Different seeds result in different hash functions.
  static member create(seed : int) =
    let hash = { Hash128.a = 0UL; b = 0UL; c = 0UL; d = 0UL; phase = 0; seed = seed }
    hash.reset()
    hash

  /// Creates a Hash128 with a default seed.
  static member create() = Hash128.create(0)


