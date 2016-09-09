/// Bit manipulation utilities and tricks.
module Fuse.Bits

open Common


// Note. Many functions in this module make use of the fact that the bit shift operator in F#
// (and C#) considers only the low order bits of the shift amount, ignoring the rest.
// For example, 1 <<< 1 is equal to 1 <<< 257 is equal to 1 <<< -31.


/// Returns whether x is a power of two.
let inline isPowerOf2 x = x > 0G && (x &&& (x - 1G)) = 0G


/// Returns x with only the lowest bit set. Returns 0 for 0.
let inline lowestBitValue x =
  // For signed types, the following is equivalent to x &&& -x (two's complement).
  x &&& (~~~x + 1G)


/// Returns an arbitrary integral type filled with the n-bit pattern x,
/// with one copy located in the least significant bits.
let inline pattern n x =
  let mutable p = 0G
  let mutable p' = x
  let mutable n = n
  while p' <> p do
    p <- p'
    p' <- (p' <<< n) ||| p'
    n <- n + n
  p'


/// Rounds a 32-bit integer down to a power of two. Returns 0 for 0.
let floorPower2 (x : uint32) =
  let x = x ||| (x >>> 1)
  let x = x ||| (x >>> 2)
  let x = x ||| (x >>> 4)
  let x = x ||| (x >>> 8)
  let x = x ||| (x >>> 16)
  x ^^^ (x >>> 1)


/// Rounds a 32-bit integer up to a power of two. Throws an exception if the result would overflow.
let ceilPower2 (x : uint32) =
  enforce (x <= 0x80000000u) "Bits.ceilPower2: Arithmetic overflow."
  if x = 0u then 1u else
    let x = x - 1u
    let x = x ||| (x >>> 1)
    let x = x ||| (x >>> 2)
    let x = x ||| (x >>> 4)
    let x = x ||| (x >>> 8)
    let x = x ||| (x >>> 16)
    x + 1u


/// Returns the bit reverse of a 32-bit integer.
let reverse32 (x : uint32) =
  let x = ((x &&& 0x55555555u) <<< 1) ||| ((x &&& 0xaaaaaaaau) >>> 1)
  let x = ((x &&& 0x33333333u) <<< 2) ||| ((x &&& 0xccccccccu) >>> 2)
  let x = ((x &&& 0x0f0f0f0fu) <<< 4) ||| ((x &&& 0xf0f0f0f0u) >>> 4)
  let x = ((x &&& 0x00ff00ffu) <<< 8) ||| ((x &&& 0xff00ff00u) >>> 8)
  ((x &&& 0x0000ffffu) <<< 16) ||| ((x &&& 0xffff0000u) >>> 16)


/// Returns the number of one bits in a 32-bit integer.
let oneBits32 (x : uint32) =
  // The overall approach is an application of the fact that isolated bit groups can be summed in parallel
  // (aka "SIMD in a register"). The following special trick saves one mask operation (compare to the next line).
  let x = x - ((x >>> 1) &&& 0x55555555u)
  let x = (x &&& 0x33333333u) + ((x >>> 2) &&& 0x33333333u)
  // Multiplication here acts as a convolution of bit groups.
  int (((x + (x >>> 4) &&& 0xf0f0f0fu) * 0x1010101u) >>> 24)


/// Returns the number of one bits in a 64-bit integer.
let oneBits64 (v : uint64) = 
  let m1 = 0x5555555555555555UL
  let m2 = 0x3333333333333333UL
  let m4 = 0x0f0f0f0f0f0f0f0fUL
  let m5 = 0x0101010101010101UL
  let v = v - ((v >>> 1) &&& m1)
  let v = (v &&& m2) + ((v >>> 2) &&& m2)
  let v = (v + (v >>> 4)) &&& m4
  int ((v * m5) >>> 56)


/// The Hamming distance between 32-bit x and y.
let inline hamming32 (x : uint32) (y : uint32) = oneBits32 (x ^^^ y)


/// The Hamming distance between 64-bit x and y.
let inline hamming64 (x : uint64) (y : uint64) = oneBits64 (x ^^^ y)


// Magic numbers.
type BITS =
  static member inline N32_MAGIC = 0x077cb531u // any 5-bit De Bruijn sequence that starts from zero works here
  static member inline N64_MAGIC = 0x022fdd63cc95386dUL // ditto for 6-bit sequences
  static member inline N32_SHIFT = 27
  static member inline N64_SHIFT = 58


let oneBits8Array = [| for i in 0u .. 0xffu -> int8 (oneBits32 i) |]

/// Returns the number of one bits in a byte. Accepts bigger argument types but all except lowest 8 bits must be zero.
let inline oneBits8 x = int oneBits8Array.[int x]


let bitReverse8Array = [| for i in 0u .. 0xffu -> uint8 (reverse32 i >>> 24) |]

/// Returns the bit reverse of a byte. Accepts bigger argument types but all except lowest 8 bits must be zero.
let inline reverse8 x = bitReverse8Array.[int x]


// Bit number formulae from Wikipedia. These are a simple application of modular arithmetic
// on a De Bruijn sequence. (Most modern CPUs feature a lowest bit instruction. But there
// appears to be no way to invoke it in .NET. Although, many CPUs seem to implement
// the instruction as a linear microcode loop, which is not helpful at all.)
let bitNumber32Array =
  let a = Array.zeroCreate<byte> 32
  for i = 0 to a.last do a.[int ((BITS.N32_MAGIC * (1u <<< i)) >>> BITS.N32_SHIFT)] <- byte i
  a
let bitNumber64Array =
  let a = Array.zeroCreate<byte> 64
  for i = 0 to a.last do a.[int ((BITS.N64_MAGIC * (1UL <<< i)) >>> BITS.N64_SHIFT)] <- byte i
  a


/// Returns the number of the lowest bit set in a 32-bit word. Returns 0 for 0.
let inline lowestBit32 x =
  int bitNumber32Array.[int ((uint32 (int x &&& (-int x)) * BITS.N32_MAGIC) >>> BITS.N32_SHIFT)]


/// Returns the number of the lowest bit set in a 64-bit word. Returns 0 for 0.
let inline lowestBit64 x =
  int bitNumber64Array.[int ((uint64 (int64 x &&& (-int64 x)) * BITS.N64_MAGIC) >>> BITS.N64_SHIFT)]


/// Returns the number of the highest bit set in a 32-bit word.
/// This is equivalent to the truncated base-2 logarithm. Returns 0 for 0.
let highestBit32 (x : uint) =
    let x = x ||| (x >>> 1)
    let x = x ||| (x >>> 2)
    let x = x ||| (x >>> 4)
    let x = x ||| (x >>> 8)
    let x = x ||| (x >>> 16)
    int bitNumber32Array.[int (((x ^^^ (x >>> 1)) * BITS.N32_MAGIC) >>> BITS.N32_SHIFT)]


/// Returns the number of the highest bit set in a 64-bit word.
/// This is equivalent to the truncated base-2 logarithm. Returns 0 for 0.
let highestBit64 (x : uint64) =
    let x = x ||| (x >>> 1)
    let x = x ||| (x >>> 2)
    let x = x ||| (x >>> 4)
    let x = x ||| (x >>> 8)
    let x = x ||| (x >>> 16)
    let x = x ||| (x >>> 32)
    int bitNumber64Array.[int (((x ^^^ (x >>> 1)) * BITS.N64_MAGIC) >>> BITS.N64_SHIFT)]


/// Returns the number of the single bit set in the 32-bit word; in other words, its binary logarithm.
/// Returns 0 for 0. Return value is undefined if more than a single bit is set.
let inline bitNumber32 x =
  int bitNumber32Array.[int (uint x * BITS.N32_MAGIC >>> BITS.N32_SHIFT)]


/// Returns the number of the single bit set in the 64-bit word; in other words, its binary logarithm.
/// Returns 0 for 0. Return value is undefined if more than a single bit is set.
let inline bitNumber64 x =
  int bitNumber64Array.[int (uint64 x * BITS.N64_MAGIC >>> BITS.N64_SHIFT)]


/// Converts a 32-bit word to its (binary reflected) Gray code.
let inline gray (u : uint) =
  // Intuitively, the XOR operation counteracts the cascading effects of bit carrying.
  u ^^^ (u >>> 1)


/// Converts a (binary reflected) Gray code back to integer.
let degray (u : uint) = 
  let u = u ^^^ (u >>> 16)
  let u = u ^^^ (u >>> 8)
  let u = u ^^^ (u >>> 4)
  let u = u ^^^ (u >>> 2)
  u ^^^ (u >>> 1)


/// Computes the parity of a 32-bit word: returns 0 if the word contains an even number of 1 bits, and 1 otherwise.
let parity32 (x : uint) =
  // Compute the parity of each nybble in parallel.
  let x = x ^^^ (x >>> 1)
  let x = x ^^^ (x >>> 2)
  // Mask nybble parities and convolve them together in the most significant nybble.
  let x = (x &&& 0x11111111u) * 0x11111111u
  int (x >>> 28) &&& 1


/// Swaps two non-overlapping sets of bits. The first set is specified by the one bits in the mask,
/// and the second set is the first shifted to the left by distance bits.
/// This operation can be handy if we want to shuffle a word while retaining the number of one bits.
let inline swapBits32 mask distance (x : uint) =
  assert (mask &&& (mask <<< distance) = 0u)
  let y = (x ^^^ (x >>> distance)) &&& mask
  x ^^^ (y ||| (y <<< distance))


/// Computes and returns the (lexicographically and arithmetically) next bit permutation. That is,
/// the next higher number with the same number of one bits. Returns 0xffffffff when the maximum
/// value has been reached.
let inline nextBitPermutation32 (x : uint) =
  let y = (x ||| (x - 1u)) + 1u
  y ||| ((((uint (int y &&& -int y)) / (uint (int x &&& -int x))) >>> 1) - 1u)


/// Rotates unsigned integer x by k bits to the left.
let inline rotateLeft x k = forceUnsigned x; (x <<< k) ||| (x >>> -k)


/// Constructs the range [i0, i1] of set bits in a 32-bit word. Both arguments are considered modulo 32.
/// Returns zero if i0 > i1 (mod 32).
let inline range32 (i0 : int) (i1 : int) = (~~~0u <<< i0) &&& (~~~0u >>> ~~~i1)


/// Constructs the range [i0, i1] of set bits in a 64-bit word. Both arguments are considered modulo 64.
/// Returns zero if i0 > i1 (mod 64).
let inline range64 (i0 : int) (i1 : int) = (~~~0UL <<< i0) &&& (~~~0UL >>> ~~~i1)


/// Returns a 64-bit word with n (mod 64) 1s in the least significant bits. The rest are set to 0.
let inline ones64 n = ~~~(~~~0UL <<< n)


/// Returns a 64-bit word with n (mod 64) 0s in the least significant bits. The rest are set to 1.
let inline zeros64 n = ~~~0UL <<< n


// Some generic bit manipulation functions.
open LanguagePrimitives


/// Rounds x down to a power of two. Returns 0 for 0.
let inline genericFloorPower2 x =
  let mutable p  = 0G
  let mutable p' = x
  let mutable n  = 1
  while p' <> p do
    p  <- p'
    p' <- p' ||| (p' >>> n)
    n  <- n + n
  p' ^^^ (p' >>> 1)


/// Rounds x up to a power of two. Throws an exception if the result would overflow.
let inline genericCeilPower2 x =
  if x <= 1G then 1G else
    let mutable p  = 0G
    let mutable p' = x - 1G
    if (p' <<< 1) < x then System.OverflowException() |> raise
    let mutable n = 1
    while p' <> p do
      p <- p'
      p' <- p' ||| (p' >>> n)
      n <- n + n
    p' + 1G

