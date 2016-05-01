/// Profiling utilities.
module FsMap3.Profile

open Common


/// Generic, single threaded profiling function. The number of arguments looped over is n,
/// which must be a power of two (for example, 1024). Returns the number of calls per second.
let profile n (genArg : Rnd -> 'a) (f : 'a -> _) =
  enforce (Bits.isPowerOf2 n) "Profile.profile: Number of arguments must be a power of 2."
  let mask = n - 1
  let rnd = Rnd(1)
  let a = Array.init n (fun _ -> genArg rnd)
  let t0 = Common.timeNow()
  let mutable t1 = Common.timeNow()
  let mutable calls = 0
  let mutable callsPerLoop = 1
  let mutable i = 0
  while t1 - t0 < 1.0 do
    // Try to run in a tight inner loop to get accurate measurements.
    if callsPerLoop = n then
      for j = 0 to mask do ignore <| f a.[j]
      calls <- calls + n
    else
      for __ = 1 to callsPerLoop do
        ignore <| f a.[i]
        i <- (i + 1) &&& mask
      calls <- calls + callsPerLoop
      callsPerLoop <- mini n (callsPerLoop <<< 1)
    t1 <- Common.timeNow()
  float calls / (t1 - t0)


/// Profiles a Vec3 function with random arguments from the unit cube.
let profileVec3 (f : Vec3 -> _) = profile (1 <<< 10) (fun rnd -> Vec3.fromSeed(rnd.tick)) f

/// Profiles a Vec3 function with random arguments from the unit cube.
let profileVec3f (f : Vec3f -> _) = profile (1 <<< 10) (fun rnd -> Vec3f.fromSeed(rnd.tick)) f

/// Profiles a float function with random arguments from the open unit interval ]0, 1[.
let profileFloat (f : float -> _) = profile (1 <<< 10) (fun rnd -> rnd.float(Interval.Open)) f

/// Profiles a float32 function with random arguments from the open unit interval ]0, 1[.
let profileFloat32 (f : float32 -> _) = profile (1 <<< 10) (fun rnd -> rnd.float32(Interval.Open)) f

/// Profiles an int function with random arguments.
let profileInt (f : int -> _) = profile (1 <<< 10) (fun rnd -> rnd.tick) f

/// Profiles a uint function with random arguments.
let profileUint (f : uint -> _) = profile (1 <<< 10) (fun rnd -> rnd.uint()) f

/// Profiles a function taking no arguments.
let profileUnit (f : unit -> _) = profile 1 (fun _ -> 0) (fun _ -> f())

/// Profiles a function with a fixed argument.
let profileFixed (x : 'a) (f : 'a -> _) = profile 1 (fun _ -> x) f

/// Profiles an int64 function with random arguments.
let profileInt64 (f : int64 -> _) = profile (1 <<< 10) (fun rnd -> rnd.int64()) f

/// Profiles a uint64 function with random arguments.
let profileUint64 (f : uint64 -> _) = profile (1 <<< 10) (fun rnd -> rnd.uint64()) f

/// Profiles a generator function with random Dna.
let profileDna (f : Dna -> _) =
  let dna = Dna.create()
  profile 256 (fun rnd -> RandomSource(rnd)) (fun source -> dna.generate(source, f))

