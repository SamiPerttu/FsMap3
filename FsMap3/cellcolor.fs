/// Cell coloring functions.
module FsMap3.CellColor

open Common
open Mangle


/// A cell color function obtains a cell color from a cell hash. Component ranges are [-1, 1] as customary.
/// The cell hash should be considered dirty and, if used, rehashed with something other than
/// Mangle.mangle32, Mangle.mangle32b or Mangle.mangle32fast.
type CellColor = int -> Vec3f


/// Returns a unit length color.
let unitColor h = mangle12UnitVec3 <| mangle32c h

/// Returns a unit length color from n possible choices.
let unitColors n seed h = mangle12UnitVec3 <| mangle32c (seed + ((h &&& 0x7fffffff) % n))

/// Returns all ones.
let monoColor (_ : int) = Vec3f.one

/// Returns uniformly picked components.
let anyColor h = Vec3f.fromSeed(mangle32c h, -1.0f, 1.0f)

/// Returns a color from n possible choices with uniformly picked components.
let anyColors n seed h = Vec3f.fromSeed(mangle32c (seed + ((h &&& 0x7fffffff) % n)), -1.0f, 1.0f)

/// Returns a vector where every component is -1 or 1.
let fullColor h = let h = mangle32c h in Vec3f(float32 (h &&& 1) * 2.0f - 1.0f, float32 (h &&& 2) - 1.0f, float32 (h &&& 4) * 0.5f - 1.0f)

/// Returns a reflection vector except that one of the components may be zero.
let bigColor h =
  let h = mangle32c h
  let v =
    match (h >>> 3) &&& 3 with
    | 0 -> Vec3f(1.0f, 1.0f, 1.0f)
    | 1 -> Vec3f(0.0f, 1.0f, 1.0f)
    | 2 -> Vec3f(1.0f, 0.0f, 1.0f)
    | _ -> Vec3f(1.0f, 1.0f, 0.0f)
  v * Vec3f(float32 (h &&& 1) * 2.0f - 1.0f, float32 (h &&& 2) - 1.0f, float32 (h &&& 4) * 0.5f - 1.0f)

/// Returns a color where the absolute value of each component is at least d.
let highColor d h = Vec3f.fromSeed(mangle32c h, d - 1.0f, 1.0f - d).map(fun x -> if x < 0.0f then x - d else x + d)

/// Returns a color from n choices where the absolute value of each component is at least d.
let highColors n seed d h = Vec3f.fromSeed(mangle32c (seed + ((h &&& 0x7fffffff) % n)), d - 1.0f, 1.0f - d).map(fun x -> if x < 0.0f then x - d else x + d)

