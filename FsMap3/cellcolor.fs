/// Cell coloring functions.
module FsMap3.CellColor

open Common
open Mangle


/// A cell color function obtains a cell color from a cell hash and a position vector.
/// The position inputs are centered around the origin.
/// The cell hash should be considered dirty and, if used, rehashed with something other than
/// Mangle.mangle32, Mangle.mangle32b or Mangle.mangle32fast.
type CellColor = int -> Vec3f -> Vec3f


/// Picks a uniformly distributed color.
let anyColor h (_ : Vec3f) =
  Vec3f.fromSeed(mangle32c h, -1.0f, 1.0f)

/// Picks a color from n possible choices with uniformly picked components.
let anyColors n seed h (_ : Vec3f) =
  Vec3f.fromSeed(mangle32c (seed + ((h &&& 0x7fffffff) % n)), -1.0f, 1.0f)

/// Picks a unit length color.
let unitColor h (_ : Vec3f) =
  mangle12UnitVec3 <| mangle32c h

/// Picks a unit length color from n possible choices.
let unitColors n seed h (_ : Vec3f) =
  mangle12UnitVec3 <| mangle32c (seed + ((h &&& 0x7fffffff) % n))

/// All ones color.
let monoColor (_ : int) (_ : Vec3f) =
  Vec3f.one

/// Picks a color where every component is -1, 0 or 1.
let fullColor h (_ : Vec3f) =
  let bitsToValue x = if x < 341 then 0.0f else float32 (x &&& 1) * 2.0f - 1.0f
  let h = mangle32c h
  Vec3f(bitsToValue (h &&& 0x3ff), bitsToValue ((h >>> 10) &&& 0x3ff), bitsToValue ((h >>> 20) &&& 0x3ff))

/// Picks a color with components biased toward high absolute values.
let bigColor h (_ : Vec3f) =
  let v = Vec3f.fromSeed(mangle32c h, -1.0f, 1.0f)
  (Vec3f.one - v * v) * v.sign

/// Picks a color with components biased toward low absolute values.
let smallColor h (_ : Vec3f) =
  let v = Vec3f.fromSeed(mangle32c h, -1.0f, 1.0f)
  v * abs v

/// Picks a color from the palette.
let paletteColor (palette : Vec3f[]) h (_ : Vec3f) =
  let h = mangle32c h
  palette.[emod h palette.size]

