module FsMap3.Atlas3

open Common
open Basis3
open Mangle
open Map3
open CellColor


/// Atlases are maps that accept an integer argument,
/// which can be interpreted as an index or a hash code.
type Atlas3 = int -> Vec3f -> Vec3f



let colorAtlas (color : CellColor) h (v : Vec3f) =
  color h


let offsetAtlas (map : Map3) (scale : float32) h (v : Vec3f) =
  map (v + Vec3f.fromSeed(mangle32c h, 0.0f, scale))


