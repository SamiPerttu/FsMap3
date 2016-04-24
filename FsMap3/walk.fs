/// Walk operators.
module FsMap3.Walk

open Common


/// Shapes the magnitude of a vector with a fade function. The input to the fade function is clamped.
let inline shape3f (fade : float32 -> float32) (v : Vec3f) =
  let length = v.length
  if length > 0.0f then
    fade (min 1.0f length) / length * v
  else
    Vec3f.zero


/// Walk operators derive a new displacement from current displacement, displacement frequency and displacement input value.
/// The displacement input value components are roughly in the customary [-1, 1] range.
type WalkOp = Vec3f -> float32 -> Vec3f -> Vec3f


let march (a : float32) (fade : float32 -> float32) (u : Vec3f) (f : float32) (v : Vec3f) = u + a / f * shape3f fade v

let restep (a : float32) (fade : float32 -> float32) (u : Vec3f) (f : float32) (v : Vec3f) = 0.333333f * u + 1.5f * a / f * shape3f fade v

let bounce (a : float32) (fade : float32 -> float32) (u : Vec3f) (f : float32) (v : Vec3f) = -u + a / f * shape3f fade v

let shuffle (a : float32) (fade : float32 -> float32) (u : Vec3f) (f : float32) (v : Vec3f) = Vec3f(u.z, u.x, u.y) + a / f * shape3f fade v

let lurch (a : float32) (fade : float32 -> float32) (u : Vec3f) (f : float32) (v : Vec3f) = Vec3f(u.y, u.z, -u.x) + a / f * shape3f fade v


