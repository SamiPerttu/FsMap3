/// Walk operators.
module FsMap3.Walk

open Common


/// Shapes the magnitude of a vector with a fade function. The input to the fade function is clamped.
let inline shape3f (fade : float32 -> float32) (v : Vec3f) =
  let length = min 1.0f v.length
  if length > 0.0f then
    (fade length / length) * v
  else
    Vec3f.zero


/// Walk operator state is stored here. The delta value is the current displacement. Modifications
/// to it are generally scaled by inverse frequency to equalize slope (the absolute value of displacement
/// is inconsequential).
[<NoComparison; NoEquality>]
type WalkState = struct
  val delta : Vec3f
  val twist : Quaternionf
  val c : Vec3f
  new(delta, twist, c) = { delta = delta; twist = twist; c = c }
  new(delta, twist) = { delta = delta; twist = twist; c = Vec3f.zero }
  new(delta, c) = { delta = delta; twist = 1G; c = c }
  new(delta) = { delta = delta; twist = 1G; c = Vec3f.zero }

  member inline this.read(f) = this.delta

  static member start = WalkState(0G, 1G, 1G)
end



/// Walk operators derive a new state from old state, displacement frequency and displacement value.
/// The displacement input value components are roughly in the customary [-1, 1] range.
type WalkOp = WalkState -> float32 -> Vec3f -> WalkState


let march (amount : float32) (fade : float32 -> float32) (damping : float32) (state : WalkState) (f : float32) (v : Vec3f) =
  WalkState((1.0f - damping) * state.delta + amount / f * shape3f fade v)


let twist (walkAmount : float32) (walkFade : float32 -> float32) (twistAmount : float32) (damping : float32) (state : WalkState) (f : float32) (v : Vec3f) =
  let u = v / f
  let L2 = u.length2
  if L2 > 0G then
    let L = sqrt L2
    let twist = state.twist * Quaternionf(u / L, twistAmount * L * 100.0f)
    WalkState((1.0f - damping) * state.delta + walkAmount / f * shape3f walkFade v * twist, twist)
  else
    state


let cross (amount : float32) (fade : float32 -> float32) (damping : float32) (state : WalkState) (f : float32) (v : Vec3f) =
  let u = shape3f fade v
  WalkState((1.0f - damping) * state.delta + amount / f * (u *+ state.c), u)


let bounce (amount : float32) (fade : float32 -> float32) (damping : float32) (state : WalkState) (f : float32) (v : Vec3f) =
  WalkState((1.0f - damping) * -state.delta + amount / f * shape3f fade v)


let shuffle (amount : float32) (fade : float32 -> float32) (damping : float32) (state : WalkState) (f : float32) (v : Vec3f) =
  let u = state.delta
  WalkState((1.0f - damping) * Vec3f(u.z, u.x, u.y) + amount / f * shape3f fade v)


let lurch (amount : float32) (fade : float32 -> float32) (damping : float32) (state : WalkState) (f : float32) (v : Vec3f) =
  let u = state.delta
  WalkState((1.0f - damping) * Vec3f(u.y, u.z, -u.x) + amount / f * shape3f fade v)


