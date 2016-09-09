// Walk operators.
namespace Fuse

open Common
open Map3


/// Walk operator state is stored here.
[<NoComparison; NoEquality>]
type WalkState = struct
  /// Current displacement. Modifications to delta are generally scaled by inverse frequency
  /// to equalize slope (the absolute value of displacement is inconsequential).
  val delta : Vec3f
  /// Current twist (optional).
  val twist : Quaternionf
  /// Extra state vector (optional).
  val c : Vec3f

  new(delta, twist, c) = { delta = delta; twist = twist; c = c }
  new(delta, twist) = { delta = delta; twist = twist; c = Vec3f.zero }
  new(delta, c) = { delta = delta; twist = 1G; c = c }
  new(delta) = { delta = delta; twist = 1G; c = Vec3f.zero }

  member inline this.read(f) = this.delta

  static member start = WalkState(0G, 1G, 1G)
end



/// Walk operators derive a new state from old state, displacement frequency, displacement value weight
/// and displacement value. The displacement value is weighted. Unweighted displacement value components
/// are roughly in the customary [-1, 1] range. Weight is in unit range; its purpose is to enable
/// smooth interpolation between partial and full steps.
type WalkOp = WalkState -> float32 -> float32 -> Vec3f -> WalkState



[<RequireQualifiedAccess>]
module Walk =

  let stand (state : WalkState) (f : float32) (w : float32) (v : Vec3f) = state


  let march (response : float32 -> float32) (damping : float32) (state : WalkState) (f : float32) (w : float32) (v : Vec3f) =
    WalkState((1.0f - w * damping) * state.delta + shape3 response v / f)


  let twist (response : float32 -> float32) (twistAmount : float32) (damping : float32) (state : WalkState) (f : float32) (w : float32) (v : Vec3f) =
    let u = v / f
    let L2 = u.length2
    if L2 > 0G then
      let L = sqrt L2
      let twist = state.twist * Quaternionf(u / L, twistAmount * L * 100.0f)
      WalkState((1.0f - w * damping) * state.delta + shape3 response v / f * twist, twist)
    else
      state


  let cross (response : float32 -> float32) (damping : float32) (state : WalkState) (f : float32) (w : float32) (v : Vec3f) =
    WalkState((1.0f - w * damping) * state.delta + shape3 response (v *+ state.c) / f, v)


  let bounce (response : float32 -> float32) (damping : float32) (state : WalkState) (f : float32) (w : float32) (v : Vec3f) =
    WalkState((1.0f - w * damping) * lerp state.delta -state.delta w + shape3 response v / f)


  let shuffle (response : float32 -> float32) (damping : float32) (state : WalkState) (f : float32) (w : float32) (v : Vec3f) =
    let u = state.delta
    WalkState((1.0f - w * damping) * lerp state.delta (Vec3f(u.z, u.x, u.y)) w + shape3 response v / f)


  let lurch (response : float32 -> float32) (damping : float32) (state : WalkState) (f : float32) (w : float32) (v : Vec3f) =
    let u = state.delta
    WalkState((1.0f - damping) * lerp state.delta (Vec3f(u.y, u.z, -u.x)) w + shape3 response v / f)


