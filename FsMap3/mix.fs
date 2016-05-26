// Mixing operators.
namespace FsMap3

open Common


/// MixState contains the current state of a mixing operator.
[<NoComparison>]
type MixState = struct
  /// Current influence (non-negative). Influence is weight multiplied by alpha.
  val i : float32
  /// Current alpha in [0, 1].
  val a : float32
  /// The current (influence weighted) result.
  val v : Vec3f
  /// Optional extra state.
  val c : Vec3f

  new(i : float32, a : float32, v : Vec3f) = { i = i; a = a; v = v; c = Vec3f.zero }
  new(i : float32, a : float32, v : Vec3f, c : Vec3f) = { i = i; a = a; v = v; c = c }
end



/// Many Map3 bases and combinators support a mix operator, which governs how the influence
/// of different features is composited into a final result. The parameters are:
/// state -> feature weight -> feature alpha -> feature value -> result.
/// Feature weight > 0. Feature alpha in [0, 1]. Feature value is unweighted.
type MixOp = MixState -> float32 -> float32 -> Vec3f -> MixState


(*
A "transmissive" prescription for smooth mixing with alpha.

state.i = I = current influence >= 0
state.a = A = current alpha in [0, 1]
state.v = V = current value (influence weighted)

When mixing in w, a and u:

w = weight > 0
a = alpha in [0, 1]
u = value (unweighted)
i = w * a = influence
v = i * u = weighted value

Mix proportion            : A * a
Transparent proportion    : (1 - A) * (1 - a)
Unmodified V contribution : (1 - a) * V
Unmodified v contribution : (1 - A) * v

The mix proportion is processed by mixing functions f and g, which receive weighted inputs and their influences
and return, respectively, total weighted value and total influence. The complete formula is:

i' = (1 - a) * I + (1 - a) * I + A * a * g(I, V, i, v)
a' = 1 - (1 - A) * (1 - a)
v' = (1 - a) * V
   + (1 - A) * v
   + A * a * f(I, V, i, v)

Sanity Checks

When a = 0 (and thus i = 0):
i' = I
a' = A
v' = V

When A = 1 and a = 1:
i' = g(...)
a' = 1
v' = f(...)
*)


[<RequireQualifiedAccess>]
module Mix =

  /// Initial mixing state is all zeros.
  let start = MixState()

  /// Smooth mixing helper.
  let inline alphaProcess (state : MixState) (w : float32) (a : float32) (u : Vec3f) (f : float32 -> Vec3f -> float32 -> Vec3f -> Pair<float32, Vec3f>) =
    let i = w * a
    if i > 0G then
      let v = i * u
      let I = state.i
      let A = state.a
      let V = state.v
      let Aa = A * a
      if Aa > 0G then
        let M = f I V i v
        MixState((1G - a) * I + (1G - A) * i + Aa * M.a,
                 1G - (1G - A) * (1G - a),
                 (1G - a) * V + (1G - A) * v + Aa * M.b)
      else
        MixState((1G - a) * I + (1G - A) * i,
                 1G - (1G - A) * (1G - a),
                 (1G - a) * V + (1G - A) * v)

    else
      state

  /// The result of mixing is stored in MixState.v.
  let inline result (state : MixState) = state.v

  /// Layering operator that maintains a comparand value. The influence of new features
  /// is diminished by the distance to the comparand in a curve defined by the width and fade parameters.
  /// The comparand itself is influenced according to the persist parameter (persist in [0, 1]).
  /// Layering can produce effects that resemble painting with a brush or pen.
  let layer (width : float32) (fade : float32 -> float32) (persist : float32) (state : MixState) (w : float32) (a : float32) (u : Vec3f) =
    let state' =
      alphaProcess state w a u (fun I V i v ->
        let Lw = fade (max 0G (1G - (u - state.c).norm1 * 0.2f / width))
        Pair(I + Lw * i, V + Lw * v)
        )
    let Pw = (1G - persist) * a
    let c' = lerp state.c u Pw
    MixState(state'.i, state'.a, state'.v, c')

  /// Weighted sum.   
  let sum (state : MixState) (w : float32) (a : float32) (u : Vec3f) =
    alphaProcess state w a u (fun I V i v -> Pair(I + i, V + v))

  /// Composites new features over old features.
  let over (state : MixState) (w : float32) (a : float32) (u : Vec3f) =
    alphaProcess state w a u (fun I V i v ->
      let O = min I i
      let thickening = 1.2f
      Pair(I - O + i - O + O * thickening, (I - O) / I * V + (i - O + O * thickening) / i * v)
      )

  /// Softmin (bias < 0) or softmax (bias > 0) mixing. Bias range is [-1, 1].
  /// Reduces to a weighted sum when bias = 0.
  let soft (bias : float32) (state : MixState) (w : float32) (a : float32) (u : Vec3f) =
    let bias = bias * abs bias * 4G
    let inline mix x y = let wx, wy = Mat.eqp(bias * x), Mat.eqp(bias * y) in (x * wx + y * wy) / (wx + wy)
    alphaProcess state w a u (fun I V i v ->
      let U = V / I
      let O = min I i
      let thickening = 1.2f
      Pair(I - O + i - O + O * thickening, (I - O) * U + (i - O) * u + O * thickening * Vec3f.bimap(U, u, mix))
      )

  /// Mixes colors according to their 1-norms with mixing hardness in [0, 1].
  let norm (hardness : float32) (state : MixState) (w : float32) (a : float32) (u : Vec3f) =
    // This is the simplest weight response that starts linearly and keeps accelerating.
    // Unlike exp, it does not work with negative arguments.
    // We just need to tweak the hardness transformation to get the desired response curve.
    let h = squared hardness * 2.0f
    let inline f x = let x = h * x in 1.0f + x + (squared x * (1.0f + 0.2f * squared x))
    alphaProcess state w a u (fun I V i v ->
      let U = V / I
      let O = min I i
      let thickening = 1.2f
      let wV = f(U.norm1)
      let wv = f(u.norm1)
      Pair(I - O + i - O + O * thickening, (I - O) * U + (i - O) * u + O * thickening * (wV * U + wv * u) / (wV + wv))
      )

  /// Hard minimum. Suitable for intra-basis mixing only.
  let min (state : MixState) (w : float32) (a : float32) (v : Vec3f) =
    alphaProcess state w a v (fun I V i v ->
      Pair(I + i, Vec3f.minimize V v)
      )

  /// Hard maximum. Suitable for intra-basis mixing only.
  let max (state : MixState) (w : float32) (a : float32) (v : Vec3f) =
    alphaProcess state w a v (fun I V i v ->
      Pair(I + i, Vec3f.maximize V v)
      )

