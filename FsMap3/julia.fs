/// Fractal iteration parameter interpolation basis.
module FsMap3.Julia

open Common
open Mangle
open Basis3
open FeatureCount


/// Fractal iteration formulas.
type IterationFormula =
  Julia | AexionJulia | Makin | White | Mandelcorn
  override this.ToString() = unionLabel this
  member this.is2D = match this with | Julia -> true | _ -> false


/// Basis that displays the results of iterating a fractal formula. The fractal parameters
/// are interpolated between feature points. The number of iterations is typically low,
/// around 2 to 8, for smoother results.
let julia (layout : LayoutFunction)
          (count : FeatureCount) 
          (formula : IterationFormula)
          iterations
          (cOrigin : Vec3f)
          (qOrigin : Vec3f)
          cRange
          qRange
          (cScale : float32)
          (qScale : float32)
          (fade : float32 -> float32)
          radius
          frequency
          (v : Vec3f) =
  let data = layout frequency v
  let R2 = squared radius
  let Ri = 1.0f / radius
  data.scan(radius)
  let mutable W = 1.0f
  let mutable c = W * cOrigin
  let mutable q0 = W * qOrigin
  for jx = data.x0 to data.x1 do
    let hx = data.hashX(jx)
    for jy = data.y0 to data.y1 do
      let hxy = data.hashY(jy, hx)
      for jz = data.z0 to data.z1 do
        let mutable h = data.hashZ(jz, hxy)
        for __ = 1 to count h do
          h <- mangle32b h
          let P = data.d - Vec3f.fromSeed(h) - Vec3f(float32 jx, float32 jy, float32 jz)
          let d2 = P.length2
          if d2 < R2 then
            let d = sqrt d2 * Ri
            let pc = cOrigin + Vec3f.fromSeed(mangle32d h, -cRange, cRange)
            let pq = qOrigin + Vec3f.fromSeed(mangle32fast h, -qRange, qRange)
            let w = fade (1.0f - d)
            W <- W + w
            c <- c + w * (pc + P * cScale)
            q0 <- q0 + w * (pq + P * qScale)
  q0 <- q0 / W
  c <- c / W

  data.release()

  match formula with

  | Julia ->
    let q0 = Vec2f(q0.x, q0.y)
    let c = Vec2f(c.x, c.y)
    let mutable q = q0
    let mutable iteration = 0
    while iteration < iterations && q.length2 < 1.0e4f do
      q <- Vec2f(q.x * q.x - q.y * q.y, 2.0f * q.x * q.y) + c
      iteration <- iteration + 1
    let q1 = q - c - q0
    let Zf (v : Vec2f) = let L = v.maxNorm in 0.5f / max3 0.5f L (squared L)
    let q1Z = Zf q1
    Vec3f(q1.x * q1Z, q1.y * q1Z, q.x * Zf q)

  | AexionJulia ->
    let mutable q = q0
    let mutable iteration = 0
    while iteration < iterations && q.length2 < 1.0e4f do
      q <- Vec3f(2.0f * q.x * q.z, q.z * q.z - q.x * q.x, -q.y) + c
      iteration <- iteration + 1
    let q1 = q - c - q0
    let Zf (v : Vec3f) = let L = v.maxNorm in 0.5f / max3 0.5f L (squared L)
    q1 * Zf q1

  | Makin ->
    let mutable q = q0
    let mutable iteration = 0
    while iteration < iterations && q.length2 < 1.0e4f do
      q <- Vec3f(squared q.x - squared q.y - squared q.z, 2.0f * q.x * q.y, 2.0f * (q.x - q.y) * q.z) + c
      iteration <- iteration + 1
    let q1 = q - c - q0
    let Zf (v : Vec3f) = let L = v.maxNorm in 0.5f / max3 0.5f L (squared L)
    q1 * Zf q1

  | White ->
    let mutable q = q0
    let mutable iteration = 0
    while iteration < iterations && q.length2 < 1.0e4f do
      let D = squared v.x + squared v.y
      if D > 0.0f then
        q <- Vec3f((squared q.x - squared q.y) * (1.0f - squared q.z / D), 2.0f * q.x * q.y * (1.0f - squared q.z / D), -2.0f * q.z * sqrt(D)) + c
      else
        q <- c
      q <- Vec3f(2.0f * q.x * q.z, q.z * q.z - q.x * q.x, -q.y) + c
      iteration <- iteration + 1
    let q1 = q - c - q0
    let Zf (v : Vec3f) = let L = v.maxNorm in 0.5f / max3 0.5f L (squared L)
    q1 * Zf q1

  | Mandelcorn ->
    let mutable q = q0
    let mutable iteration = 0
    while iteration < iterations && q.length2 < 1.0e4f do
      q <- Vec3f(squared q.x - squared q.y - squared q.z, 2.0f * q.x * q.y, -2.0f * q.x * q.z) + c
      iteration <- iteration + 1
    let q1 = q - c - q0
    let Zf (v : Vec3f) = let L = v.maxNorm in 0.5f / max3 0.5f L (squared L)
    q1 * Zf q1

