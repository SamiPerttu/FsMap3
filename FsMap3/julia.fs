/// Fractal iteration parameter interpolation basis.
module FsMap3.Julia

open Common
open Mangle
open Basis3
open FeatureCount


/// Fractal iteration formulas.
type FractalFormula =
  Julia | AexionJulia | Makin | White | Mandelcorn
  override this.ToString() = unionLabel this
  member this.is2D = match this with | Julia -> true | _ -> false


[<NoComparison; NoEquality>]
type FractalParameters = struct
  val cOrigin : Vec3f
  val qOrigin : Vec3f
  val cRange : float32
  val qRange : float32
  val cScale : float32
  val qScale : float32
  new(cOrigin, qOrigin, cRange, qRange, cScale, qScale) = { cOrigin = cOrigin; qOrigin = qOrigin; cRange = cRange; qRange = qRange; cScale = cScale; qScale = qScale }
end


/// Basis that accumulates a result by iterating a fractal formula. The fractal parameters
/// are interpolated between feature points. The number of iterations is typically low,
/// around 2 to 10.
let julia (layout : LayoutFunction)
          (count : FeatureCount) 
          (formula : FractalFormula)
          iterations
          roughness
          radius
          (fade : float32 -> float32)
          (p : FractalParameters)
          seed
          frequency =
  let layoutInstance = layout seed frequency

  fun (v : Vec3f) ->
    let data = layoutInstance.run v
    let R2 = squared radius
    let Ri = 1.0f / radius
    data.scan(radius)
    let mutable W = 1.0f
    let mutable c = W * p.cOrigin
    let mutable q0 = W * p.qOrigin
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
              let pc = p.cOrigin + Vec3f.fromSeed(mangle32d h, -p.cRange, p.cRange)
              let pq = p.qOrigin + Vec3f.fromSeed(mangle32fast h, -p.qRange, p.qRange)
              let w = fade (1.0f - d)
              W <- W + w
              c <- c + w * (pc + P * p.cScale)
              q0 <- q0 + w * (pq + P * p.qScale)
    q0 <- q0 / W
    c <- c / W

    data.release()

    let inline Zf2 (v : Vec2f) = let L = v.maxNorm in 0.5f / max3 0.5f L (squared L)
    let inline Zf3 (v : Vec3f) = let L = v.maxNorm in 0.5f / max3 0.5f L (squared L)

    let inline iterate3 f =
      let mutable q = q0
      let mutable iteration = 0
      let mutable result = Vec3f.zero
      let mutable W = 1.0f / Mat.geometricSum iterations 1.0f roughness
      while iteration < iterations && q.length2 < 1.0e4f do
        q <- f q
        result <- result + W * q * Zf3 q
        W <- W * roughness
        q <- q + c
        iteration <- iteration + 1
      result

    match formula with

    | Julia ->
      let q0 = Vec2f(q0.x, q0.y)
      let c = Vec2f(c.x, c.y)
      let mutable q = q0
      let mutable iteration = 0
      let mutable result = Vec3f.zero
      let mutable W = 1.0f / Mat.geometricSum iterations 1.0f roughness
      while iteration < iterations && q.length2 < 1.0e4f do
        q <- Vec2f(q.x * q.x - q.y * q.y, 2.0f * q.x * q.y)
        let Z = Zf2 q
        let q1 = q - q0
        result <- result + W * Vec3f(q.x * Z, q.y * Z, (q1.x + q1.y) * 0.5f * Zf2 q1)
        W <- W * roughness
        q <- q + c
        iteration <- iteration + 1
      result

    | AexionJulia ->
      iterate3 (fun q ->
        Vec3f(2.0f * q.x * q.z, q.z * q.z - q.x * q.x, -q.y)
        )

    | Makin ->
      iterate3 (fun q ->
        Vec3f(squared q.x - squared q.y - squared q.z, 2.0f * q.x * q.y, 2.0f * (q.x - q.y) * q.z)
        )

    | White ->
      iterate3 (fun q ->
        let D = squared q.x + squared q.y
        if D > 0.0f then
          Vec3f((squared q.x - squared q.y) * (1.0f - squared q.z / D), 2.0f * q.x * q.y * (1.0f - squared q.z / D), -2.0f * q.z * sqrt(D))
        else Vec3f.zero
        )

    | Mandelcorn ->
      iterate3 (fun q ->
        Vec3f(squared q.x - squared q.y - squared q.z, 2.0f * q.x * q.y, -2.0f * q.x * q.z)
        )

