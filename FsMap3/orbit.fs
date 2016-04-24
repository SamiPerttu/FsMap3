/// Orbit trapped iteration fractals.
module FsMap3.Orbit

open Common
open Mangle
open Basis3
open Julia
open Atlas3
open FeatureCount


/// Basis that orbit traps a fractal formula. The fractal parameters are interpolated between feature points.
/// The number of iterations is typically low, around 2 to 8, for smoother results.
let orbit (layout : LayoutFunction)
          (count : FeatureCount) 
          (formula : IterationFormula)
          iterations
          (cOrigin : Vec3f)
          (qOrigin : Vec3f)
          cRange
          qRange
          (cScale : float32)
          (qScale : float32)
          (formulaFade : float32 -> float32)
          (trap : Vec3f)
          (trapRadius : float32)
          (trapFade : float32 -> float32)
          (trapPattern : Atlas3)
          (mix : MixOp)
          radius
          frequency
          (v : Vec3f) =
  let data = layout frequency v
  data.scan(radius)

  let R2 = squared radius
  let Ri = 1.0f / radius

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
            let w = formulaFade (1.0f - d)
            W <- W + w
            c <- c + w * (pc + P * cScale)
            q0 <- q0 + w * (pq + P * qScale)

  q0 <- q0 / W
  c <- c / W

  let tR2 = squared trapRadius
  let tRi = 1.0f / trapRadius

  data.release()

  match formula with

  | Julia ->
    let q0 = Vec2f(q0.x, q0.y)
    let c = Vec2f(c.x, c.y)
    let trap = Vec2f(trap.x, trap.y)
    let mutable q = q0
    let mutable iteration = 0
    let mutable value = Mix.start
    while iteration < iterations && q.length2 < 100.0f do
      q <- Vec2f(q.x * q.x - q.y * q.y, 2.0f * q.x * q.y) + c
      let d2 = (q - trap).length2
      if d2 < tR2 then
        let w = trapFade(1.0f - sqrt d2 * tRi)
        value <- mix value w (trapPattern iteration (Vec3f((q.x - trap.x) * tRi, (q.y - trap.y) * tRi, 1.0f)))
      iteration <- iteration + 1
    Mix.result value

  | AexionJulia ->
    let mutable q = q0
    let mutable iteration = 0
    let mutable value = Mix.start
    while iteration < iterations && q.length2 < 100.0f do
      q <- Vec3f(2.0f * q.x * q.z, q.z * q.z - q.x * q.x, -q.y) + c
      let d2 = (q - trap).length2
      if d2 < tR2 then
        let w = trapFade (1.0f - sqrt d2 * tRi)
        value <- mix value w (trapPattern iteration ((q - trap) * tRi))
      iteration <- iteration + 1
    Mix.result value

  | Makin ->
    let mutable q = q0
    let mutable iteration = 0
    let mutable value = Mix.start
    while iteration < iterations && q.length2 < 100.0f do
      q <- Vec3f(squared q.x - squared q.y - squared q.z, 2.0f * q.x * q.y, 2.0f * (q.x - q.y) * q.z) + c
      let d2 = (q - trap).length2
      if d2 < tR2 then
        let w = trapFade (1.0f - sqrt d2 * tRi)
        value <- mix value w (trapPattern iteration ((q - trap) * tRi))
      iteration <- iteration + 1
    Mix.result value

  | White ->
    let mutable q = q0
    let mutable iteration = 0
    let mutable value = Mix.start
    while iteration < iterations && q.length2 < 100.0f do
      let D = squared v.x + squared v.y
      if D > 0.0f then
        q <- Vec3f((squared q.x - squared q.y) * (1.0f - squared q.z / D), 2.0f * q.x * q.y * (1.0f - squared q.z / D), -2.0f * q.z * sqrt(D)) + c
      else
        q <- c
      q <- Vec3f(2.0f * q.x * q.z, q.z * q.z - q.x * q.x, -q.y) + c
      let d2 = (q - trap).length2
      if d2 < tR2 then
        let w = trapFade (1.0f - sqrt d2 * tRi)
        value <- mix value w (trapPattern iteration ((q - trap) * tRi))
      iteration <- iteration + 1
    Mix.result value

  | Mandelcorn ->
    let mutable q = q0
    let mutable iteration = 0
    let mutable value = Mix.start
    while iteration < iterations && q.length2 < 100.0f do
      q <- Vec3f(squared q.x - squared q.y - squared q.z, 2.0f * q.x * q.y, -2.0f * q.x * q.z) + c
      let d2 = (q - trap).length2
      if d2 < tR2 then
        let w = trapFade (1.0f - sqrt d2 * tRi)
        value <- mix value w (trapPattern iteration ((q - trap) * tRi))
      iteration <- iteration + 1
    Mix.result value



