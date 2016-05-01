/// Implicit flow basis functions.
module FsMap3.Impflow

open Common
open Mangle
open Potential
open Basis3
open Map3
open Atlas3
open CellColor
open FeatureCount


/// Implicit flow basis. Mixes potential function features. The potential function rotation
/// at each point is sampled from the flow basis.
let impflow (layout : LayoutFunction) (count : FeatureCount) (potential : Potential3) (mix : MixOp) (color : CellColor) (fade : float32 -> float32) (shading : float32) (radius : float32) (flow : Basis3) (flowFrequencyFactor : float32) (frequency : float32) =
  let R2 = squared radius
  let Ri = 1G / radius

  let layoutInstance = layout frequency

  fun (v : Vec3f) ->
    let data = layoutInstance.run v
    // Get rotation from the flow map.
    let L = G pi * flow (frequency * flowFrequencyFactor) v
    let length = L.length
    let pose = if length > 1.0e-9f then Quaternionf(L / length, length) else Quaternionf.one
    data.scan(radius)

    let mutable value = Mix.start
    for jx = data.x0 to data.x1 do
      let hx = data.hashX(jx)
      for jy = data.y0 to data.y1 do
        let hxy = data.hashY(jy, hx)
        for jz = data.z0 to data.z1 do
          let mutable h = data.hashZ(jz, hxy)
          for __ = 1 to count h do
            h <- mangle32 h
            let P = data.d - Vec3f.fromSeed(h) - Vec3f(float32 jx, float32 jy, float32 jz)
            if P.length2 < R2 then
              let v = P * Ri * pose
              let p = potential v
              if p < 1G then
                let w = fade (1G - p)
                let g = Potential.gradient potential v * 0.5f
                let gN = g.length2
                let g = if gN > 1G then g / sqrt gN else g
                value <- mix value w (color h * (1G + shading * p * 2.0f * g))
    data.release()
    Mix.result value


/// Default implicit flow basis with the standard layout, count, mixing and color functions.
let inline impflowd fade potential radius flow flowFactor frequency = impflow hifiLayout unityCount potential Mix.sum anyColor fade 0.5f radius flow flowFactor frequency



/// Patterned implicit flow basis. Mixes potential function features via an atlas.
/// The potential function rotation at each point is sampled from the flow basis.
let patflow (layout : LayoutFunction) (count : FeatureCount) (potential : Potential3) (mix : MixOp) (fade : float32 -> float32) (radius : float32) (curvature : float32) (pattern : Atlas3) (flow : Basis3) (flowFrequencyFactor : float32) (frequency : float32) =
  let R2 = squared radius
  let Ri = 1G / radius

  let layoutInstance = layout frequency

  fun (v : Vec3f) ->
    let data = layoutInstance.run v
    data.scan(radius)

    // Get rotation from the flow map.
    let L = G pi * flow (frequency * flowFrequencyFactor) v
    let length = L.length
    let pose = if length > 1.0e-9f then Quaternionf(L / length, length) else Quaternionf.one

    let mutable value = Mix.start
    for jx = data.x0 to data.x1 do
      let hx = data.hashX(jx)
      for jy = data.y0 to data.y1 do
        let hxy = data.hashY(jy, hx)
        for jz = data.z0 to data.z1 do
          let mutable h = data.hashZ(jz, hxy)
          for __ = 1 to count h do
            h <- mangle32 h
            let P = data.d - Vec3f.fromSeed(h) - Vec3f(float32 jx, float32 jy, float32 jz)
            if P.length2 < R2 then
              let v = P * Ri * pose
              let p = potential v
              if p < 1G then
                let w = fade (1G - p)
                let g = Potential.gradient potential v * 0.5f
                let gN2 = g.length2
                let g = if gN2 > 1G then g / sqrt gN2 else g
                value <- mix value w (pattern h (lerp v (g * p) curvature))
    data.release()
    Mix.result value
