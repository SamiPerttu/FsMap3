/// The capsule flow basis.
module FsMap3.Capflow

open Common
open Basis3
open Mangle
open Map3
open CellColor
open FeatureCount


/// Capsule flow basis consists of oriented capsules.
/// The length and orientation of the capsules at each point is sampled from the flow basis.
let capflow (layout : LayoutFunction) (count : FeatureCount) (mix : MixOp) (color : CellColor) (fade : float32 -> float32) (shading : float32) (length : float32) (radius : float32) (flow : Basis3) (flowFrequencyFactor : float32) (frequency : float32) =
  let R2 = squared radius
  let Ri = 1G / radius

  let layoutInstance = layout frequency

  fun (v : Vec3f) ->
    let data = layoutInstance.run v
    // Get streak vector from the flow map.
    let V = length * flow (frequency * flowFrequencyFactor) v
    let L = V.length
    let D = if L > 0.0f then V / L else Vec3f.unitX

    data.scan(radius + max0 V.x, radius - min0 V.x, radius + max0 V.y, radius - min0 V.y, radius + max0 V.z, radius - min0 V.z)

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
            let p = clamp 0.0f L (P *. D)
            let S = P - p * D
            let distance2 = S.length2
            if distance2 < R2 then
              let distance = sqrt distance2
              let w = fade (1G - distance * Ri)
              value <- mix value 1.0f w (color h * (1G + Ri * shading * S))
    data.release()
    Mix.result value
