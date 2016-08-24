/// The capsule flow basis.
module FsMap3.Capflow

open Common
open Basis3
open Mangle
open Map3
open CellColor
open FeatureCount


/// Capsule flow basis consists of oriented and stretched capsules.
/// The length and orientation of the capsules at each point is sampled from the flow basis.
let capflow (layout : LayoutFunction)
            (count : FeatureCount)
            (mix : MixOp)
            (color : CellColor)
            (fade : float32 -> float32)
            (length : float32)
            (radius : float32)
            (flowBasis : Basis3)
            (flowFrequencyFactor : float32)
            seed
            frequency =

  let R2 = squared radius
  let Ri = 1G / radius

  let layoutInstance = layout (mangle32 seed) frequency

  let flow = flowBasis seed (frequency * flowFrequencyFactor)

  fun (v : Vec3f) ->
    let data = layoutInstance.run v
    // Get streak vector from the flow map.
    let V = length * flow v
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
            let d2 = S.length2
            if d2 < R2 then
              let d = sqrt d2 * Ri
              let w = fade (1.0f - d)
              value <- mix value 1.0f w (color h (S * Ri))
    data.release()
    Mix.result value
