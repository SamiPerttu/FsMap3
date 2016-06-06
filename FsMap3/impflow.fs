/// Implicit flow basis functions.
module FsMap3.Impflow

open Common
open Mangle
open Potential
open Basis3
open Map3
open CellColor
open FeatureCount


/// Implicit flow basis. Mixes potential function features. The potential function rotation
/// at each point is sampled from the flow basis.
let impflow (layout : LayoutFunction)
            (count : FeatureCount)
            (potential : Potential3)
            (mix : MixOp)
            (color : CellColor)
            (fade : float32 -> float32)
            (radius : float32)
            (flow : Basis3)
            (flowFrequencyFactor : float32)
            seed
            frequency =
  let R2 = squared radius
  let Ri = 1G / radius

  let layoutInstance = layout seed frequency

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
                value <- mix value 1.0f w (color h g)
    data.release()
    Mix.result value


/// Default implicit flow basis with the standard layout, count, mixing and color functions.
/// The cell hash seed is derived from the frequency.
let inline impflowd fade potential radius flow flowFactor frequency =
  impflow hifiLayout unityCount potential Mix.sum anyColor fade radius flow flowFactor (manglef32 frequency) frequency

