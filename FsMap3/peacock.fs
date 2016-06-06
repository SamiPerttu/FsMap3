/// The peacock basis.
module FsMap3.Peacock

open Common
open Mangle
open Basis3
open Potential
open FeatureCount
open CellColor


/// Basis that produces a pattern of randomly rotated potential functions.
/// radius is the radius, in cell units, of individual potentials (radius in ]0, 1]).
/// The shape of the falloff is controlled by the fade function.
let peacock (layout : LayoutFunction)
            (count : FeatureCount)
            (potential : Potential3)
            (mix : MixOp)
            (color : CellColor)
            (fade : float32 -> float32)
            (radius : float32)
            seed
            frequency =
  assert (radius > 0G)
  let R2 = squared radius
  let Ri = 1G / radius

  let layoutInstance = layout seed frequency

  fun (v : Vec3f) ->
    let data = layoutInstance.run v
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
            let r2 = P.length2
            if r2 < R2 then
              let q = mangle12Rotation (mangle32fast h)
              let v = Ri * P * q
              let p = potential v
              if p < 1G then
                let w = fade (1G - p)
                let g = Potential.gradient potential v * 0.5f
                let gN2 = g.length2
                // TODO. Roll off gradient magnitude smoothly.
                let g = if gN2 > 1G then g / sqrt gN2 else g
                value <- mix value 1.0f w (color h g)
    data.release()
    Mix.result value


/// Default peacock pattern with the standard cell layout and the smooth-2 fade function.
/// The cell hash seed is derived from the frequency.
let inline peacockd potential radius frequency = peacock hifiLayout unityCount potential Mix.sum anyColor Fade.smooth2 radius (manglef32 frequency) frequency


/// Peacock pattern with the standard cell layout.
/// The cell hash seed is derived from the frequency.
let inline peacockf potential fade radius frequency = peacock hifiLayout unityCount potential Mix.sum anyColor fade radius (manglef32 frequency) frequency

