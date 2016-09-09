/// Cubex noise.
module Fuse.Cubex

open Common
open Mangle
open Basis3
open CellColor


/// Cubex noise. Radial basis gradient noise resembling Perlin noise.
/// The implementation is similar to simplex noise except we interpolate inside a cube.
let cubex (layout : LayoutFunction) (color : CellColor) (fade : float32 -> float32) (seed : int) (frequency : float32) =
  let layoutInstance = layout seed frequency

  fun (v : Vec3f) ->
    let data = layoutInstance.run v

    let mutable value = Vec3f.zero

    for jx = 0 to 1 do
      let hx = data.hashX(jx)
      for jy = 0 to 1 do
        let hxy = data.hashY(jy, hx)
        for jz = 0 to 1 do
          let P = data.d - Vec3f(float32 jx, float32 jy, float32 jz)
          let r2 = P.length2
          if r2 < 1G then
            let h = data.hashZ(jz, hxy)
            let w = fade (1G - sqrt r2)
            value <- value + (P *. mangle12UnitVec3 h * 2.0f * w * color h P)

    data.release()
    value


/// Default cubex noise with the standard cell layout and the smooth-2 fade function.
/// The cell hash seed is derived from the frequency.
let cubexd frequency = cubex hifiLayout anyColor Fade.smooth2 (manglef32 frequency) frequency


/// Cubex noise with the standard cell layout.
/// The cell hash seed is derived from the frequency.
let cubexf fade frequency = cubex hifiLayout anyColor fade (manglef32 frequency) frequency



