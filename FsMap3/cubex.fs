/// Cubex noise.
module FsMap3.Cubex

open Common
open Mangle
open Basis3
open CellColor


/// Cubex noise. Radial basis gradient noise resembling Perlin noise.
/// The implementation is similar to simplex noise except we interpolate inside a cube.
let cubex (layout : LayoutFunction) (mix : MixOp) (color : CellColor) (fade : float32 -> float32) (frequency : float32) =
  let layoutInstance = layout frequency

  fun (v : Vec3f) ->
    let data = layoutInstance.run v

    let mutable value = Mix.start

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
            value <- mix value w (P *. mangle12UnitVec3 h * 2.0f * color h)

    data.release()
    Mix.result value


/// Default cubex noise with the standard cell layout and the smooth-2 fade function.
let cubexd frequency = cubex hifiLayout Mix.sum anyColor Fade.smooth2 frequency


/// Cubex noise with the standard cell layout.
let cubexf fade frequency = cubex hifiLayout Mix.sum anyColor fade frequency



