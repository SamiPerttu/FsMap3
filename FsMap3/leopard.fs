/// Leopard basis.
module FsMap3.Leopard

open Common
open Mangle
open Basis3
open FeatureCount
open CellColor


/// Basis function that produces a spotted pattern.
/// The radius of individual spots is radius (radius > 0).
/// The shape of the falloff is controlled by the fade function.
let leopard (layout : LayoutFunction)
            (count : FeatureCount)
            (mix : MixOp)
            (color : CellColor)
            (fade : float32 -> float32)
            radius
            seed
            frequency =

  assert (radius > 0.0f)
  let R2 = squared radius
  let Ri = 1.0f / radius

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
            h <- mangle32fast h
            let P = data.d - Vec3f.fromSeed(h) - Vec3f(float32 jx, float32 jy, float32 jz)
            let d2 = P.length2
            if d2 < R2 then
              let d = sqrt d2 * Ri
              let w = fade (1.0f - d)
              value <- mix value 1.0f w (color h (P * Ri))
    data.release()
    Mix.result value


/// Default leopard pattern with the standard cell layout and the smooth-2 fade function.
/// The cell hash seed is derived from the frequency.
let inline leopardd radius frequency = leopard hifiLayout unityCount Mix.sum anyColor Fade.smooth2 radius (manglef32 frequency) frequency


/// Leopard pattern with the standard cell layout.
/// The cell hash seed is derived from the frequency.
let inline leopardf fade radius frequency = leopard hifiLayout unityCount Mix.sum anyColor fade radius (manglef32 frequency) frequency

