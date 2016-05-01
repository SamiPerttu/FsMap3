/// Leopard basis function.
module FsMap3.Leopard

open Common
open Mangle
open Basis3
open Atlas3
open FeatureCount
open CellColor


/// Basis function that produces a spotted pattern.
/// The radius of individual spots is radius (0 < radius <= 1).
/// The shape of the falloff is controlled by the fade function.
/// The amount of cell gradient mixed with cell color is controlled by shading (shading in [0, 1]).
let leopard (layout : LayoutFunction)
            (count : FeatureCount)
            (mix : MixOp)
            (color : CellColor)
            (fade : float32 -> float32)
            (shading : float32)
            (radius : float32)
            (frequency : float32) =

  assert (radius > 0.0f)
  let R2 = squared radius
  let Ri = 1.0f / radius

  let layoutInstance = layout frequency

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
              value <- mix value w (color h * (1G + shading * Ri * P))
    data.release()
    Mix.result value


/// Default leopard pattern with the standard cell layout and the smooth fade function.
let inline leopardd radius frequency = leopard hifiLayout unityCount Mix.sum anyColor Fade.smooth 0.0f radius frequency


/// Leopard pattern with the standard cell layout.
let inline leopardf fade radius frequency = leopard hifiLayout unityCount Mix.sum anyColor fade 0.0f radius frequency



/// Basis function that mixes patterned spots from an atlas.
/// The radius of individual spots is radius (0 < radius <= 1).
/// The shape of the falloff is controlled by the fade function.
let geopard (layout : LayoutFunction)
            (count : FeatureCount)
            (mix : MixOp)
            (fade : float32 -> float32)
            (radius : float32)
            (curvature : float32)
            (pattern : Atlas3)
            (frequency : float32) =

  assert (radius > 0.0f)
  let R2 = squared radius
  let Ri = 1.0f / radius

  let layoutInstance = layout frequency

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
              value <- mix value w (pattern h (P * (Ri * (1.0f - curvature + curvature * 2.0f * d))))
    data.release()
    Mix.result value

