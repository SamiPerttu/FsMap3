/// Radial weighted value noise basis.
module FsMap3.Radial

open Common
open Mangle
open Basis3
open CellColor


/// Radial weighted value noise basis.
let radial (layout : LayoutFunction) (color : CellColor) (fade : float32 -> float32) (frequency : float32) =

  // Note, minimum radius R that absolutely guarantees a full cover is (2 * sqrt 3) / (1 + sqrt 3) < 1.26795.
  let R = 1.26795f
  let Rm = R - 1G
  let RM = 2G - R
  let R2 = squared R

  let layoutInstance = layout frequency

  fun (v : Vec3f) ->
    let data = layoutInstance.run v
    let mutable value = Vec3f.zero
    let mutable W = 0G
    for jx = -1 to 1 do
      let hx = data.hashX(jx)
      for jy = -1 to 1 do
        let hxy = data.hashY(jy, hx)
        for jz = -1 to 1 do
          let h = data.hashZ(jz, hxy)
          let P = Vec3f.fromSeed(h, Rm, RM) + Vec3f(float32 jx, float32 jy, float32 jz)
          let r2 = (P - data.d).length2
          if r2 < R2 then
            let w = fade(1G - sqrt r2 / R)
            value <- value + w * color h
            W <- W + w
    data.release()
    value / W


/// Default radial weighted value noise with the standard cell layout and the cubic fade function.
let inline radiald f = radial hifiLayout anyColor Fade.cubic f


/// Radial weighted value noise with the standard cell layout.
let inline radialf fade f = radial hifiLayout anyColor fade f



