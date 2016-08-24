/// Radial weighted value noise basis.
module FsMap3.Radial

open Common
open Mangle
open Basis3
open CellColor


/// Radial weighted value noise basis.
let radial (layout : LayoutFunction) (color : CellColor) (fade : float32 -> float32) seed (frequency : float32) =

  // Note, minimum radius R that absolutely guarantees a full cover is (2 * sqrt 3) / (1 + sqrt 3) < 1.26795.
  let R = 1.26795f
  let Ri = 1G / R
  let Rm = R - 1G
  let RM = 2G - R
  let R2 = squared R

  let layoutInstance = layout seed frequency

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
          let P = data.d - Vec3f.fromSeed(h, Rm, RM) - Vec3f(float32 jx, float32 jy, float32 jz)
          let r2 = P.length2
          if r2 < R2 then
            let w = fade(1G - sqrt r2 * Ri)
            value <- value + w * color h (P * Ri)
            W <- W + w
    data.release()
    value / W


/// Default radial weighted value noise with the standard cell layout and the smooth-1 fade.
/// The cell hash seed is derived from the frequency.
let inline radiald frequency = radial hifiLayout anyColor Fade.smooth1 (manglef32 frequency) frequency


/// Radial weighted value noise with the standard cell layout.
/// The cell hash seed is derived from the frequency.
let inline radialf fade frequency = radial hifiLayout anyColor fade (manglef32 frequency) frequency

