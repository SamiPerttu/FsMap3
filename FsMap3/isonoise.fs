/// Isotropic noise basis.
module FsMap3.Isonoise

open Common
open Mangle
open Basis3


/// Isotropic noise basis. Has no directional artifacts (excepting those arising from tiling layouts).
/// The shape of the falloff is controlled by the fade function.
let isonoise (layout : LayoutFunction)
             (fade : float32 -> float32)
             seed
             frequency =

  let layoutInstance = layout seed frequency

  fun (v : Vec3f) ->
    let data = layoutInstance.run v
    let mutable value = Vec3f.zero
    for jx = -1 to 1 do
      let hx = data.hashX(jx)
      for jy = -1 to 1 do
        let hxy = data.hashY(jy, hx)
        for jz = -1 to 1 do
          let mutable h = data.hashZ(jz, hxy)
          // For the number of features, we take a sample from Poisson(1.9).
          // This puts the average number of features influencing a point close to unity.
          let count =
            let x = uint h
            if x < 0x264c2f83u then 0 // 0.1496
            elif x < 0x6f0d844du then 1 // 0.4338
            elif x < 0xb42c3c9eu then 2 // 0.7038
            elif x < 0xdff2e48eu then 3 // 0.8748
            elif x < 0xf4bc6a7eu then 4 else 5 // 0.9560
          for __ = 1 to count do
            h <- mangle32 h
            let P = data.d - Vec3f(float32 jx, float32 jy, float32 jz) - Vec3f.fromSeed(h)
            let d2 = P.length2
            if d2 < 1.0f then
              let w = fade(1G - sqrt d2)
              let h2 = mangle32c h
              let color = Vec3f(float32 ((h2 >>> 12) &&& 0x3f) - 31.5f, float32 ((h2 >>> 18) &&& 0x3f) - 31.5f, float32 ((h2 >>> 24) &&& 0x3f) - 31.5f) * (0.06349206349f * (mangle12UnitVec3 h2 *. P))
              value <- value + w * color
    data.release()
    value

