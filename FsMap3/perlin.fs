/// Perlin noise.
module FsMap3.Perlin

open Common
open Mangle
open Basis3


/// Improved Perlin gradient noise. This function is designed to emulate band-limited noise.
/// A smooth, symmetric fade is recommended for best results.
let perlin (layout : LayoutFunction) (fade : float32 -> float32) (seed : int) (frequency : float32) =
  let layoutInstance = layout seed frequency

  let inline grad h (d : Vec3f) =
    Vec3f(float32 ((h >>> 12) &&& 0x3f) - 31.5f, float32 ((h >>> 18) &&& 0x3f) - 31.5f, float32 ((h >>> 24) &&& 0x3f) - 31.5f) * (0.06349206349f * (mangle12UnitVec3 h *. d))

  fun (v : Vec3f) ->
    let data = layoutInstance.run v

    let d = data.d
    let e = d - Vec3f(1.0f)
    let p = d.map(fade)

    let hi, hj = data.hashX(0), data.hashX(1)
    let hii = data.hashY(0, hi)
    let hij = data.hashY(1, hi)
    let hji = data.hashY(0, hj)
    let hjj = data.hashY(1, hj)

    let result =
      Vec3f.lerp (Vec3f.lerp (Vec3f.lerp (grad (data.hashZ(0, hii)) d)                      (grad (data.hashZ(1, hii)) (Vec3f(d.x, d.y, e.z))) p.z)
                             (Vec3f.lerp (grad (data.hashZ(0, hij)) (Vec3f(d.x, e.y, d.z))) (grad (data.hashZ(1, hij)) (Vec3f(d.x, e.y, e.z))) p.z) p.y)  
                 (Vec3f.lerp (Vec3f.lerp (grad (data.hashZ(0, hji)) (Vec3f(e.x, d.y, d.z))) (grad (data.hashZ(1, hji)) (Vec3f(e.x, d.y, e.z))) p.z)
                             (Vec3f.lerp (grad (data.hashZ(0, hjj)) (Vec3f(e.x, e.y, d.z))) (grad (data.hashZ(1, hjj)) e) p.z) p.y) p.x
    data.release()
    result


/// Default Perlin noise with the standard cell layout and the smooth-2 fade.
/// The cell hash seed is derived from the frequency.
let perlind frequency = perlin hifiLayout Fade.smooth2 (manglef32 frequency) frequency


/// Perlin noise with the standard cell layout.
/// The cell hash seed is derived from the frequency.
let perlinf fade frequency = perlin hifiLayout fade (manglef32 frequency) frequency


