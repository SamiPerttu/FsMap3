module FsMap3.ColorDna

open Common


/// Generates a palette as a Map3. It maps abstract component values to RGB, with both inputs and outputs
/// in the customary range [-1, 1]. The grid resolution of the color interpolation cube is n (for example, 30).
/// Note that no gamma conversion is done; it is expected that we want to operate in gamma corrected space.
let genPalette (n : int) (dna : Dna) =

  // Choose a color space.
  let space = dna.category("Color space", C(HSD), C(HSV), C(HSL), C(CIELch), C(Mugsell))

  let h0 = dna.float32("Hue origin")
  let hr = dna.float32("Hue range", squared >> lerp 0.1f 1.0f)
  let hc = dna.float32("Hue concentration") |> (*) 3.0f |> exp2
  let hdamp = dna.float32("Hue damping")

  let sr = dna.float32("Saturation range", lerp 0.2f 1.0f)
  let sm = dna.float32("Saturation minimum", squared >> lerp 0.0f (1.0f - sr))
  let sM = sm + sr
  let sfade = Fade.skew (dna.float32("Saturation skew", lerp -2.0f 2.0f))

  let vm = 0.0f
  let vM = 1.0f
  let vfade = Fade.skew (dna.float32("Value skew", lerp -3.0f 3.0f))

  let cube = Array.zeroCreate (cubed n)

  for x = 0 to n - 1 do
    let h = float32 x / float32 (n - 1) * 2.0f - 1.0f
    let h = apow h hc
    let sdamp = 1.0f - sqrt (abs h) * hdamp
    let h = fract (h0 + h * hr * 0.5f)
    for y = 0 to n - 1 do
      let s = float32 y / float32 (n - 1)
      let s = sdamp * lerp sm sM (sfade s)
      for z = 0 to n - 1 do
        let v = float32 z / float32 (n - 1)
        let v = lerp vm vM (vfade v)
        let rgb = space.rgb(h, s, v)
        // Transform to [-1, 1] range.
        cube.[x * squared n + y * n + z] <- map01to11 rgb

  let vZ = float32 (n - 1) * 0.5f

  fun (v : Vec3f) ->
    assert (v.isFinite)
    let u = v.map(fun x -> vZ * (clamp -0.999f 0.999f x + 1.0f))
    let d = u - u.map(floor)
    let ix = int u.x * squared n
    let jx = ix + squared n
    let iy = int u.y * n
    let jy = iy + n
    let iz = int u.z
    let jz = iz + 1
    let xii = Vec3f.lerp cube.[ix + iy + iz] cube.[jx + iy + iz] d.x
    let xij = Vec3f.lerp cube.[ix + iy + jz] cube.[jx + iy + jz] d.x
    let xji = Vec3f.lerp cube.[ix + jy + iz] cube.[jx + jy + iz] d.x
    let xjj = Vec3f.lerp cube.[ix + jy + jz] cube.[jx + jy + jz] d.x
    let yi = Vec3f.lerp xii xji d.y
    let yj = Vec3f.lerp xij xjj d.y
    Vec3f.lerp yi yj d.z
 
