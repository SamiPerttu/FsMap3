/// Color spaces and conversions.
module FsMap3.Color

open Common


/// Gamma converts a linear component (one of RGB) value to sRGB.
let gammaConvert x =
  if x <= 0.0031308f then 12.92f * x else 1.055f * x ** Q 1 2.4 - 0.055f



/// Converts a HSV (hue-saturation-value) color to RGB.
/// Each component is in unit range.
let HSVtoRGB H S V =
  let C  = V * S
  let H' = H * 6.0f
  let X  = C * (1.0f - abs((emodf H' 2.0f) - 1.0f))
  let R, G, B =
    match emod (int H') 6 with
    | 0 -> (C, X, 0G) // H = 0/6 = red
    | 1 -> (X, C, 0G) // H = 1/6 = yellow
    | 2 -> (0G, C, X) // H = 2/6 = green
    | 3 -> (0G, X, C) // H = 3/6 = cyan
    | 4 -> (X, 0G, C) // H = 4/6 = blue
    | _ -> (C, 0G, X) // H = 5/6 = magenta
  let m = V - C
  Vec3f(R + m, G + m, B + m)



/// Converts a HSL (hue-saturation-lightness) color to RGB.
/// Each component is in unit range.
let HSLtoRGB H S L =
  let C  = (1.0f - abs(2.0f * L - 1G)) * S
  let H' = H * 6.0f
  let X  = C * (1.0f - abs((emodf H' 2.0f) - 1.0f))
  let R, G, B =
    match emod (int H') 6 with
    | 0 -> (C, X, 0G)
    | 1 -> (X, C, 0G)
    | 2 -> (0G, C, X)
    | 3 -> (0G, X, C)
    | 4 -> (X, 0G, C)
    | _ -> (C, 0G, X)
  let m = L - 0.5f * C
  Vec3f(R + m, G + m, B + m)



/// Converts a RYB (red-yellow-blue) color to RGB. Each component is in unit range.
/// RYB is a subtractive color space - the color white is (0, 0, 0). It is intuitive
/// because the familiar complementary colors red - green, blue - orange, and yellow - purple
/// are inverses of each other.
let RYBtoRGB (R : float32) (Y : float32) (B : float32) =
  let R, Y, B = clamp01 R, clamp01 Y, clamp01 B
  // Values drawn from Gossett, N., Chen, B., Paint Inspired Color Compositing.
  let r00 = lerp (Vec3f(1.000f, 1.000f, 1.000f)) (Vec3f(1.000f, 0.000f, 0.000f)) R
  let r01 = lerp (Vec3f(0.163f, 0.373f, 0.600f)) (Vec3f(0.500f, 0.000f, 0.500f)) R
  let r10 = lerp (Vec3f(1.000f, 1.000f, 0.000f)) (Vec3f(1.000f, 0.500f, 0.000f)) R
  let r11 = lerp (Vec3f(0.000f, 0.666f, 0.200f)) (Vec3f(0.200f, 0.094f, 0.000f)) R
  let ry0 = lerp r00 r10 Y
  let ry1 = lerp r01 r11 Y
  let ryb = lerp ry0 ry1 B
  ryb



/// Converts an XYZ color to RGB. The result is clamped because the XYZ space
/// contains colors that cannot be represented in RGB.
let XYZtoRGB X Y Z = 
  let rgb = Vec3f(3.240479f * X - 1.537150f * Y - 0.498535f * Z,
                 -0.969256f * X + 1.875992f * Y + 0.041556f * Z,
                  0.055648f * X - 0.204043f * Y + 1.057311f * Z)
  rgb.map(clamp01)



/// Converts a CIELab color to XYZ.
let CIELABtoXYZ L a b =
  let Y = (L + 16.0f) / 116.0f
  let X = a / 500.0f + Y
  let Z = Y - b / 200.0f
  let Y = if cubed Y > 0.008856f then cubed Y else (Y - 16.0f / 116.0f) / 7.787f
  let X = if cubed X > 0.008856f then cubed X else (X - 16.0f / 116.0f) / 7.787f
  let Z = if cubed Z > 0.008856f then cubed Z else (Z - 16.0f / 116.0f) / 7.787f
  // Multiply with reference coordinates, here - observer at 2 degrees with a D65 illuminant.
  Vec3f(X * 95.047f, Y * 100.0f, Z * 108.883f)



/// Converts a CIELch color to CIELab. Hue h is in unit range.
let CIELCHtoCIELAB L c h =
  Vec3f(L, c * cosr(h), c * sinr(h))



/// Converts a Munsell chart style color to RGB. The letter 'g' in Mugsell
/// comes from genetic programming, which came up with this approximation.
/// All arguments are in unit range; H is wrapped to unit range if it is not in it.
/// H value at zero (and one - hue wraps around) approximates Munsell Hue R2.5.
/// V is Munsell Value divided by 9 (V = 0 is thus extrapolated).
/// C values run from Munsell Chroma 2 at zero to Chroma 16 at one, with C values
/// biased toward lower Munsell Chromas. Higher Chromas than 16 are not considered.
let MugsellToRGB H V C =
  let C = sqrt (max 0.0f (cubed C))
  let CH1 = C * cosr H / (1.0f + C)
  let CH2 = exp (C * sinr H)
  let CV = C * V
  let R = -0.13661939063f + CH1 * 1.0254504025f + V * 0.8887657987f + CH2 * 0.1404878269f - CV * 0.22558629703f
  let G = -0.06665101167f - CH1 * 0.3521260182f + V * 0.9363955443f + CH2 * 0.0267202001f - CV * 0.07914511401f
  let B = +0.64061183186f - CH1 * 0.1703769390f + V * 0.7830775712f - CH2 * 0.6006639740f + CV * 0.04047916710f
  Vec3f(R, G, B).map(clamp01)



/// Various 3-component color spaces.
type ColorSpace =
  RYB | HSV | HSL | CIELch | Mugsell
  override this.ToString() = unionLabel this



/// Generates a pseudo-random palette as a Map3. It maps abstract component values to RGB,
/// with both inputs and outputs in the range [-1, 1] (as is common with Map3 functions).
/// The grid resolution of the color interpolation cube is n, which must be a power of two
/// (for example, 32). Note that no gamma conversion is done; it is expected that we want
/// to operate in gamma corrected space.
let genPalette (n : int) (dna : Dna) =

  // The palette is a linearly interpolated color cube in the chosen color space.
  enforce (Bits.isPowerOf2 n) "Color.genPalette: Dimension must be a power of two."
  let shift = Bits.bitNumber32 n
  let mask = n - 1

  // Choose a color space.
  let space = dna.category("Color space", C(RYB), C(HSV), C(HSL), C(CIELch), C(Mugsell))

  let hm = dna.float32("Hue origin")
  let hr = dna.float32("Hue range", squared >> lerp 0.05f 0.8f)
  let hM = hm + hr
  let hfade = Fade.skew -(dna.float32("Hue skew", lerp -2.0f 2.0f))

  let sr = dna.float32("Saturation range", lerp 0.2f 0.8f)
  let sm = dna.float32("Saturation minimum", squared >> lerp 0.0f (1.0f - sr))
  let sM = sm + sr
  let sfade = Fade.skew -(dna.float32("Saturation skew", lerp -2.0f 2.0f))

  let vm = 0.0f
  let vM = 1.0f
  let vfade = Fade.skew -(dna.float32("Value skew", lerp -2.0f 2.0f))

  let pick x y z =
    let h = float32 x / float32 (n - 1)
    let h = fract (lerp hm hM (hfade h))
    let s = float32 y / float32 (n - 1)
    let s = lerp sm sM (sfade s)
    let v = float32 z / float32 (n - 1)
    let v = lerp vm vM (vfade v)
    let rgb =
      match space with
      | RYB ->
        // RYB components are interpreted as hue-saturation-value, with RGB replaced by RYB.
        let ryb = Vec3f.one - HSVtoRGB h s v
        RYBtoRGB ryb.x ryb.y ryb.z
      | HSV ->
        HSVtoRGB h s v
      | HSL ->
        HSLtoRGB h s v
      | CIELch ->
        let lab = CIELCHtoCIELAB (v * 10.0f) (s * 10.0f) h
        let xyz = CIELABtoXYZ lab.x lab.y lab.z
        XYZtoRGB xyz.x xyz.y xyz.z
      | Mugsell | _ ->
        MugsellToRGB h v s
    // Transform to [-1, 1] range.
    map01to11 rgb

  let cube = Array.init (cubed n) (fun i -> pick (i >>> (shift * 2)) ((i >>> shift) &&& mask) (i &&& mask))

  let vZ = float32 (n - 1) * 0.5f

  fun (v : Vec3f) ->
    assert (isFinite v.x && isFinite v.y && isFinite v.z)
    let u = v.map(fun x -> vZ * (clamp -0.999f 0.999f x + 1.0f))
    let d = u - u.map(floor)
    let ix = int u.x <<< (shift * 2)
    let jx = int u.x + 1 <<< (shift * 2)
    let iy = int u.y <<< shift
    let jy = int u.y + 1 <<< shift
    let iz = int u.z
    let jz = iz + 1
    let xii = Vec3f.lerp cube.[ix + iy + iz] cube.[jx + iy + iz] d.x
    let xij = Vec3f.lerp cube.[ix + iy + jz] cube.[jx + iy + jz] d.x
    let xji = Vec3f.lerp cube.[ix + jy + iz] cube.[jx + jy + iz] d.x
    let xjj = Vec3f.lerp cube.[ix + jy + jz] cube.[jx + jy + jz] d.x
    let yi = Vec3f.lerp xii xji d.y
    let yj = Vec3f.lerp xij xjj d.y
    Vec3f.lerp yi yj d.z
 
