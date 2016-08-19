/// Color spaces and conversions.
namespace FsMap3

open Common


/// Various 3-component color spaces.
type ColorSpace =
  RGB | RYB | XYZ | HSV | HSL | HSD | CIELch | Mugsell

  override this.ToString() = unionLabel this



module Color =

  /// Gamma converts a linear component value to sRGB.
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
      | 0 -> C, X, 0G // H = 0/6 = red
      | 1 -> X, C, 0G // H = 1/6 = yellow
      | 2 -> 0G, C, X // H = 2/6 = green
      | 3 -> 0G, X, C // H = 3/6 = cyan
      | 4 -> X, 0G, C // H = 4/6 = blue
      | _ -> C, 0G, X // H = 5/6 = magenta
    let m = V - C
    Vec3f(R + m, G + m, B + m)


  /// Converts a HSL (hue-saturation-lightness) color to RGB.
  /// Each component is in unit range.
  let HSLtoRGB H S L =
    // Compared to the bog standard conversion, we smooth out the slope discontinuity at L = 1/2.
    let C  = Fade.smoothLine (1.0f - abs(2.0f * L - 1G)) * S
    let H' = H * 6.0f
    let X  = C * (1.0f - abs((emodf H' 2.0f) - 1.0f))
    let R, G, B =
      match emod (int H') 6 with
      | 0 -> C, X, 0G
      | 1 -> X, C, 0G
      | 2 -> 0G, C, X
      | 3 -> 0G, X, C
      | 4 -> X, 0G, C
      | _ -> C, 0G, X
    let m = L - 0.5f * C
    Vec3f(R + m, G + m, B + m)


  /// Converts a RYB (red-yellow-blue) color to RGB. Each component is in unit range.
  /// RYB is a subtractive color space. It is intuitive because the familiar complementary colors
  /// red - green, blue - orange, and yellow - purple are inverses of each other.
  let RYBtoRGB (R : float32) (Y : float32) (B : float32) =
    let R, Y, B = clamp01 R, clamp01 Y, clamp01 B
    // These are original values from Gossett, N., Chen, B., Paint Inspired Color Compositing.
    //let r00 = lerp (Vec3f(0.200f, 0.094f, 0.000f)) (Vec3f(0.000f, 0.666f, 0.200f)) R
    //let r01 = lerp (Vec3f(1.000f, 0.500f, 0.000f)) (Vec3f(1.000f, 1.000f, 0.000f)) R
    //let r10 = lerp (Vec3f(0.500f, 0.000f, 0.500f)) (Vec3f(0.163f, 0.373f, 0.600f)) R
    //let r11 = lerp (Vec3f(1.000f, 0.000f, 0.000f)) (Vec3f(1.000f, 1.000f, 1.000f)) R
    // These are values tweaked for a fuller range.
    let r00 = lerp (Vec3f(0.050f, 0.025f, 0.000f)) (Vec3f(0.000f, 1.000f, 0.300f)) R
    let r01 = lerp (Vec3f(1.000f, 0.500f, 0.000f)) (Vec3f(1.000f, 1.000f, 0.000f)) R
    let r10 = lerp (Vec3f(0.800f, 0.000f, 0.800f)) (Vec3f(0.290f, 0.610f, 1.000f)) R
    let r11 = lerp (Vec3f(1.000f, 0.000f, 0.000f)) (Vec3f(1.000f, 1.000f, 1.000f)) R
    let ry1 = lerp r01 r11 Y
    let ry0 = lerp r00 r10 Y
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
    let epsilon = 0.008856f
    let kappa = 903.3f
    let Y = if L > epsilon * kappa then cubed Y else L / kappa
    let X = if cubed X > epsilon then cubed X else (116.0f * X - 16.0f) / kappa
    let Z = if cubed Z > epsilon then cubed Z else (116.0f * Z - 16.0f) / kappa
    // Multiply with reference coordinates, here - observer at 2 degrees with a D65 illuminant.
    Vec3f(X * 95.047f, Y * 100.0f, Z * 108.883f)


  /// Converts a CIELch color to CIELab. Hue h is in unit range.
  let CIELCHtoCIELAB h c L =
    Vec3f(L, c * cosrFast(h), c * sinrFast(h))


  /// Converts a Munsell chart style color to RGB. The letter 'g' in Mugsell
  /// comes from genetic programming, which came up with this approximation.
  /// All arguments are in unit range; H is wrapped to unit range if it is not in it.
  /// H value at zero (and one - hue wraps around) approximates Munsell Hue R2.5.
  /// C values run from Munsell Chroma 2 at C = 0 to Chroma 16 at C = 1, with C values
  /// biased toward lower Munsell Chromas. Higher Chromas than 16 are not considered.
  /// V is Munsell Value divided by 9 (V = 0 is thus extrapolated).
  let MugsellToRGB H C V =
    let C = sqrt (max 0.0f (cubed C))
    let CH1 = C * cosrFast H / (1.0f + C)
    let CH2 = exp (C * sinrFast H)
    let CV = C * V
    let R = -0.13661939063f + CH1 * 1.0254504025f + V * 0.9777657987f + CH2 * 0.1404878269f - CV * 0.24758629703f
    let G = -0.06665101167f - CH1 * 0.3521260182f + V * 1.0296955443f + CH2 * 0.0267202001f - CV * 0.08724511401f
    let B = +0.64061183186f - CH1 * 0.1703769390f + V * 0.8613775712f - CH2 * 0.6006639740f + CV * 0.04047916710f
    Vec3f(R, G, B).map(clamp01)



[<AutoOpen>]
module ColorSpaceExtensions =

  type ColorSpace with
    /// Converts a color from this space to RGB. Input and output components are in [0, 1].
    /// Parameter a represents red, X or hue.
    /// Parameter b represents green, yellow, Y or saturation.
    /// Parameter c represents blue, Z, value or lightness.
    member this.rgb(a, b, c) =
      match this with
      | RGB ->
        Vec3f(a, b, c)
      | RYB ->
        Color.RYBtoRGB a b c
      | XYZ ->
        Color.XYZtoRGB a b c
      | HSV ->
        Color.HSVtoRGB a b c
      | HSL ->
        Color.HSLtoRGB a b c
      | HSD ->
        // RYB components are interpreted as hue-saturation-value, with RGB replaced by RYB.
        let ryb = Color.HSVtoRGB a b c
        Color.RYBtoRGB ryb.x ryb.y ryb.z
      | CIELch ->
        let lab = Color.CIELCHtoCIELAB a (b * 9.0f) (c * 9.0f)
        let xyz = Color.CIELABtoXYZ lab.x lab.y lab.z
        Color.XYZtoRGB xyz.x xyz.y xyz.z
      | Mugsell ->
        Color.MugsellToRGB a b c
