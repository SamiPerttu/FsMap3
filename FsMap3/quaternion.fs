// Quaternions.
namespace Fuse

open Common

// Note. Most of the formulas in this module have been verified multiple times.
// But it would not hurt to introduce some tests here.


/// Quaternion structure, double precision. Among other things, quaternions are a convenient representation
/// for rotations in 3-space. Quaternions corresponding to rotations are normalized; this is not
/// enforced automatically but is mentioned in any methods requiring it.
[<NoComparison>]
type Quaternion = struct

  val w : float
  val x : float
  val y : float
  val z : float

  /// Builds an arbitrary quaternion.
  new(w, x, y, z) = { w = w; x = x; y = y; z = z }

  /// Builds a rotation from axis and angle. Axis must be normalized.
  new(axis : Vec3, angle) =
    let s = sin (angle * 0.5)
    let w = cos (angle * 0.5)
    let x = axis.x * s
    let y = axis.y * s
    let z = axis.z * s
    { w = w; x = x; y = y; z = z }

  /// The length of the quaternion, also known as magnitude.
  member inline q.length = sqrt(q.w * q.w + q.x * q.x + q.y * q.y + q.z * q.z)

  /// Length squared of the quaternion.
  member inline q.length2 = q.w * q.w + q.x * q.x + q.y * q.y + q.z * q.z

  /// The angle of rotation of the (normalized) quaternion.
  member inline q.angle = 2G * acos q.w

  /// Conjugate of the quaternion.
  member inline q.conjugate = Quaternion(q.w, -q.x, -q.y, -q.z)

  /// Converts the quaternion to single precision.
  member inline q.quaternionf = Quaternionf(float32 q.w, float32 q.x, float32 q.y, float32 q.z)

  /// The axis of rotation of the (normalized) quaternion.
  member q.axis =
    let s = sqrt (q.x * q.x + q.y * q.y + q.z * q.z)
    if s > 0G then
      let si = 1G / s in Vec3(q.x * si, q.y * si, q.z * si)
    else
      /// This is an identity rotation. Return an arbitrary axis.
      Vec3(1G, 0G, 0G)

  /// Returns the quaternion normalized.
  member q.normalize =
    let n = q.length
    if n > 0G then
      let ni = 1G / n
      Quaternion(q.w * ni, q.x * ni, q.y * ni, q.z * ni)
    else
      Quaternion.one

  /// Returns the inverse of the quaternion. This is also called the reciprocal.
  member q.inverse =
    let n2 = q.length2
    if n2 > 0G then
      let n2i = 1G / n2
      Quaternion(q.w * n2i, -q.x * n2i, -q.y * n2i, -q.z * n2i)
    else
      Quaternion.one

  /// X basis vector of the (normalized) quaternion.
  /// This is the first column of the corresponding 3x3 rotation matrix.
  member q.basisX =
    let x = 1G - (2G * q.y * q.y) - (2G * q.z * q.z)
    let y = (2G * q.x * q.y) + (2G * q.w * q.z)
    let z = (2G * q.x * q.z) - (2G * q.w * q.y)
    Vec3(x, y, z)

  /// Y basis vector of the (normalized) quaternion.
  /// This is the second column of the corresponding 3x3 rotation matrix.
  member q.basisY =
    let x = (2G * q.x * q.y) - (2G * q.w * q.z)
    let y = 1G - (2G * q.x * q.x) - (2G * q.z * q.z)
    let z = (2G * q.y * q.z) + (2G * q.w * q.x)
    Vec3(x, y, z)
  
  /// Z basis vector of the (normalized) quaternion.
  /// This is the third column of the corresponding 3x3 rotation matrix.
  member q.basisZ =
    let x = (2G * q.x * q.z) + (2G * q.w * q.y)
    let y = (2G * q.y * q.z) - (2G * q.w * q.x)
    let z = 1G - (2G * q.x * q.x) - (2G * q.y * q.y)
    Vec3(x, y, z)

  /// Composition of rotations. The result composes rotation q0 followed by rotation q1.
  /// Composing a rotation with its inverse results in the identity quaternion.
  static member ( * ) (q0 : Quaternion, q1 : Quaternion) =
    // Note. This is called "Hamilton product" in Wikipedia. In Math.NET, quaternion product
    // is defined with the arguments reversed. I don't know why.
    let w = (q0.w * q1.w) - (q0.x * q1.x) - (q0.y * q1.y) - (q0.z * q1.z)
    let x = (q0.w * q1.x) + (q0.x * q1.w) + (q0.y * q1.z) - (q0.z * q1.y)
    let y = (q0.w * q1.y) - (q0.x * q1.z) + (q0.y * q1.w) + (q0.z * q1.x)
    let z = (q0.w * q1.z) + (q0.x * q1.y) - (q0.y * q1.x) + (q0.z * q1.w)
    Quaternion(w, x, y, z)

  /// Vector transformation with the (normalized) quaternion.
  static member inline ( * ) (v : Vec3, q : Quaternion) =
    let i = Vec3(q.x, q.y, q.z)
    v + (i * 2G) *+ ((i *+ v) + v * q.w)

  static member inline ( + ) (q0 : Quaternion, q1 : Quaternion) = Quaternion(q0.w + q1.w, q0.x + q1.x, q0.y + q1.y, q0.z + q1.z)
  static member inline ( - ) (q0 : Quaternion, q1 : Quaternion) = Quaternion(q0.w - q1.w, q0.x - q1.x, q0.y - q1.y, q0.z - q1.z)
  static member inline ( * ) (q : Quaternion, c : float) = Quaternion(q.w * c, q.x * c, q.y * c, q.z * c)
  static member inline ( * ) (c : float, q : Quaternion) = Quaternion(c * q.w, c * q.x, c * q.y, c * q.z)
  static member inline ( / ) (q : Quaternion, c : float) = let ci = 1G / c in Quaternion(q.w * ci, q.x * ci, q.y * ci, q.z * ci)
  static member inline ( / ) (c : float, q : Quaternion) = Quaternion(c / q.w, c / q.x, c / q.y, c / q.z)

  /// Dot product.
  static member inline Dot(q0 : Quaternion, q1 : Quaternion) = q0.w * q1.w + q0.x * q1.x + q0.y * q1.y + q0.z * q1.z

  /// The multiplicative identity element (the identity rotation).
  static member one = Quaternion(1G, 0G, 0G, 0G)

  /// The multiplicative identity element (the identity rotation).
  static member One = Quaternion(1G, 0G, 0G, 0G)

  /// The additive identity element (the zero vector).
  static member zero = Quaternion()

  /// The additive identity element (the zero vector).
  static member Zero = Quaternion()

  /// Builds a rotation from v to u. The input vectors need not be normalized.
  static member between(v : Vec3, u : Vec3) =
    let w = v *+ u
    let q = Quaternion(v *. u, w.x, w.y, w.z)
    Quaternion(q.w + q.length, q.x, q.y, q.z).normalize

  /// "Triplex" multiplication. Used in some fractal iteration formulas.
  static member triplex(p : Quaternion, q : Quaternion) =
    Quaternion(p.w * q.w + p.x * q.x - p.y * q.y - p.z * q.z,
               p.w * q.x + p.x * q.w - p.y * q.z - p.z * q.y,
               p.w * q.y + p.x * q.z + p.y * q.w + p.z * q.x,
               p.w * q.z + p.x * q.y + p.y * q.x + p.z * q.w)

  /// Spherical linear interpolation - interpolates between two rotations.
  /// t = 0 results in q0, t = 1 results in q1. Both q0 and q1 must be normalized.
  static member slerp(q0 : Quaternion, q1 : Quaternion, t) =
    let d = q0 *. q1
    if d > 0.9999 then
      // We can't compute a robust orthonormal basis for the rotation if the quaternions are too close,
      // therefore we use linear interpolation instead.
      let q = lerp q0 q1 t
      q.normalize
    else
      // Make sure input is valid for the acos function.
      let d = clamp11 d
      let h = t * acos d
      let q2 = (q1 - (q0 * d)).normalize
      (q0 * (cos h)) + (q2 * (sin h))

  override q.ToString() = "Quaternion(" + Pretty.string q.w + ", " + Pretty.string q.x + ", " + Pretty.string q.y + ", " + Pretty.string q.z + ")"

end



/// Quaternion structure, single precision. Among other things, quaternions are a convenient representation
/// for rotations in 3-space. Quaternions corresponding to rotations are normalized; this is not
/// enforced automatically but is mentioned in any methods requiring it.
and [<NoComparison>] Quaternionf = struct

  val w : float32
  val x : float32
  val y : float32
  val z : float32

  /// Builds an arbitrary quaternion.
  new(w, x, y, z) = { w = w; x = x; y = y; z = z }

  /// Builds a rotation from axis and angle. Axis must be normalized.
  new(axis : Vec3f, angle) =
    let s = sin (angle * 0.5f)
    let w = cos (angle * 0.5f)
    let x = axis.x * s
    let y = axis.y * s
    let z = axis.z * s
    { w = w; x = x; y = y; z = z }

  /// The length of the quaternion, also known as magnitude.
  member inline q.length = sqrt(q.w * q.w + q.x * q.x + q.y * q.y + q.z * q.z)

  /// Length squared of the quaternion.
  member inline q.length2 = q.w * q.w + q.x * q.x + q.y * q.y + q.z * q.z

  /// The angle of rotation of the (normalized) quaternion.
  member inline q.angle = 2G * acos q.w

  /// Conjugate of the quaternion.
  member inline q.conjugate = Quaternionf(q.w, -q.x, -q.y, -q.z)

  /// Converts the quaternion to double precision.
  member inline q.quaternion = Quaternion(float q.w, float q.x, float q.y, float q.z)

  /// The axis of rotation of the (normalized) quaternion.
  member q.axis =
    let s = sqrt (q.x * q.x + q.y * q.y + q.z * q.z)
    if s > 0G then
      let si = 1G / s in Vec3f(q.x * si, q.y * si, q.z * si)
    else
      /// This is an identity rotation. Return an arbitrary axis.
      Vec3f.unitX

  /// Returns the quaternion normalized.
  member q.normalize =
    let n = q.length
    if n > 0G then
      let ni = 1G / n
      Quaternionf(q.w * ni, q.x * ni, q.y * ni, q.z * ni)
    else
      Quaternionf.one

  /// Returns the inverse of the quaternion. This is also called the reciprocal.
  member q.inverse =
    let n2 = q.length2
    if n2 > 0G then
      let n2i = 1G / n2
      Quaternionf(q.w * n2i, -q.x * n2i, -q.y * n2i, -q.z * n2i)
    else
      Quaternionf.one

  /// X basis vector of the (normalized) quaternion.
  /// This is the first column of the corresponding 3x3 rotation matrix.
  member q.basisX =
    let x = 1G - (2G * q.y * q.y) - (2G * q.z * q.z)
    let y = (2G * q.x * q.y) + (2G * q.w * q.z)
    let z = (2G * q.x * q.z) - (2G * q.w * q.y)
    Vec3f(x, y, z)

  /// Y basis vector of the (normalized) quaternion.
  /// This is the second column of the corresponding 3x3 rotation matrix.
  member q.basisY =
    let x = (2G * q.x * q.y) - (2G * q.w * q.z)
    let y = 1G - (2G * q.x * q.x) - (2G * q.z * q.z)
    let z = (2G * q.y * q.z) + (2G * q.w * q.x)
    Vec3f(x, y, z)
  
  /// Z basis vector of the (normalized) quaternion.
  /// This is the third column of the corresponding 3x3 rotation matrix.
  member q.basisZ =
    let x = (2G * q.x * q.z) + (2G * q.w * q.y)
    let y = (2G * q.y * q.z) - (2G * q.w * q.x)
    let z = 1G - (2G * q.x * q.x) - (2G * q.y * q.y)
    Vec3f(x, y, z)

  /// Composition of rotations. The result composes rotation q1 followed by rotation q0.
  /// Composing a rotation with its inverse results in the identity quaternion.
  static member ( * ) (q0 : Quaternionf, q1 : Quaternionf) =
    let w = (q0.w * q1.w) - (q0.x * q1.x) - (q0.y * q1.y) - (q0.z * q1.z)
    let x = (q0.w * q1.x) + (q0.x * q1.w) + (q0.y * q1.z) - (q0.z * q1.y)
    let y = (q0.w * q1.y) - (q0.x * q1.z) + (q0.y * q1.w) + (q0.z * q1.x)
    let z = (q0.w * q1.z) + (q0.x * q1.y) - (q0.y * q1.x) + (q0.z * q1.w)
    Quaternionf(w, x, y, z)

  /// Vector transformation with the (normalized) quaternion.
  static member inline ( * ) (v : Vec3f, q : Quaternionf) =
    let i = Vec3f(q.x, q.y, q.z)
    v + (i * 2G) *+ ((i *+ v) + v * q.w)

  static member inline ( + ) (q0 : Quaternionf, q1 : Quaternionf) = Quaternionf(q0.w + q1.w, q0.x + q1.x, q0.y + q1.y, q0.z + q1.z)
  static member inline ( - ) (q0 : Quaternionf, q1 : Quaternionf) = Quaternionf(q0.w - q1.w, q0.x - q1.x, q0.y - q1.y, q0.z - q1.z)
  static member inline ( * ) (q : Quaternionf, c : float32) = Quaternionf(q.w * c, q.x * c, q.y * c, q.z * c)
  static member inline ( * ) (c : float32, q : Quaternionf) = Quaternionf(c * q.w, c * q.x, c * q.y, c * q.z)
  static member inline ( / ) (q : Quaternionf, c : float32) = let c' = 1G / c in Quaternionf(q.w * c', q.x * c', q.y * c', q.z * c')
  static member inline ( / ) (c : float32, q : Quaternionf) = Quaternionf(c / q.w, c / q.x, c / q.y, c / q.z)

  /// Dot product.
  static member inline Dot(q0 : Quaternionf, q1 : Quaternionf) = q0.w * q1.w + q0.x * q1.x + q0.y * q1.y + q0.z * q1.z

  /// The multiplicative identity element (the identity rotation).
  static member one = Quaternionf(1G, 0G, 0G, 0G)

  /// The multiplicative identity element (the identity rotation).
  static member One = Quaternionf(1G, 0G, 0G, 0G)

  /// The additive identity element (the zero vector).
  static member zero = Quaternionf()

  /// The additive identity element (the zero vector).
  static member Zero = Quaternionf()

  /// Builds a rotation from v to u. The input vectors need not be normalized.
  static member between(v : Vec3f, u : Vec3f) =
    let w = v *+ u
    let q = Quaternionf(v *. u, w.x, w.y, w.z)
    Quaternionf(q.w + q.length, q.x, q.y, q.z).normalize

  /// "Triplex" multiplication. Used in some fractal iteration formulas.
  static member triplex(p : Quaternionf, q : Quaternionf) =
    Quaternionf(p.w * q.w + p.x * q.x - p.y * q.y - p.z * q.z,
                p.w * q.x + p.x * q.w - p.y * q.z - p.z * q.y,
                p.w * q.y + p.x * q.z + p.y * q.w + p.z * q.x,
                p.w * q.z + p.x * q.y + p.y * q.x + p.z * q.w)

  /// Spherical linear interpolation - interpolates between two rotations.
  /// t = 0 results in q0, t = 1 results in q1. Both q0 and q1 must be normalized.
  static member slerp(q0 : Quaternionf, q1 : Quaternionf, t) =
    let d = q0 *. q1
    if d > 0.9999f then
      // We can't compute a robust orthonormal basis for the rotation if the quaternions are too close,
      // therefore we use linear interpolation instead.
      let q = lerp q0 q1 t
      q.normalize
    else
      // Make sure input is valid for the acos function.
      let d = clamp11 d
      let h = t * acos d
      let q2 = (q1 - (q0 * d)).normalize
      (q0 * (cos h)) + (q2 * (sin h))

  override q.ToString() = "Quaternionf(" + Pretty.string q.w + ", " + Pretty.string q.x + ", " + Pretty.string q.y + ", " + Pretty.string q.z + ")"

end




