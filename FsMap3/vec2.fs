/// Floating point 2-vectors.
namespace Fuse

open Common


/// 2-vector structure with component arithmetic, double precision.
[<NoComparison>]
type Vec2 = struct

  val x : float
  val y : float

  /// Builds a 2-vector.
  new(x, y) = { x = x; y = y }

  /// Builds a 2-vector where both components have the same value.
  new(c : float) = { x = c; y = c }

  /// The length squared of the vector.
  member inline v.length2 = v.x * v.x + v.y * v.y

  /// The length of the vector.
  member inline v.length = sqrt v.length2

  /// The vector as a tuple.
  member inline v.tuple = (v.x, v.y)

  /// Returns the vector normalized.
  member inline v.normalize =
    let Z = v.length
    if Z > 0.0 then Vec2(v.x / Z, v.y / Z) else Vec2(0.0, 0.0)

  /// Returns whether the vector is zero.
  member inline v.isZero = v.length2 = 0.0

  /// Returns whether the vector is non-zero.
  member inline v.isNonZero = v.length2 > 0.0

  /// Transforms the components of the vector with a function.
  member inline v.map (f : float -> float) = Vec2(f v.x, f v.y)

  /// Returns the vector rotated angle radians.
  member inline v.rotate(angle) =
    let sin_a = sin(angle)
    let cos_a = cos(angle)
    Vec2(v.x * cos_a - v.y * sin_a, v.x * sin_a + v.y * cos_a)

  /// Returns the vector rotated using the precomputed sine and cosine.
  member inline v.rotate(sin_a, cos_a) =
    Vec2(v.x * cos_a - v.y * sin_a, v.x * sin_a + v.y * cos_a)

  /// Rotates a vector 90 degrees.
  member inline v.rotate90 = Vec2(v.y, -v.x)

  /// Rotates a vector 270 degrees.
  member inline v.rotate270 = Vec2(-v.y, v.x)

  /// Returns the angle (in radians in [0, 2pi[) of the vector. Returns zero for the zero vector.
  member inline v.angle = atan2 v.y v.x

  /// Converts the vector to single precision.
  member inline v.vec2f = Vec2f(float32 v.x, float32 v.y)

  /// Vector negation.
  static member inline ( ~- ) (a : Vec2) = Vec2(-a.x, -a.y)
  /// Vector sum.
  static member inline ( + ) (a : Vec2, b : Vec2) = Vec2(a.x + b.x, a.y + b.y)
  /// Vector sum.
  static member inline ( + ) (a : Vec2, b : float) = Vec2(a.x + b, a.y + b)
  /// Vector sum.
  static member inline ( + ) (a : float, b : Vec2) = Vec2(a + b.x, a + b.y)
  /// Vector difference.
  static member inline ( - ) (a : Vec2, b : Vec2) = Vec2(a.x - b.x, a.y - b.y)
  /// Vector difference.
  static member inline ( - ) (a : Vec2, b : float) = Vec2(a.x - b, a.y - b)
  /// Vector difference.
  static member inline ( - ) (a : float, b : Vec2) = Vec2(a - b.x, a - b.y)
  /// Vector component product.
  static member inline ( * ) (a : Vec2, b : Vec2) = Vec2(a.x * b.x, a.y * b.y)
  /// Vector component product.
  static member inline ( * ) (a : Vec2, b : float) = Vec2(a.x * b, a.y * b)
  /// Vector component product.
  static member inline ( * ) (a : float, b : Vec2) = Vec2(a * b.x, a * b.y)
  /// Vector component division.
  static member inline ( / ) (a : Vec2, b : Vec2) = Vec2(a.x / b.x, a.y / b.y)
  /// Vector component division.
  static member inline ( / ) (a : Vec2, b : float) = Vec2(a.x / b, a.y / b)
  /// Vector component division.
  static member inline ( / ) (a : float, b : Vec2) = Vec2(a / b.x, a / b.y)
  /// Dot product.
  static member inline Dot(a : Vec2, b : Vec2) = a.x * b.x + a.y * b.y
  /// Perpendicular dot product: a *^ b = a *. b.rotate90.
  static member inline ( *^ ) (a : Vec2, b : Vec2) = a.x * b.y - a.y * b.x
  /// Aligns a with b, that is, inverts a if it points away from b.
  static member inline ( ^^ ) (a : Vec2, b : Vec2) = a * signum (a *. b)

  /// Reflects this vector from the given normal vector, which must be of unit length
  /// and point toward this vector.
  member inline v.reflect(normal : Vec2) : Vec2 = v - 2.0 * (v *. normal) * normal

  /// The sign of the angle from this vector to w: -1 for angle in ]-pi, 0[,
  /// 1 for angle in ]0, pi[, or 0 for angle of 0 or pi.
  member inline v.angleSign(w : Vec2) = sign (v *^ w)
  
  /// The zero vector.
  static member zero = Vec2(0.0, 0.0)
  static member Zero = Vec2(0.0, 0.0)

  /// The one vector.
  static member one = Vec2(1.0, 1.0)
  static member One = Vec2(1.0, 1.0)

  /// The unit X vector.
  static member unitX = Vec2(1.0, 0.0)

  /// The unit Y vector.
  static member unitY = Vec2(0.0, 1.0)

  /// Cartesian distance between two vectors.
  static member distance(a : Vec2, b : Vec2) = (a - b).length

  /// Creates a direction vector from an angle (in radians).
  static member direction(angle) = Vec2(cos angle, sin angle)
  
  /// Builds a vector from an integer seed with components in the range [min, max]. Each component has 16 bits of precision.
  static member fromSeed(seed : int, min : float, max : float) =
    let mask = 0xffff
    let Z = (max - min) / float mask
    Vec2(float (seed &&& mask) * Z + min, float ((seed >>> 16) &&& mask) * Z + min)

  /// Builds a vector in the unit square from an integer seed. Each component has 16 bits of precision.
  static member fromSeed(seed : int) = Vec2.fromSeed(seed, 0.0, 1.0)

  override v.ToString() = "Vec2(" + Pretty.string v.x + ", " + Pretty.string v.y + ")"

end



[<AutoOpen>]
module Vec2Extensions =

  type Vec2f with
    /// Converts the vector to double precision.
    member inline v.vec2 = Vec2(float v.x, float v.y)

