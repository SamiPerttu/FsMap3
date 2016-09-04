// Single precision 2-vectors.
namespace FsMap3

open Common


/// Single precision 2-vector structure. Note that 2-vectors are not complex numbers:
/// operations are component-wise.
[<NoComparison>]
type Vec2f = struct

  val x : float32
  val y : float32

  /// Builds a 2-vector.
  new(x, y) = { x = x; y = y }

  /// Builds a 2-vector where both components have the same value.
  new(v) = { x = v; y = v }

  /// Builds a vector in the unit square from an integer seed. Each component has 16 bits of precision.
  new(seed : int) =
    let mask = 0xffff
    { x = float32 (seed &&& mask) / float32 mask; y = float32 ((seed >>> 16) &&& mask) / float32 mask }

  /// The length squared of the vector.
  member inline v.length2 = v.x * v.x + v.y * v.y

  /// The length of the vector.
  member inline v.length = sqrt v.length2

  /// Transforms the components of the vector with a function.
  member inline v.map(f : float32 -> float32) = Vec2f(f v.x, f v.y)

  /// The sum of the components.
  member inline v.sum = v.x + v.y

  /// The average of the components.
  member inline v.average = (v.x + v.y) * 0.5f

  /// L1-norm aka Manhattan norm.
  member inline v.norm1 = abs v.x + abs v.y

  /// Maximum norm.
  member inline v.maxNorm = max (abs v.x) (abs v.y)

  /// The L-norm of the vector.
  member inline v.norm(L : float32) = (abs v.x ** L + abs v.y ** L) ** (1.0f / L)

  /// 4-norm of the vector.
  member inline v.norm4 = v.map(fun x -> squared (squared x)).sum |> sqrt |> sqrt

  /// 8-norm of the vector.
  member inline v.norm8 = v.map(fun x -> squared (squared (squared x))).sum |> sqrt |> sqrt |> sqrt

  /// The vector as a tuple.
  member inline v.tuple = (v.x, v.y)

  /// Returns the vector normalized.
  member inline v.normalize =
    let Z = v.length
    if Z > 0.0f then Vec2f(v.x / Z, v.y / Z) else Vec2f(0.0f, 0.0f)

  /// Returns whether the vector is zero.
  member inline v.isZero = v.length2 = 0.0f

  /// Returns whether the vector is non-zero.
  member inline v.isNonZero = v.length2 > 0.0f

  /// Returns the vector rotated angle radians.
  member inline v.rotate(angle) =
    let sin_a = sin(angle)
    let cos_a = cos(angle)
    Vec2f(v.x * cos_a - v.y * sin_a, v.x * sin_a + v.y * cos_a)

  /// Returns the vector rotated using the precomputed sine and cosine.
  member inline v.rotate(sin_a, cos_a) =
    Vec2f(v.x * cos_a - v.y * sin_a, v.x * sin_a + v.y * cos_a)

  /// Rotates a vector 90 degrees.
  member inline v.rotate90 = Vec2f(v.y, -v.x)

  /// Rotates a vector 270 degrees.
  member inline v.rotate270 = Vec2f(-v.y, v.x)

  /// Returns the angle (in radians) of the vector. Returns zero for a zero vector.
  member inline v.angle = atan2 v.y v.x

  /// Vector negation.
  static member inline ( ~- ) (a : Vec2f) = Vec2f(-a.x, -a.y)
  /// Vector sum.
  static member inline ( + ) (a : Vec2f, b : Vec2f) = Vec2f(a.x + b.x, a.y + b.y)
  /// Vector sum.
  static member inline ( + ) (a : Vec2f, b : float32) = Vec2f(a.x + b, a.y + b)
  /// Vector sum.
  static member inline ( + ) (a : float32, b : Vec2f) = Vec2f(a + b.x, a + b.y)
  /// Vector difference.
  static member inline ( - ) (a : Vec2f, b : Vec2f) = Vec2f(a.x - b.x, a.y - b.y)
  /// Vector difference.
  static member inline ( - ) (a : Vec2f, b : float32) = Vec2f(a.x - b, a.y - b)
  /// Vector difference.
  static member inline ( - ) (a : float32, b : Vec2f) = Vec2f(a - b.x, a - b.y)
  /// Vector component product.
  static member inline ( * ) (a : Vec2f, b : Vec2f) = Vec2f(a.x * b.x, a.y * b.y)
  /// Vector component product.
  static member inline ( * ) (a : Vec2f, b : float32) = Vec2f(a.x * b, a.y * b)
  /// Vector component product.
  static member inline ( * ) (a : float32, b : Vec2f) = Vec2f(a * b.x, a * b.y)
  /// Vector component division.
  static member inline ( / ) (a : Vec2f, b : Vec2f) = Vec2f(a.x / b.x, a.y / b.y)
  /// Vector component division.
  static member inline ( / ) (a : Vec2f, b : float32) = Vec2f(a.x / b, a.y / b)
  /// Vector component division.
  static member inline ( / ) (a : float32, b : Vec2f) = Vec2f(a / b.x, a / b.y)
  /// Dot product.
  static member inline Dot(a : Vec2f, b : Vec2f) = a.x * b.x + a.y * b.y
  /// Perpendicular dot product: a *^ b = a *. b.rotate90.
  static member inline ( *^ ) (a : Vec2f, b : Vec2f) = a.x * b.y - a.y * b.x
  /// Aligns a with b, that is, inverts a if it points away from b.
  static member inline ( ^^ ) (a : Vec2f, b : Vec2f) = a * signum (a *. b)

  /// Reflects this vector from the given normal vector, which must be of unit length
  /// and point toward this vector.
  member inline v.reflect(normal : Vec2f) : Vec2f = v - 2.0f * (v *. normal) * normal

  /// The sign of the angle from this vector to w: -1 for angle in ]-pi, 0[,
  /// 1 for angle in ]0, pi[, or 0 for angle of 0 or pi.
  member inline v.angleSign(w : Vec2f) = sign (v *^ w)

  /// The zero vector.
  static member zero = Vec2f(0.0f, 0.0f)

  /// The zero vector.
  static member Zero = Vec2f(0.0f, 0.0f)

  /// The one vector.
  static member one = Vec2f(1.0f, 1.0f)

  /// The one vector.
  static member One = Vec2f(1.0f, 1.0f)

  /// The unit X vector.
  static member unitX = Vec2f(1.0f, 0.0f)

  /// The unit Y vector.
  static member unitY = Vec2f(0.0f, 1.0f)

  /// Cartesian distance between two vectors.
  static member distance(a : Vec2f, b : Vec2f) = (a - b).length

  /// Creates a direction vector from an angle (in radians).
  static member direction(angle : float32) = Vec2f(cos angle, sin angle)
  
  // Generic number literal support.
  static member inline genericNumber(x : int32, _ : Vec2f) = Vec2f(float32 x)
  static member inline genericNumber(x : int64, _ : Vec2f) = Vec2f(float32 x)
  static member inline genericNumber(x : string, _ : Vec2f) = Vec2f(float32 x)

  override v.ToString() = "Vec2f(" + string (float v.x) + ", " + string (float v.y) + ")"

end



