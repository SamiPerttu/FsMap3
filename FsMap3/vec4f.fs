// Single precision 4-vectors.
namespace FsMap3

open Common


/// Single precision 4-vector structure with component arithmetic.
[<NoComparison>]
type Vec4f = struct

  val x : float32
  val y : float32
  val z : float32
  val w : float32

  new(x, y, z, w) = { x = x; y = y; z = z; w = w }
  new(c) = { x = c; y = c; z = c; w = c }

  /// The length squared of the vector.
  member inline v.length2 = v.x * v.x + v.y * v.y + v.z * v.z + v.w * v.w

  /// The length of the vector.
  member inline v.length = sqrt v.length2

  /// Absolute norm, also called L1-norm.
  member inline v.absNorm = abs v.x + abs v.y + abs v.z + abs v.w

  /// Maximum norm.
  member inline v.maxNorm = max4 (abs v.x) (abs v.y) (abs v.z) (abs v.w)

  /// The L-norm of the vector.
  member inline v.norm(L : float32) = (abs v.x ** L + abs v.y ** L + abs v.z ** L + abs v.w ** L) ** (1.0f / L)

  /// The average of the components.
  member inline v.average = (v.x + v.y + v.z + v.w) * Q 1 4

  /// The smallest of the components.
  member inline v.minimum = min4 v.x v.y v.z v.w

  /// The largest of the components.
  member inline v.maximum = max4 v.x v.y v.z v.w

  /// The vector as an array.
  member v.toArray = [| v.x; v.y; v.z; v.w |]

  /// The vector as a list.
  member v.toList = [ v.x; v.y; v.z; v.w ]

  /// The vector as a tuple.
  member inline v.tuple = (v.x, v.y, v.z, v.w)

  /// The sum of the components.
  member inline v.sum = v.x + v.y + v.z + v.w

  /// Transforms the components of the vector with a function.
  member inline v.map(f : float32 -> float32) = Vec4f(f v.x, f v.y, f v.z, f v.w)

  /// Reduces the components of a vector with a binary operator.
  member inline v.reduce(op : float32 -> float32 -> float32) = op (op (op v.x v.y) v.z) v.w

  /// Reduces the projected components of a vector with a binary operator.
  member inline v.reduceWith(projection, op) = op (op (op (projection v.x) (projection v.y)) (projection v.z)) (projection v.w)

  /// Returns the vector normalized (or the zero vector).
  member v.normalize =
    let length = v.length2
    if length > 0.0f then
      let Z = 1.0f / sqrt length
      Vec4f(v.x * Z, v.y * Z, v.z * Z, v.w * Z)
    else v

  /// The zero vector.
  static member inline zero = Vec4f(0.0f)

  /// The zero vector.
  static member inline Zero = Vec4f(0.0f)

  /// The one vector.
  static member inline one = Vec4f(1.0f)

  /// The one vector.
  static member inline One = Vec4f(1.0f)

  /// The unit X vector.
  static member inline unitX = Vec4f(1.0f, 0.0f, 0.0f, 0.0f)

  /// The unit Y vector.
  static member inline unitY = Vec4f(0.0f, 1.0f, 0.0f, 0.0f)

  /// The unit Z vector.
  static member inline unitZ = Vec4f(0.0f, 0.0f, 1.0f, 0.0f)

  /// The unit W vector.
  static member inline unitW = Vec4f(0.0f, 0.0f, 0.0f, 1.0f)

  static member inline ( ~- ) (a : Vec4f) = Vec4f(-a.x, -a.y, -a.z, -a.w)
  static member inline ( + ) (a : Vec4f, b : Vec4f) = Vec4f(a.x + b.x, a.y + b.y, a.z + b.z, a.w + b.w)
  static member inline ( + ) (a : Vec4f, b : float32) = Vec4f(a.x + b, a.y + b, a.z + b, a.w + b)
  static member inline ( + ) (a : float32, b : Vec4f) = Vec4f(a + b.x, a + b.y, a + b.z, a + b.w)
  static member inline ( - ) (a : Vec4f, b : Vec4f) = Vec4f(a.x - b.x, a.y - b.y, a.z - b.z, a.w - b.w)
  static member inline ( - ) (a : Vec4f, b : float32) = Vec4f(a.x - b, a.y - b, a.z - b, a.w - b)
  static member inline ( - ) (a : float32, b : Vec4f) = Vec4f(a - b.x, a - b.y, a - b.z, a - b.w)
  static member inline ( / ) (a : Vec4f, b : Vec4f) = Vec4f(a.x / b.x, a.y / b.y, a.z / b.z, a.w / b.w)
  static member inline ( / ) (a : Vec4f, b : float32) = Vec4f(a.x / b, a.y / b, a.z / b, a.w / b)
  static member inline ( / ) (a : float32, b : Vec4f) = Vec4f(a / b.x, a / b.y, a / b.z, a / b.w)
  static member inline ( * ) (a : Vec4f, b : Vec4f) = Vec4f(a.x * b.x, a.y * b.y, a.z * b.z, a.w * b.w)
  static member inline ( * ) (a : Vec4f, b : float32) = Vec4f(a.x * b, a.y * b, a.z * b, a.w * b)
  static member inline ( * ) (a : float32, b : Vec4f) = Vec4f(a * b.x, a * b.y, a * b.z, a * b.w)
  /// Dot product.
  static member inline Dot(a : Vec4f, b : Vec4f) = a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w

  static member inline Abs (v : Vec4f) = v.map(abs)
  static member inline Ceiling (v : Vec4f) = v.map(ceil)
  static member inline Cos (v : Vec4f) = v.map(cos)
  static member inline Exp (v : Vec4f) = v.map(exp)
  static member inline Floor (v : Vec4f) = v.map(floor)
  static member inline Log (v : Vec4f) = v.map(log)
  static member inline Pow (v : Vec4f, b : float32) = Vec4f(v.x ** b, v.y ** b, v.z ** b, v.w ** b)
  static member inline Pow (u : Vec4f, v : Vec4f) = Vec4f(u.x ** v.x, u.y ** v.y, u.z ** v.z, u.w ** v.w)
  static member inline Round (v : Vec4f) = v.map(round)
  static member inline Sin (v : Vec4f) = v.map(sin)
  static member inline Sqrt (v : Vec4f) = v.map(sqrt)
  static member inline Tan (v : Vec4f) = v.map(tan)
  static member inline Tanh (v : Vec4f) = v.map(tanh)
  static member inline Truncate (v : Vec4f) = v.map(truncate)

  /// Builds a vector from an integer seed with components in the range [min, max]. Each component has 8 bits of precision.
  static member fromSeed(seed : int, min : float32, max : float32) =
    let m = 0xff
    let Z = (max - min) / float32 m
    Vec4f(float32 ((seed >>> 24) &&& m) * Z + min, float32 ((seed >>> 16) &&& m) * Z + min, float32 ((seed >>> 8) &&& m) * Z + min, float32 (seed &&& m) * Z + min)

  /// Builds a vector in the unit cube from an integer seed. Each component has 10 bits of precision.
  static member fromSeed(seed : int) = Vec4f.fromSeed(seed, 0.0f, 1.0f)

  /// Builds a vector from an indexed function.
  static member inline create(f : int -> float32) = Vec4f(f 0, f 1, f 2, f 3)

  /// Returns the result of a component binary operation.
  static member inline bimap(v : Vec4f, u : Vec4f, f : float32 -> float32 -> float32) = Vec4f(f v.x u.x, f v.y u.y, f v.z u.z, f v.w u.w)

  /// Returns the result of a component ternary operation.
  static member inline trimap(v : Vec4f, u : Vec4f, w : Vec4f, f : float32 -> float32 -> float32 -> float32) = Vec4f(f v.x u.x w.x, f v.y u.y w.y, f v.z u.z w.z, f v.w u.w w.w)

  /// Clamps components of v between components of u0 and u1.
  static member inline clamp u0 u1 v = Vec4f.trimap(u0, u1, v, clamp)

  /// Linear interpolation of vectors.
  static member inline lerp u0 u1 x = lerp u0 u1 x

  /// Euclidean distance between v and u.
  static member inline distance(v : Vec4f, u : Vec4f) = (v - u).length

  /// Euclidean distance squared between v and u.
  static member inline distance2(v : Vec4f, u : Vec4f) = (v - u).length2

  /// Component minimum. Note that we cannot implement this as operator min
  /// as in F# that one is defined via the comparison operator.
  static member inline minimize (v : Vec4f) (u : Vec4f) = Vec4f.bimap(v, u, min)

  /// Component maximum. Note that we cannot implement this as operator max
  /// as in F# that one is defined via the comparison operator.
  static member inline maximize (v : Vec4f) (u : Vec4f) = Vec4f.bimap(v, u, max)

  /// Distance to line. The line is defined by an origin point and a unit length direction vector.
  member p.lineDistance(origin : Vec4f, direction : Vec4f) =
    let d = origin - p
    (d - (d *. direction) * direction).length

  /// Sine function with a period of unity. Uses a Taylor approximation.
  member v.sinr =
    let vpi12 = Vec4f(G pi12)
    let s = v - Vec4f(Q 1 4)
    let phi = (s - s.map(floor)) * G tau
    sinTaylor (abs (phi - vpi12 * 2G) - vpi12)

  /// Cosine function with a period of unity. Uses a Taylor approximation.
  member v.cosr =
    let vpi12 = Vec4f(G pi12)
    let phi = (v - v.map(floor)) * G tau
    sinTaylor (abs (phi - vpi12 * 2G) - vpi12)

  /// Sine function. Uses a Taylor approximation.
  member v.sin =
    let v' = v * Q 1 tau in v'.sinr

  /// Cosine function. Uses a Taylor approximation.
  member v.cos =
    let v' = v * Q 1 tau in v'.cosr

  override v.ToString() = "Vec4f(" + string v.x + ", " + string v.y + ", " + string v.z + ")"

end


[<AutoOpen>]
module Vec4fPatterns =

  /// Active pattern that deconstructs 3-vectors.
  let inline (|Vec4f|) (v : Vec4f) = (v.x, v.y, v.z, v.w)

