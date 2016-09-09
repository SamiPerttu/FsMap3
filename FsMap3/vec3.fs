// Double precision 3-vectors.
namespace Fuse

open System.Numerics

open Common


/// Double precision 3-vector structure with component arithmetic.
[<NoComparison>]
type Vec3 = struct

  val x : float
  val y : float
  val z : float

  /// Builds a vector with the given component values.
  new(x, y, z) = { x = x; y = y; z = z }

  /// Builds a vector where all components have the same value.
  new(c : float) = { x = c; y = c; z = c }

  /// Builds a vector from an indexed function.
  new(f : int -> float) = { x = f 0; y = f 1; z = f 2 }

  /// Builds a vector from a triple.
  new(triple : float * float * float) = let x, y, z = triple in { x = x; y = y; z = z }

  member inline v.reduce(f : float -> float -> float) = f (f v.x v.y) v.z

  member inline v.reduceWith(projection, f) = f (f (projection v.x) (projection v.y)) (projection v.z)

  /// L1-norm aka Manhattan norm.
  member inline v.norm1 = abs v.x + abs v.y + abs v.z

  member inline v.maxNorm = max3 (abs v.x) (abs v.y) (abs v.z)

  /// Returns the length squared of the vector.
  member inline v.length2 = v.x * v.x + v.y * v.y + v.z * v.z

  /// Returns the length of the vector.
  member inline v.length = sqrt v.length2

  /// Returns the sum of the components.
  member inline v.sum = v.x + v.y + v.z

  /// Returns the average of the components.
  member inline v.average = (v.x + v.y + v.z) * 0.33333333333333333333333

  /// Returns the smallest of the components.
  member inline v.minimum = min3 v.x v.y v.z

  /// Returns the largest of the components.
  member inline v.maximum = max3 v.x v.y v.z

  /// Converts the vector to single precision.
  member inline v.vec3f = Vec3f(float32 v.x, float32 v.y, float32 v.z)

  /// Returns the vector as an array.
  member v.toArray = [| v.x; v.y; v.z |]

  /// Returns the vector as a list.
  member v.toList = [ v.x; v.y; v.z ]

  /// Transforms the components of the vector with the given function.
  member inline v.map(f : float -> float) = Vec3(f v.x, f v.y, f v.z)

  /// Returns the vector normalized.
  member inline v.normalize =
    let len2 = v.length2
    if len2 > 0.0 then
      let Z = 1.0 / sqrt len2 in Vec3(v.x * Z, v.y * Z, v.z * Z)
    else v

  /// Returns a vector with components replaced with their signs.
  member inline v.sign = v.map(signum)

  /// Indexed component access.
  member v.at(i) = match i with | 0 -> v.x | 1 -> v.y | _ -> v.z

  /// Returns the vector with component i set to a.
  member v.set(i, a : float) = match i with | 0 -> Vec3(a, v.y, v.z) | 1 -> Vec3(v.x, a, v.z) | _ -> Vec3(v.x, v.y, a)

  /// Vector negation.
  static member inline ( ~- ) (v : Vec3) = Vec3(-v.x, -v.y, -v.z)
  /// Vector sum.
  static member inline ( + ) (v : Vec3, u : Vec3) = Vec3(v.x + u.x, v.y + u.y, v.z + u.z)
  /// Vector sum.
  static member inline ( + ) (v : Vec3, a : float) = Vec3(v.x + a, v.y + a, v.z + a)
  /// Vector sum.
  static member inline ( + ) (a : float, v : Vec3) = Vec3(a + v.x, a + v.y, a + v.z)
  /// Vector difference.
  static member inline ( - ) (v : Vec3, u : Vec3) = Vec3(v.x - u.x, v.y - u.y, v.z - u.z)
  /// Vector difference.
  static member inline ( - ) (v : Vec3, a : float) = Vec3(v.x - a, v.y - a, v.z - a)
  /// Vector difference.
  static member inline ( - ) (a : float, v : Vec3) = Vec3(a - v.x, a - v.y, a - v.z)
  /// Vector component division.
  static member inline ( / ) (v : Vec3, a : float) = let ia = 1.0 / a in Vec3(v.x * ia, v.y * ia, v.z * ia)
  /// Vector component division.
  static member inline ( / ) (v : Vec3, u : Vec3) = Vec3(v.x / u.x, v.y / u.y, v.z / u.z)
  /// Vector component division.
  static member inline ( / ) (a : float, v : Vec3) = Vec3(a / v.x, a / v.y, a / v.z)
  /// Vector component product.
  static member inline ( * ) (v : Vec3, a : float) = Vec3(v.x * a, v.y * a, v.z * a)
  /// Vector component product.
  static member inline ( * ) (a : float, v : Vec3) = Vec3(a * v.x, a * v.y, a * v.z)
  /// Vector component product.
  static member inline ( * ) (v : Vec3, u : Vec3) = Vec3(v.x * u.x, v.y * u.y, v.z * u.z)
  /// Dot product.
  static member inline Dot(v : Vec3, u : Vec3) = v.x * u.x + v.y * u.y + v.z * u.z
  /// Cross product.
  static member inline Cross(v : Vec3, u : Vec3) = 
    let x = v.y * u.z - v.z * u.y
    let y = v.z * u.x - v.x * u.z
    let z = v.x * u.y - v.y * u.x
    Vec3(x, y, z)

  static member inline Abs (v : Vec3) = v.map(abs)
  static member inline Ceiling (v : Vec3) = v.map(ceil)
  static member inline Cos (v : Vec3) = v.map(cos)
  static member inline Exp (v : Vec3) = v.map(exp)
  static member inline Floor (v : Vec3) = v.map(floor)
  static member inline Log (v : Vec3) = v.map(log)
  static member inline Pow (v : Vec3, b : float) = Vec3(v.x ** b, v.y ** b, v.z ** b)
  static member inline Pow (u : Vec3, v : Vec3) = Vec3(u.x ** v.x, u.y ** v.y, u.z ** v.z)
  static member inline Round (v : Vec3) = v.map(round)
  static member inline Sin (v : Vec3) = v.map(sin)
  static member inline Sqrt (v : Vec3) = v.map(sqrt)
  static member inline Tan (v : Vec3) = v.map(tan)
  static member inline Tanh (v : Vec3) = v.map(tanh)
  static member inline Truncate (v : Vec3) = v.map(truncate)

  static member Zero = Vec3(0.0)
  static member One = Vec3(1.0)
  static member zero = Vec3(0.0)
  static member one = Vec3(1.0)

  /// Builds a vector from an integer seed with components in the range [min, max]. Each component has 10 bits of precision.
  static member fromSeed(seed : int, min : float, max : float) =
    let m = 0x3ff
    let Z = (max - min) / float m
    Vec3(float ((seed >>> 20) &&& m) * Z + min, float ((seed >>> 10) &&& m) * Z + min, float (seed &&& m) * Z + min)

  /// Builds a vector in the unit cube from an integer seed. Each component has 10 bits of precision.
  static member fromSeed(seed : int) = Vec3.fromSeed(seed, 0.0, 1.0)

  /// Returns the result of a component binary operation.
  static member inline bimap(v : Vec3, u : Vec3, f : float -> float -> float) = Vec3(f v.x u.x, f v.y u.y, f v.z u.z)

  /// Component minimum. Note that we cannot implement this as operator min
  /// as that one is defined via the comparison operator.
  static member inline minimize (v : Vec3) (u : Vec3) = Vec3.bimap(v, u, min)

  /// Component maximum. Note that we cannot implement this as operator max
  /// as that one is defined via the comparison operator.
  static member inline maximize (v : Vec3) (u : Vec3) = Vec3.bimap(v, u, max)

  // Generic number literal support.
  static member inline genericNumber(x : int32, _ : Vec3) = Vec3(float x)
  static member inline genericNumber(x : int64, _ : Vec3) = Vec3(float x)
  static member inline genericNumber(x : string, _ : Vec3) = Vec3(float x)

  /// Distance to line. The line is defined by an origin point and a unit length direction vector.
  member p.lineDistance(origin : Vec3, direction : Vec3) =
    let d = origin - p
    (d - (d *. direction) * direction).length

  override v.ToString() = "Vec3(" + Pretty.string v.x + ", " + Pretty.string v.y + ", " + Pretty.string v.z + ")"

end



[<AutoOpen>]
module Vec3Extensions =

  type System.Numerics.Vector3 with
    /// Converts the vector to double precision.
    member v.vec3 = Vec3(float v.x, float v.y, float v.z)



[<AutoOpen>]
module Vec3Pattern =

  /// Active pattern that deconstructs 3-vectors.
  let inline (|Vec3|) (v : Vec3) = (v.x, v.y, v.z)

