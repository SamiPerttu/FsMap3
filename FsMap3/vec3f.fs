// Single precision 3-vectors.
namespace Fuse

open System.Numerics

open Common


[<AutoOpen>]
module Vector3Extensions =

  // We shape the Vector.Numerics.Vector3 interface to our liking with class extensions.
  // These functions from System.Numerics.Vector3 are not yet wrapped: Transform, TransformNormal.
  // Unfortunately, since static type constraints do not take into account type extensions, we cannot implement
  // generic number support beyond the available 0G and 1G, and we cannot extend the set of available operators.
  type Vector3 with

    member inline v.x = v.X
    member inline v.y = v.Y
    member inline v.z = v.Z

    /// Length squared of the vector.
    member inline v.length2 = v.LengthSquared()

    /// Length of the vector.
    member inline v.length = v.Length()

    /// L1-norm aka Manhattan norm.
    member inline v.norm1 = abs v.x + abs v.y + abs v.z

    /// Maximum norm.
    member inline v.maxNorm = max3 (abs v.x) (abs v.y) (abs v.z)

    /// The sum of the components.
    member inline v.sum = v.x + v.y + v.z

    /// The average of the components.
    member inline v.average = (v.x + v.y + v.z) * 0.333333333333333f

    /// The smallest component.
    member inline v.minimum = min3 v.x v.y v.z

    /// The largest component.
    member inline v.maximum = max3 v.x v.y v.z

    /// Reflects the vector about the normal.
    member inline v.reflect(normal) = Vector3.Reflect(v, normal)

    /// The vector as an array.
    member v.toArray = [| v.x; v.y; v.z |]

    /// The vector as a list.
    member v.toList = [ v.x; v.y; v.z ]

    /// Transforms the components of the vector with the given function.
    member inline v.map(f : float32 -> float32) = Vector3(f v.x, f v.y, f v.z)

    /// 4-norm of the vector.
    member inline v.norm4 = v.map(fun x -> squared (squared x)).sum |> sqrt |> sqrt

    /// 8-norm of the vector.
    member inline v.norm8 = v.map(fun x -> squared (squared (squared x))).sum |> sqrt |> sqrt |> sqrt

    /// L-norm of the vector.
    member inline v.norm(L : float32) = (abs v.x ** L + abs v.y ** L + abs v.z ** L) ** (1.0f / L)

    /// Raises components to the given power.
    member inline v.power(a : float32) = Vector3(v.x ** a, v.y ** a, v.z ** a)

    /// Reduces the components of a vector with a binary operator.
    member inline v.reduce(op : float32 -> float32 -> float32) = op (op v.x v.y) v.z

    /// Reduces the projected components of a vector with a binary operator.
    member inline v.reduceWith(projection, op) = op (op (projection v.x) (projection v.y)) (projection v.z)

    /// The vector normalized.
    member inline v.normalize = Vector3.Normalize(v)

    /// Returns a vector with components replaced with their signs.
    member inline v.sign = v.map(signum)

    /// Returns whether all components of the vector fulfill the predicate.
    member inline v.forAll(f) = f v.x && f v.y && f v.z

    /// Returns whether all components of the vector are finite.
    member inline v.isFinite = v.forAll(isFinite)

    /// Indexed component access.
    member v.at(i) = match i with | 0 -> v.x | 1 -> v.y | _ -> v.z

    /// The vector with component i set to a.
    member v.set(i, a : float32) = match i with | 0 -> Vector3(a, v.y, v.z) | 1 -> Vector3(v.x, a, v.z) | _ -> Vector3(v.x, v.y, a)

    /// Sine map with a period of unity. Uses a Taylor approximation.
    member v.sinr =
      // Compute in parallel using SIMD.
      let vpi12 = Vector3(G pi12)
      let s = v - Vector3(0.25f)
      let phi = (s - s.map(floor)) * G tau
      sinTaylor (abs (phi - vpi12 * 2G) - vpi12)

    /// Cosine map with a period of unity. Uses a Taylor approximation.
    member v.cosr =
      let vpi12 = Vector3(G pi12)
      let phi = (v - v.map(floor)) * G tau
      sinTaylor (abs (phi - vpi12 * 2G) - vpi12)

    /// Sine function. Uses a Taylor approximation.
    member v.sin =
      let v' : Vector3 = v * Q 1 tau
      v'.sinr

    /// Cosine function. Uses a Taylor approximation.
    member v.cos =
      let v' : Vector3 = v * Q 1 tau
      v'.cosr

    /// The zero vector.
    static member inline zero = Vector3.Zero

    /// All ones vector.
    static member inline one = Vector3.One

    /// The unit X vector.
    static member inline unitX = Vector3.UnitX

    /// The unit Y vector.
    static member inline unitY = Vector3.UnitY

    /// The unit Z vector.
    static member inline unitZ = Vector3.UnitZ

    /// Clamps components of v between components of u0 and u1.
    static member inline clamp u0 u1 v = Vector3.Clamp(v, u0, u1)

    /// Linear interpolation of vectors.
    static member inline lerp u0 u1 x = Vector3.Lerp(u0, u1, x)

    /// Euclidean distance between v and u.
    static member inline distance(v, u) = Vector3.Distance(v, u)

    /// Euclidean distance squared between v and u.
    static member inline distance2(v, u) = Vector3.DistanceSquared(v, u)

    /// Builds a vector from an integer seed with components in the range [min, max]. Each component has 10 bits of precision.
    static member inline fromSeed(seed : int, min : float32, max : float32) =
      let m = 0x3ff
      // The constant is 1 / m.
      let Z = (max - min) * 0.0009775171065f
      Vector3((seed >>> 20) &&& m |> float32, (seed >>> 10) &&& m |> float32, seed &&& m |> float32) * Z + Vector3(min)

    /// Builds a vector in the unit cube from an integer seed. Each component has 10 bits of precision.
    static member inline fromSeed(seed : int) = Vector3.fromSeed(seed, 0.0f, 1.0f)

    /// Builds a vector from an indexed function.
    static member inline create(f : int -> float32) = Vector3(f 0, f 1, f 2)

    /// Returns the result of a component binary operation.
    static member inline bimap(v : Vector3, u : Vector3, f : float32 -> float32 -> float32) = Vector3(f v.x u.x, f v.y u.y, f v.z u.z)

    /// Component minimum. Note that we cannot implement this as operator min
    /// as in F# that one is defined via the comparison operator.
    static member inline minimize (v : Vector3) (u : Vector3) = Vector3.Min(v, u)

    /// Component maximum. Note that we cannot implement this as operator max
    /// as in F# that one is defined via the comparison operator.
    static member inline maximize (v : Vector3) (u : Vector3) = Vector3.Max(v, u)

    /// Distance to line. The line is defined by an origin point and a unit length direction vector.
    member p.lineDistance(origin : Vector3, direction : Vector3) =
      let d = origin - p
      (d - (d *. direction) * direction).length

  end



/// 32-bit floating point 3-vector structure. Our custom version - not SIMD accelerated.
[<NoComparison>]
type Vec3e = struct

  val x : float32
  val y : float32
  val z : float32

  new(x, y, z) = { x = x; y = y; z = z }
  new(c) = { x = c; y = c; z = c }

  /// The length squared of the vector.
  member inline v.length2 = v.x * v.x + v.y * v.y + v.z * v.z

  /// The length of the vector.
  member inline v.length = sqrt v.length2

  /// L1-norm aka Manhattan norm.
  member inline v.norm1 = abs v.x + abs v.y + abs v.z

  /// Maximum norm.
  member inline v.maxNorm = max3 (abs v.x) (abs v.y) (abs v.z)

  /// The L-norm of the vector.
  member inline v.norm(L : float32) = (abs v.x ** L + abs v.y ** L + abs v.z ** L) ** (1.0f / L)

  /// The average of the components.
  member inline v.average = (v.x + v.y + v.z) * Q 1 3

  /// The smallest of the components.
  member inline v.minimum = min3 v.x v.y v.z

  /// The largest of the components.
  member inline v.maximum = max3 v.x v.y v.z

  /// The vector as an array.
  member v.toArray = [| v.x; v.y; v.z |]

  /// The vector as a list.
  member v.toList = [ v.x; v.y; v.z ]

  /// The vector as a tuple.
  member inline v.tuple = (v.x, v.y, v.z)

  /// The sum of the components.
  member inline v.sum = v.x + v.y + v.z

  /// Transforms the components of the vector with a function.
  member inline v.map(f : float32 -> float32) = Vec3e(f v.x, f v.y, f v.z)

  /// Reduces the components of a vector with a binary operator.
  member inline v.reduce(op : float32 -> float32 -> float32) = op (op v.x v.y) v.z

  /// Reduces the projected components of a vector with a binary operator.
  member inline v.reduceWith(projection, op) = op (op (projection v.x) (projection v.y)) (projection v.z)

  /// Returns the vector normalized (or the zero vector).
  member v.normalize =
    let length = v.length2
    if length > 0.0f then
      let Z = 1.0f / sqrt length
      Vec3e(v.x * Z, v.y * Z, v.z * Z)
    else v

  /// The zero vector.
  static member inline zero = Vec3e(0.0f)

  /// The zero vector.
  static member inline Zero = Vec3e(0.0f)

  /// The one vector.
  static member inline one = Vec3e(1.0f)

  /// The one vector.
  static member inline One = Vec3e(1.0f)

  /// The unit X vector.
  static member inline unitX = Vec3e(1.0f, 0.0f, 0.0f)

  /// The unit Y vector.
  static member inline unitY = Vec3e(0.0f, 1.0f, 0.0f)

  /// The unit Z vector.
  static member inline unitZ = Vec3e(0.0f, 0.0f, 1.0f)

  static member inline ( ~- ) (a : Vec3e) = Vec3e(-a.x, -a.y, -a.z)
  static member inline ( + ) (a : Vec3e, b : Vec3e) = Vec3e(a.x + b.x, a.y + b.y, a.z + b.z)
  static member inline ( + ) (a : Vec3e, b : float32) = Vec3e(a.x + b, a.y + b, a.z + b)
  static member inline ( + ) (a : float32, b : Vec3e) = Vec3e(a + b.x, a + b.y, a + b.z)
  static member inline ( - ) (a : Vec3e, b : Vec3e) = Vec3e(a.x - b.x, a.y - b.y, a.z - b.z)
  static member inline ( - ) (a : Vec3e, b : float32) = Vec3e(a.x - b, a.y - b, a.z - b)
  static member inline ( - ) (a : float32, b : Vec3e) = Vec3e(a - b.x, a - b.y, a - b.z)
  static member inline ( / ) (a : Vec3e, b : Vec3e) = Vec3e(a.x / b.x, a.y / b.y, a.z / b.z)
  static member inline ( / ) (a : Vec3e, b : float32) = Vec3e(a.x / b, a.y / b, a.z / b)
  static member inline ( / ) (a : float32, b : Vec3e) = Vec3e(a / b.x, a / b.y, a / b.z)
  static member inline ( * ) (a : Vec3e, b : float32) = Vec3e(a.x * b, a.y * b, a.z * b)
  static member inline ( * ) (a : float32, b : Vec3e) = Vec3e(a * b.x, a * b.y, a * b.z)
  static member inline ( * ) (a : Vec3e, b : Vec3e) = Vec3e(a.x * b.x, a.y * b.y, a.z * b.z)
  /// Dot product.
  static member inline Dot(a : Vec3e, b : Vec3e) = a.x * b.x + a.y * b.y + a.z * b.z
  /// Cross product.
  static member inline Cross(u : Vec3e, v : Vec3e) = 
    let x = u.y * v.z - u.z * v.y
    let y = u.z * v.x - u.x * v.z
    let z = u.x * v.y - u.y * v.x
    Vec3e(x, y, z)

  static member inline Abs (v : Vec3e) = v.map(abs)
  static member inline Ceiling (v : Vec3e) = v.map(ceil)
  static member inline Cos (v : Vec3e) = v.map(cos)
  static member inline Exp (v : Vec3e) = v.map(exp)
  static member inline Floor (v : Vec3e) = v.map(floor)
  static member inline Log (v : Vec3e) = v.map(log)
  static member inline Pow (v : Vec3e, b : float32) = Vec3e(v.x ** b, v.y ** b, v.z ** b)
  static member inline Pow (u : Vec3e, v : Vec3e) = Vec3e(u.x ** v.x, u.y ** v.y, u.z ** v.z)
  static member inline Round (v : Vec3e) = v.map(round)
  static member inline Sin (v : Vec3e) = v.map(sin)
  static member inline Sqrt (v : Vec3e) = v.map(sqrt)
  static member inline Tan (v : Vec3e) = v.map(tan)
  static member inline Tanh (v : Vec3e) = v.map(tanh)
  static member inline Truncate (v : Vec3e) = v.map(truncate)

  /// Builds a vector from an integer seed with components in the range [min, max]. Each component has 10 bits of precision.
  static member fromSeed(seed : int, min : float32, max : float32) =
    let m = 0x3ff
    let Z = (max - min) / float32 m
    Vec3e(float32 ((seed >>> 20) &&& m) * Z + min, float32 ((seed >>> 10) &&& m) * Z + min, float32 (seed &&& m) * Z + min)

  /// Builds a vector in the unit cube from an integer seed. Each component has 10 bits of precision.
  static member fromSeed(seed : int) = Vec3e.fromSeed(seed, 0.0f, 1.0f)

  /// Builds a vector from an indexed function.
  static member inline create(f : int -> float32) = Vec3e(f 0, f 1, f 2)

  /// Returns the result of a component binary operation.
  static member inline bimap(v : Vec3e, u : Vec3e, f : float32 -> float32 -> float32) = Vec3e(f v.x u.x, f v.y u.y, f v.z u.z)

  /// Returns the result of a component ternary operation.
  static member inline trimap(v : Vec3e, u : Vec3e, w : Vec3e, f : float32 -> float32 -> float32 -> float32) = Vec3e(f v.x u.x w.x, f v.y u.y w.y, f v.z u.z w.z)

  /// Clamps components of v between components of u0 and u1.
  static member inline clamp u0 u1 v = Vec3e.trimap(u0, u1, v, clamp)

  /// Linear interpolation of vectors.
  static member inline lerp u0 u1 x = lerp u0 u1 x

  /// Euclidean distance between v and u.
  static member inline distance(v : Vec3e, u : Vec3e) = (v - u).length

  /// Euclidean distance squared between v and u.
  static member inline distance2(v : Vec3e, u : Vec3e) = (v - u).length2

  /// Component minimum. Note that we cannot implement this as operator min
  /// as in F# that one is defined via the comparison operator.
  static member inline minimize (v : Vec3e) (u : Vec3e) = Vec3e.bimap(v, u, min)

  /// Component maximum. Note that we cannot implement this as operator max
  /// as in F# that one is defined via the comparison operator.
  static member inline maximize (v : Vec3e) (u : Vec3e) = Vec3e.bimap(v, u, max)

  /// Distance to line. The line is defined by an origin point and a unit length direction vector.
  member p.lineDistance(origin : Vec3e, direction : Vec3e) =
    let d = origin - p
    (d - (d *. direction) * direction).length

  /// Sine map with a period of unity. Uses a Taylor approximation.
  member v.sinr =
    let vpi12 = Vec3e(G pi12)
    let s = v - Vec3e(0.25f)
    let phi = (s - s.map(floor)) * G tau
    sinTaylor (abs (phi - vpi12 * 2G) - vpi12)

  /// Cosine map with a period of unity. Uses a Taylor approximation.
  member v.cosr =
    let vpi12 = Vec3e(G pi12)
    let phi = (v - v.map(floor)) * G tau
    sinTaylor (abs (phi - vpi12 * 2G) - vpi12)

  /// Sine function. Uses a Taylor approximation.
  member v.sin =
    let v' = v * Q 1 tau in v'.sinr

  /// Cosine function. Uses a Taylor approximation.
  member v.cos =
    let v' = v * Q 1 tau in v'.cosr

  override v.ToString() = "Vec3e(" + Pretty.string v.x + ", " + Pretty.string v.y + ", " + Pretty.string v.z + ")"

end



/// Single precision floating point 3-vector. SIMD accelerated when possible.
type Vec3f = Vector3



[<AutoOpen>]
module Vec3fPattern =
  /// Active pattern that deconstructs 3-vectors.
  let inline (|Vec3f|) (v : Vec3f) = (v.x, v.y, v.z)




module Vec3fTest =

  let testSimd() =

    let n = 5000

    let rnd = createLcg 1
    let a1 = Array.init n (fun _ -> Vec3f.fromSeed(rnd.tick))
    let a2 = Array.init n (fun i -> Vec3e(a1.[i].x, a1.[i].y, a1.[i].z))
    let a3 = Array.init n (fun i -> Vector3(a1.[i].x, a1.[i].y, a1.[i].z))

    let t0 = Common.timeNow()
    let mutable x1d = 0.0f
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        x1d <- x1d + a1.[i] *. a1.[j]
        x1d <- x1d + a1.[i] *. a1.[i]
        x1d <- x1d + a1.[j] *. a1.[i]
        x1d <- x1d + a1.[j] *. a1.[j]
    let t1d = Common.timeNow() - t0

    let t0 = Common.timeNow()
    let mutable x1m = Vec3f.zero
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        x1m <- x1m + a1.[i] * a1.[j]
        x1m <- x1m + a1.[i] * a1.[i]
        x1m <- x1m + a1.[j] * a1.[i]
        x1m <- x1m + a1.[j] * a1.[j]
    let t1m = Common.timeNow() - t0

    let t0 = Common.timeNow()
    let mutable x2d = 0.0f
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        x2d <- x2d + a2.[i] *. a2.[j]
        x2d <- x2d + a2.[i] *. a2.[i]
        x2d <- x2d + a2.[j] *. a2.[i]
        x2d <- x2d + a2.[j] *. a2.[j]
    let t2d = Common.timeNow() - t0

    let t0 = Common.timeNow()
    let mutable x2m = Vec3e.zero
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        x2m <- x2m + a2.[i] * a2.[j]
        x2m <- x2m + a2.[i] * a2.[i]
        x2m <- x2m + a2.[j] * a2.[i]
        x2m <- x2m + a2.[j] * a2.[j]
    let t2m = Common.timeNow() - t0

    let t0 = Common.timeNow()
    let mutable x3d = 0.0f
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        x3d <- x3d + Vector3.Dot(a3.[i], a3.[j])
        x3d <- x3d + Vector3.Dot(a3.[i], a3.[i])
        x3d <- x3d + Vector3.Dot(a3.[j], a3.[i])
        x3d <- x3d + Vector3.Dot(a3.[j], a3.[j])
    let t3d = Common.timeNow() - t0

    let t0 = Common.timeNow()
    let mutable x3m = Vector3.Zero
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        x3m <- x3m + a3.[i] * a3.[j]
        x3m <- x3m + a3.[i] * a3.[i]
        x3m <- x3m + a3.[j] * a3.[i]
        x3m <- x3m + a3.[j] * a3.[j]
    let t3m = Common.timeNow() - t0

    printfn "Unaccelerated dot time : %f | value %f" t2d x2d
    printfn "Accelerated dot time   : %f | value %f" t1d x1d
    printfn "Raw dot time           : %f | value %f" t3d x3d

    printfn "Unaccelerated mul time : %f | value %A" t2m x2m
    printfn "Accelerated mul time   : %f | value %A" t1m x1m
    printfn "Raw mul time           : %f | value %A" t3m x3m
  
