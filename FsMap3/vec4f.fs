// Single precision 4-vectors.
namespace Fuse

open System.Numerics

open Common


/// Single precision floating point 4-vector. SIMD accelerated when possible.
type Vec4f = Vector4



[<AutoOpen>]
module Vec4fExtensions =

  // We shape the Vector.Numerics.Vector4 interface to our liking with class extensions.
  // These functions from System.Numerics.Vector4 are not yet wrapped: Transform.
  type Vector4 with

    member inline v.x = v.X
    member inline v.y = v.Y
    member inline v.z = v.Z
    member inline v.w = v.W

    /// Length squared of the vector.
    member inline v.length2 = v.LengthSquared()

    /// Length of the vector.
    member inline v.length = v.Length()

    /// Returns the XYZ components as a Vec3f.
    member inline v.xyz = Vec3f(v.x, v.y, v.z)

    /// L1-norm aka Manhattan norm.
    member inline v.norm1 = abs v.x + abs v.y + abs v.z + abs v.w

    /// Max-norm.
    member inline v.maxNorm = max4 (abs v.x) (abs v.y) (abs v.z) (abs v.w)

    /// The sum of the components.
    member inline v.sum = v.x + v.y + v.z + v.w

    /// The average of the components.
    member inline v.average = (v.x + v.y + v.z + v.w) * 0.25f

    /// The smallest of the components.
    member inline v.minimum = min4 v.x v.y v.z v.w

    /// The largest of the components.
    member inline v.maximum = max4 v.x v.y v.z v.w

    /// The vector as an array.
    member v.toArray = [| v.x; v.y; v.z; v.w |]

    /// The vector as a list.
    member v.toList = [ v.x; v.y; v.z; v.w ]

    /// Transforms the components of the vector with the given function.
    member inline v.map(f : float32 -> float32) = Vec4f(f v.x, f v.y, f v.z, f v.w)

    /// Reduces the components of a vector with a binary operator.
    member inline v.reduce(op : float32 -> float32 -> float32) = op (op (op v.x v.y) v.z) v.w

    /// Reduces the projected components of a vector with a binary operator.
    member inline v.reduceWith(projection, op) = op (op (op (projection v.x) (projection v.y)) (projection v.z)) (projection v.w)

    /// The vector normalized.
    member inline v.normalize = Vector4.Normalize(v)

    /// Returns a vector with components replaced with their signs.
    member inline v.sign = v.map(signum)

    /// Indexed component access.
    member v.at(i) = match i with | 0 -> v.x | 1 -> v.y | _ -> v.z

    /// Returns the vector with component i set to a.
    member v.set(i, a : float32) = match i with | 0 -> Vec4f(a, v.y, v.z, v.w) | 1 -> Vec4f(v.x, a, v.z, v.w) | 2 -> Vec4f(v.x, v.y, a, v.w) | _ -> Vec4f(v.x, v.y, v.z, a)

    /// The zero vector.
    static member inline zero = Vector4.Zero

    /// All ones vector.
    static member inline one = Vector4.One

    /// The unit X vector.
    static member inline unitX = Vector4.UnitX

    /// The unit Y vector.
    static member inline unitY = Vector4.UnitY

    /// The unit Z vector.
    static member inline unitZ = Vector4.UnitZ

    /// The unit W vector.
    static member inline unitW = Vector4.UnitW

    /// Euclidean distance between v and u.
    static member inline distance(v, u) = Vector4.Distance(v, u)

    /// Euclidean distance squared between v and u.
    static member inline distance2(v, u) = Vector4.DistanceSquared(v, u)

    /// Builds a vector from an integer seed with components in the range [min, max]. Each component has 8 bits of precision.
    static member fromSeed(seed : int, min : float32, max : float32) =
      let m = 0xff
      let Z = (max - min) / float32 m
      Vec4f(float32 ((seed >>> 24) &&& m) * Z + min, float32 ((seed >>> 16) &&& m) * Z + min, float32 ((seed >>> 8) &&& m) * Z + min, float32 (seed &&& m) * Z + min)

    /// Builds a vector in the unit cube from an integer seed. Each component has 10 bits of precision.
    static member inline fromSeed(seed : int) = Vec4f.fromSeed(seed, 0.0f, 1.0f)

    /// Builds a vector from an indexed function.
    static member inline create(f : int -> float32) = Vec4f(f 0, f 1, f 2, f 3)

    /// Returns the result of a component binary operation.
    static member inline bimap(v : Vec4f, u : Vec4f, f : float32 -> float32 -> float32) = Vec4f(f v.x u.x, f v.y u.y, f v.z u.z, f v.w u.w)

    /// Linear interpolation of vectors.
    static member inline lerp u0 u1 x = Vector4.Lerp(u0, u1, x)

    /// Component minimum. Note that we cannot implement this as operator min
    /// as in F# that one is defined via the comparison operator.
    static member inline minimize (v : Vec4f) (u : Vec4f) = Vector4.Min(v, u)

    /// Component maximum. Note that we cannot implement this as operator max
    /// as in F# that one is defined via the comparison operator.
    static member inline maximize (v : Vec4f) (u : Vec4f) = Vector4.Max(v, u)

    /// Clamps components of v between components of u0 and u1.
    static member inline clamp u0 u1 v = Vector4.Clamp(v, u0, u1)

    // Generic number literal support.
    static member inline genericNumber(x : int32, _ : Vec4f) = Vec4f(float32 x)
    static member inline genericNumber(x : int64, _ : Vec4f) = Vec4f(float32 x)
    static member inline genericNumber(x : string, _ : Vec4f) = Vec4f(float32 x)

  end



  /// Active pattern that deconstructs 4-vectors.
  let inline (|Vec4f|) (v : Vec4f) = (v.x, v.y, v.z, v.w)
