/// Potential functions in 3-space.
module FsMap3.Potential

open Common


/// A potential can be interpreted as a distance. A unity potential is considered to define
/// the surface of a potential function. The surface should always fit inside the unit ball.
type Potential3 = Vec3f -> float32


(*
Notes.
-The Y axis is 'up'.
-For the supershapes, as any power parameter drops below 1, the maximum gradient magnitude starts to climb rapidly.
-The potentials have been adjusted to near unity radius, some more closely than others.
-The range of the potentials has been adjusted so that the minimum value is zero
 or a positive number very close to zero.
*)



/// Ball potential function. The radius is r (0 < r < 1).
let ball r (v : Vec3f) = v.length / r


/// Superellipsoid potential function. The norms r ("radial") and t ("translational") (r > 0, t > 0)
/// control the shape of the superellipsoid. For example, r = t = 2 is a ball. r = t = 1 is an octahedron.
/// r = t = 6 is a rounded cube.
let superellipsoid r t =
  assert (r > 0G && t > 0G)
  let inline formula (v : Vec3f) =
    let v = abs v
    (v.x ** r + v.z ** r) ** (t / r) + v.y ** t
  // If necessary, scale the superellipsoid to fit it inside the unit sphere.
  let radius =
    max4 1.0f
      (G sqrt3 * Fun.pinpointArg 0G 2G 1G (fun x -> formula (Vec3f(x))))
      (G sqrt2 * Fun.pinpointArg 0G 2G 1G (fun x -> formula (Vec3f(x, 0G, x))))
      (G sqrt2 * Fun.pinpointArg 0G 2G 1G (fun x -> formula (Vec3f(x, x, 0G))))
  fun (v : Vec3f) -> formula (v * radius)


/// Fast rounded cube potential function (although, not the classical one of 6th degree - this is an 8th degree one,
/// which is slightly more angular).
let roundedCube (v : Vec3f) =
  let inline power x = squared (squared (squared x))
  let inline root x = sqrt (sqrt x)
  let v = 1.51f * v
  root (power v.x + power v.y + power v.z)


/// Fast rounded cylinder potential function. 0 < radius <= 1.
let roundedCylinder radius (v : Vec3f) =
  assert (0G < radius && radius <= 1G)
  let v = Vec3f(1.047f / radius, 1.047f, 1.047f / radius) * v
  2.0f * squared v.x + 2.0f * squared v.z + squared (squared (squared v.y))


/// Rounded cone potential function. The rounded base has a radius of 0 < radius <= 1.
let roundedCone radius (v : Vec3f) =
  assert (0G < radius && radius <= 1G)
  let scale = 1.035f
  let v = Vec3f(scale / radius, scale, scale / radius) * v
  (sqrt (2.0f * squared v.x + 2.0f * squared v.z + squared (squared (squared (min v.y 0.0f))) + v.y * 0.5f + 0.5f) - 1.0f) * 1.829f + 1.0f


/// Torus potential function. Major axis is R (0 < R < 1). Minor axis is 1 - R.
/// The torus contains a hole when R > 0.5.
let torus R (v : Vec3f) =
  assert (0G < R && R < 1G)
  let r = 1.0f - R
  (squared(R - sqrt(squared v.x + squared v.z)) + squared v.y) / squared r


/// Supertorus potential function. Its cross sections are superellipses with power parameter p.
/// Relative length of the major axis of the supertorus is R (0 < R < 1); minor axis is 1 - R.
/// These are scaled, if necessary, to fit inside the unit sphere. The supertorus contains a hole when R > 0.5.
let supertorus p R =
  assert (p > 0G && 0G < R && R < 1G)
  // Compute an approximate bounding sphere for the supertorus.
  let scale = 1.02f * max 1.0f (let b = (1.0f - R) / exp2(1.0f / p) in sqrt(squared (R + b) + squared b))
  let r = 1.0f - R
  fun (v : Vec3f) ->
    let v = scale * v
    let a = R - sqrt(squared v.x + squared v.z)
    (abs a ** p + abs v.y ** p) ** (1.0f / p) / r


/// Teardrop potential function.
let teardrop (v : Vec3f) =
  let v = Vec3f(v.x, v.y + 1.0f, v.z) * 0.5f
  (sqrt (squared v.x + squared v.z + cubed v.y * (v.y - 1.0f) + 1.0f) - 1.0f) * 18.45f + 1.0f


/// Supercone potential function. Its cross sections are superellipses with power parameter p.
/// The rounded base has a radius of 0 < radius <= 1.
let supercone p radius (v : Vec3f) =
  assert (p > 0G && 0G < radius && radius < 1G)
  // Compute a rough bounding ball for the supercone.
  let scale = clamp 1.089f 1.33f (lerp 1.089f 1.127f (delerp 2.0f 2.5f p))
  let v = Vec3f(scale / radius * abs v.x, scale * v.y, scale / radius * abs v.z)
  (v.x ** p + v.z ** p + squared (squared (squared (min v.y 0.0f))) + 0.5f * v.y - 0.5f) * 1.2618f + 1.0f


/// Steiner's Roman surface.
let roman (v : Vec3f) =
  let v = v / G sqrt3
  (squared v.x * (squared v.y + squared v.z) + squared v.y * squared v.z - v.x * v.y * v.z) * 256.0f + 1.0f



/// Calculates the gradient of potential function f at v.
let gradient (f : Potential3) (v : Vec3f) =
  let epsilon = 1.0e-3f
  let v0 = f v
  let dx = f (v + Vec3f(epsilon, 0.0f, 0.0f)) - v0
  let dy = f (v + Vec3f(0.0f, epsilon, 0.0f)) - v0
  let dz = f (v + Vec3f(0.0f, 0.0f, epsilon)) - v0
  Vec3f(dx, dy, dz) / epsilon



/// Calculates the normal of potential function f at v. Returns the zero vector if the normal is not well defined.
let normal (f : Potential3) (v : Vec3f) =
  let epsilon = 1.0e-3f
  let v0 = f v
  let dx = f(v + Vec3f(epsilon, 0.0f, 0.0f)) - v0
  let dy = f(v + Vec3f(0.0f, epsilon, 0.0f)) - v0
  let dz = f(v + Vec3f(0.0f, 0.0f, epsilon)) - v0
  Vec3f(dx, dy, dz).normalize



/// Samples the volume of potential function f. The volume is returned as a fraction of the unit ball.
let sampleVolume samples (f : Potential3) =
  let rnd = Rnd(1)
  let mutable n = 0
  for __ = 1 to samples do
    let v = Common.doFind (fun _ -> rnd.vec3f(map01to11)) (fun v -> v.length <= 1.0f)
    if f v < 1.0f then n <- n + 1
  float32 n / float32 samples

