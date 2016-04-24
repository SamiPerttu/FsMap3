/// A simple ray tracer.
module FsMap3.Ray

open Common
open Map3


[<NoComparison; NoEquality>]
type DiffuseModel =
  | Lambert
  | OrenNayar of roughness : Map3



[<NoComparison; NoEquality>]
type SpecularModel =
  | Phong of shininess : Map3



[<NoEquality; NoComparison>]
type Material =
  {
    ambient : Map3
    diffuseModel : DiffuseModel
    diffuse : Map3
    specularModel : SpecularModel
    specular : Map3
    bump : Map3
  }

  static member create(ambient, diffuseModel, diffuse, specularModel, specular, bump) =
    { Material.ambient = ambient; diffuseModel = diffuseModel; diffuse = diffuse; specularModel = specularModel; specular = specular; bump = bump }



[<NoEquality; NoComparison>]
type Ray =
  {
    /// Ray origin.
    mutable origin : Vec3
    /// Normalized direction vector.
    mutable direction : Vec3
  }
  static member create() = { Ray.origin = Vec3.zero; direction = Vec3.zero }
  static member create(origin) = { Ray.origin = origin; direction = Vec3.zero }
  static member create(origin, direction) = { Ray.origin = origin; direction = direction }



[<NoEquality; NoComparison>]
type Intersection = struct
  /// Distance to intersection from ray origin.
  val distance : float
  /// Point of intersection.
  val position : Vec3
  /// The normal of the intersection. This is always a geometric normal without any smoothing or normal mapping.
  val normal : Vec3

  new(distance, position, normal) = { distance = distance; position = position; normal = normal }

  static member empty = Intersection(infinity, Vec3.zero, Vec3.zero)
end



/// An intersection interval. At the moment, used for intersection of polyhedra.
[<NoEquality; NoComparison>]
type Interval =
  {
    mutable minD : float
    mutable maxD : float
    mutable minN : Vec3
    mutable maxN : Vec3
  }

  member inline this.reset() =
    this.minD <- -infinity
    this.maxD <- infinity

  member inline this.isEmpty = this.minD >= this.maxD

  /// Intersects an X plane interval minX < x < maxX.
  member this.intersectX(ray : Ray, minX, maxX) =
    if ray.direction.x > 0.0 then
      let d0 = (minX - ray.origin.x) / ray.direction.x
      if d0 > this.minD then
        this.minD <- d0
        this.minN <- Vec3(-1.0, 0.0, 0.0)
      let d1 = (maxX - ray.origin.x) / ray.direction.x
      if d1 < this.maxD then
        this.maxD <- d1
        this.maxN <- Vec3(-1.0, 0.0, 0.0)
    elif ray.direction.x < 0.0 then
      let d0 = (maxX - ray.origin.x) / ray.direction.x
      if d0 > this.minD then
        this.minD <- d0
        this.minN <- Vec3(1.0, 0.0, 0.0)
      let d1 = (minX - ray.origin.x) / ray.direction.x
      if d1 < this.maxD then
        this.maxD <- d1
        this.maxN <- Vec3(1.0, 0.0, 0.0)
    else
      if ray.origin.x < minX || ray.origin.x > maxX then this.minD <- infinity

  /// Intersects a Y plane interval minY < y < maxY.
  member this.intersectY(ray : Ray, minY, maxY) =
    if ray.direction.y > 0.0 then
      let d = (minY - ray.origin.y) / ray.direction.y
      if d > this.minD then
        this.minD <- d
        this.minN <- Vec3(0.0, -1.0, 0.0)
      let d = (maxY - ray.origin.y) / ray.direction.y
      if d < this.maxD then
        this.maxD <- d
        this.maxN <- Vec3(0.0, -1.0, 0.0)
    elif ray.direction.y < 0.0 then
      let d = (maxY - ray.origin.y) / ray.direction.y
      if d > this.minD then
        this.minD <- d
        this.minN <- Vec3(0.0, 1.0, 0.0)
      let d = (minY - ray.origin.y) / ray.direction.y
      if d < this.maxD then
        this.maxD <- d
        this.maxN <- Vec3(0.0, 1.0, 0.0)
    else
      if ray.origin.y < minY || ray.origin.y > maxY then this.minD <- infinity

  /// Intersects a Z plane interval minZ < z < maxZ.
  member this.intersectZ(ray : Ray, minZ, maxZ) =
    if ray.direction.z > 0.0 then
      let d = (minZ - ray.origin.z) / ray.direction.z
      if d > this.minD then
        this.minD <- d
        this.minN <- Vec3(0.0, 0.0, -1.0)
      let d = (maxZ - ray.origin.z) / ray.direction.z
      if d < this.maxD then
        this.maxD <- d
        this.maxN <- Vec3(0.0, 0.0, -1.0)
    elif ray.direction.z < 0.0 then
      let d = (maxZ - ray.origin.z) / ray.direction.z
      if d > this.minD then
        this.minD <- d
        this.minN <- Vec3(0.0, 0.0, 1.0)
      let d = (minZ - ray.origin.z) / ray.direction.z
      if d < this.maxD then
        this.maxD <- d
        this.maxN <- Vec3(0.0, 0.0, 1.0)
    else
      if ray.origin.z < minZ || ray.origin.z > maxZ then this.minD <- infinity

  static member create() = { minD = -infinity; maxD = infinity; minN = Vec3.zero; maxN = Vec3.zero }


let threadLocalInterval = new System.Threading.ThreadLocal<_>(Interval.create)



type IPrimitive =
  abstract intersect : Ray -> Intersection Optionval
  abstract logicalNormal : Intersection -> Vec3



/// Axis-aligned box.
type Box(center : Vec3, radii : Vec3) =
  let minimum = center - radii
  let maximum = center + radii

  /// If enabled, uses normals from Implicit.RoundedCube.
  member val smoothNormals = false with get, set

  interface IPrimitive with

    member this.intersect(ray) =
      let i = !threadLocalInterval
      i.reset()
      i.intersectX(ray, minimum.x, maximum.x)
      i.intersectY(ray, minimum.y, maximum.y)
      i.intersectZ(ray, minimum.z, maximum.z)
      if i.isEmpty || i.maxD < 0.0 then
        Noneval
      elif i.minD > 0.0 then
        let point = ray.origin + i.minD * ray.direction
        Someval(Intersection(i.minD, point, i.minN))
      else
        let point = ray.origin + i.maxD * ray.direction
        Someval(Intersection(i.maxD, point, i.maxN))

    member this.logicalNormal(intersection) =
      if this.smoothNormals then
        // The scale of the transform from the box to the implicit cube does not really matter,
        // but we stay inside the unit sphere to adhere to the definition.
        let v = ((intersection.position - center) / (radii * sqrt3)).vec3f
        (Potential.normal Potential.roundedCube v).vec3
      else
        intersection.normal



/// Sphere with center and radius.
type Sphere(center, radius) =
  interface IPrimitive with

    member this.intersect(ray : Ray) =
      let OC = ray.origin - center
      let B = OC *. ray.direction
      let D = B * B + radius * radius - OC.length2
      if D < 0.0 then
        Noneval
      else 
        let distance = -B - sqrt(D)
        if distance > 0.0 then
          let p = ray.origin + distance * ray.direction
          Someval(Intersection(distance, p, (p - center).normalize))
        else Noneval

    member this.logicalNormal(intersection) = intersection.normal



/// Cylinder on Y axis with center, height and radius.
type Cylinder(center : Vec3, height : float, radius : float) =
  let baseY = -height * 0.5
  let apexY = height * 0.5

  /// If enabled, uses normals from Implicit.roundedCylinder.
  member val smoothNormals = false with get, set

  interface IPrimitive with

    member this.intersect(ray : Ray) =
      let origin = ray.origin - center
      let mutable maxD = infinity
      let mutable minD = -infinity
      if ray.direction.y > 0.0 then
        maxD <- (apexY - origin.y) / ray.direction.y
        minD <- (baseY - origin.y) / ray.direction.y
      elif ray.direction.y < 0.0 then
        maxD <- (baseY - origin.y) / ray.direction.y
        minD <- (apexY - origin.y) / ray.direction.y
      let a = 1.0 - ray.direction.y * ray.direction.y
      let c = squared origin.x + squared origin.z - squared radius
      if a > 0.0 then
        let b = origin.x * ray.direction.x + origin.z * ray.direction.z
        let mutable d = b * b - a * c
        if d > 0.0 then
          d <- sqrt(d)
          let minC = (-d - b) / a
          let maxC = (d - b) / a
          minD <- max minC minD
          maxD <- min maxC maxD
          if maxD <= minD || maxD < 0.0 then
            Noneval
          elif minD > 0.0 then
            let point = origin + minD * ray.direction
            let normal =
              if minC = minD then Vec3(point.x, 0.0, point.z).normalize else Vec3(0.0, -signum ray.direction.y, 0.0)
            Someval(Intersection(minD, point + center, normal))
          else
            let point = origin + maxD * ray.direction
            let normal =
              if maxC = maxD then Vec3(-point.x, 0.0, -point.z).normalize else Vec3(0.0, -signum ray.direction.y, 0.0)
            Someval(Intersection(maxD, point + center, normal))
        else
          Noneval
      else
        Noneval

    member this.logicalNormal(intersection) =
      if this.smoothNormals then
        let v = ((intersection.position - center) / Vec3(radius, height * 0.5, radius)).vec3f
        (Potential.normal (Potential.roundedCylinder 1.0f) v).vec3
      else intersection.normal



[<NoComparison; NoEquality>]
type Camera =
  {
    position : Vec3
    forward : Vec3
    right : Vec3
    up : Vec3
  }

  static member create(position, forward, right, up) =
    { Camera.position = position; forward = forward; right = right; up = up }

  member this.initialize(ray : Ray, x, y) =
    ray.origin <- this.position
    ray.direction <- (this.forward + (x - 0.5) * this.right - (y - 0.5) * this.up).normalize

  member this.initialize(ray : Ray, width : int, height : int, x : int, y : int, i) =
    let samplePosition = Poisson.poissonSquare.[i]
    this.initialize(ray, (float x + float samplePosition.x) / float width, (float y + float samplePosition.y) / float height)



[<NoComparison; NoEquality>]
type Traceable =
  {
    primitive : IPrimitive
    material : Material
  }

  static member create(primitive : IPrimitive, material) = { Traceable.primitive = primitive; material = material }



[<NoComparison; NoEquality>]
type Scene =
  {
    camera : Camera
    traceableArray : Traceable[]
    light : Vec3

    L : Vec3
  }

  static member create(camera, traceableArray, light) =
    {
      Scene.camera = camera
      traceableArray = traceableArray
      light = light
      L = light.normalize
    }

  member this.trace(ray : Ray) =

    let mutable closestIntersection = Fallbackval(Intersection.empty)
    let mutable closestTraceable = Noneval

    for i = 0 to this.traceableArray.last do
      let primitive = this.traceableArray.[i].primitive
      match primitive.intersect(ray) with
      | Someval(intersection) ->
        if intersection.distance < (!closestIntersection).distance then
          closestIntersection <- Someval(intersection)
          closestTraceable <- Someval(this.traceableArray.[i])
      | Noneval -> ()

    if closestIntersection.isSome then
      let hit = !closestIntersection
      let material = (!closestTraceable).material
      let P = hit.position
      let logicalN = (!closestTraceable).primitive.logicalNormal(hit)

      let V = (this.camera.position - P).normalize
      let N = (logicalN + (material.bump P.vec3f).vec3).normalize
      let H = (this.L + V).normalize
      let NdotL = N *. this.L

      let ambientTerm = (material.ambient P.vec3f).vec3

      let diffuse = (material.diffuse P.vec3f).vec3

      let diffuseTerm =
        match material.diffuseModel with
        | Lambert ->
          diffuse * (max NdotL 0.0)
        | OrenNayar(roughness) ->
          // The model implemented here is an improved version of the simple Oren-Nayar model by Yasuhiro Fujii.
          // The range of the roughness parameter is [0, 1], with zero roughness yielding a Lambertian reflector.
          if NdotL > 0.0 then
            let NdotV = N *. V
            let sigma = float (roughness P.vec3f).x
            let S = this.L *. V - NdotL * NdotV
            let T = if S > 0.0 then max NdotL NdotV else 1.0
            let A = 1.0 / (1.0 + (0.5 - tau / 3.0) * sigma)
            let B = sigma * A
            diffuse * NdotL * (A + B * S / T)
          else
            Vec3.zero

      let specularTerm =
        match material.specularModel with
        | Phong(shininess) ->
          if NdotL > 0.0 then
            let specularAmount = (material.specular P.vec3f).vec3
            let shininessAmount = float (shininess P.vec3f).x
            Vec3(specularAmount.average) * (max 0.0 (N *. H)) ** shininessAmount
          else
            Vec3.zero

      let color = ambientTerm + diffuseTerm + specularTerm
      Pair(closestTraceable, color)

    else
      Pair(Noneval, Vec3.zero)



[<NoComparison; NoEquality>]
type RayBuffer =
  {
    pixmap : Pixmap
    traceableMap : Traceable Optionval Grid
  }



let renderLine (buffer : RayBuffer) (scene : Scene) (background : Map3) y =

  let pixmap = buffer.pixmap
  let traceableMap = buffer.traceableMap
  let ray = Ray.create()

  let fy = 1.0f - float32 (y + 1) / float32 pixmap.sizeY

  for x = 0 to pixmap.lastX do
    scene.camera.initialize(ray, pixmap.width, pixmap.height, x, y, 0)
    let (Pair(traceable, color)) = scene.trace(ray)
    if traceable.isSome then
      pixmap.set(x, y, color.vec3f)
    else
      pixmap.set(x, y, background <| Vec3f(float32 x / float32 pixmap.sizeX, fy, 0.0f))
    traceableMap.[x, y] <- traceable



let refineLine (buffer : RayBuffer) (scene : Scene) (background : Map3) y =

  let pixmap = buffer.pixmap
  let traceableMap = buffer.traceableMap
  let ray = Ray.create()

  let fy = 1.0f - float32 (y + 1) / float32 pixmap.sizeY

  for x = 0 to pixmap.lastX do
    let traceable = traceableMap.[x, y]
    let mutable refine = false
    for sx = max 0 (x - 1) to min pixmap.lastX (x + 1) do
      for sy = max 0 (y - 1) to min pixmap.lastY (y + 1) do
        if x <> sx && y <> sy && !traceable <>= !traceableMap.[sx, sy] then
          refine <- true
    if refine then
      let mutable acolor = Vec3f.zero
      let n = 9
      for i = 1 to n do
        scene.camera.initialize(ray, pixmap.width, pixmap.height, x, y, i)
        let (Pair(traceable, color)) = scene.trace(ray)
        if traceable.isSome then
          acolor <- acolor + color.vec3f
        else
          acolor <- acolor + background (Vec3f(float32 x / float32 pixmap.sizeX, fy, 0.0f))
      pixmap.set(x, y, (pixmap.at(x, y) + acolor) / float32 (n + 1))



