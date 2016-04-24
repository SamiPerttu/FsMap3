/// The Worley cellular basis.
module FsMap3.Worley

open Common
open Mangle
open Basis3
open FeatureCount
open CellColor


// Ignoring signedness, there are 13 basic Worley cell distance (d0, d1, d2) weight patterns:
let worleyPattern =
  [|
    Vec3f(1.0f, 0.0f, 0.0f)   // 0  cells
    Vec3f(1.0f, 1.0f, 0.0f)   // 1  cells with some subcells
    Vec3f(1.0f, 1.0f, 1.0f)   // 2  cells with more subcells
    Vec3f(-1.0f, 1.0f, 0.0f)  // 3  Voronoi
    Vec3f(-1.0f, 1.0f, 1.0f)  // 4  Voronoi + added diamond-like detail
    Vec3f(0.0f, -1.0f, 1.0f)  // 5  more detailed Voronoi with somewhat web-like appearance
    Vec3f(-1.0f, -1.0f, 1.0f) // 6  most detailed Voronoi (this is the only pattern not guaranteed to be positive)
    Vec3f(1.0f, -1.0f, 1.0f)  // 7  crystal pattern
    Vec3f(1.0f, 0.0f, 1.0f)   // 8  less sharp crystal pattern
    Vec3f(0.0f, 1.0f, 1.0f)   // 9  least sharp crystal pattern
    Vec3f(0.0f, 1.0f, 0.0f)   // 10 crystals with plateaus
    Vec3f(0.0f, 0.0f, 1.0f)   // 11 web-like polygons
    Vec3f(-1.0f, 0.0f, 1.0f)  // 12 weird web-like pattern
  |]



/// A cell distance computes distances between points.
type CellDistance =
  /// Calculates an unnormalized distance from a non-negative coordinate difference.
  abstract scalar : float32 -> float32
  /// Calculates an unnormalized distance from a vector.
  abstract vector : Vec3f -> float32
  /// Normalizes an unnormalized distance. Normalization should make scalar distances linear
  /// and, if possible, make the closest distance average 0.5 with the unity cell count.
  abstract normalize : float32 -> float32
  /// The normalization factor applied to distances after linearization. It must hold that
  /// normalize (scalar d) = normalizationFactor * d.
  abstract normalizationFactor : float32

let cellNorm1 =
  // The normalization factor has been estimated via Monte Carlo for the norms with known exponents.
  let Z = 0.654f
  { new CellDistance with
    member __.scalar(x) = x
    member __.vector(v) = v.absNorm
    member __.normalize(d) = Z * d
    member __.normalizationFactor = Z
  }

let cellNorm2 =
  let Z = 0.961f
  { new CellDistance with
    member __.scalar(x) = squared x
    member __.vector(v) = v.length2
    member __.normalize(d) = Z * sqrt d
    member __.normalizationFactor = Z
  }

let cellNorm3 =
  let Z = 1.064f
  { new CellDistance with
    member __.scalar(x) = cubed x
    member __.vector(v) = v.reduceWith(abs >> cubed, (+))
    member __.normalize(d) = Z * d ** Q 1 3
    member __.normalizationFactor = Z
  }

let cellNorm4 =
  let Z = 1.110f
  { new CellDistance with
    member __.scalar(x) = squared (squared x)
    member __.vector(v) = v.reduceWith(squared >> squared, (+))
    member __.normalize(d) = Z * sqrt (sqrt d)
    member __.normalizationFactor = Z
  }

let cellNorm6 =
  let Z = 1.149f
  { new CellDistance with
    member __.scalar(x) = squared (cubed x)
    member __.vector(v) = v.reduceWith(squared >> cubed, (+))
    member __.normalize(d) = Z * d ** Q 1 6
    member __.normalizationFactor = Z
  }

let cellNorm8 =
  let Z = 1.165f
  { new CellDistance with
    member __.scalar(x) = squared (squared (squared x))
    member __.vector(v) = v.reduceWith(squared >> squared >> squared, (+))
    member __.normalize(d) = Z * sqrt (sqrt (sqrt d))
    member __.normalizationFactor = Z
  }

let cellNormMax =
  let Z = 1.188f
  { new CellDistance with
    member __.scalar(x) = x
    member __.vector(v) = v.maxNorm
    member __.normalize(d) = Z * d
    member __.normalizationFactor = Z
  }

let cellNorm L =
  // Pick a reasonable default for the normalization factor.
  let Z = 0.8f
  { new CellDistance with
    member __.scalar(x) = x ** L
    member __.vector(v) = v.norm(L)
    member __.normalize(d) = Z * d ** (1.0f / L)
    member __.normalizationFactor = Z
  }



/// Generic Worley cellular basis function.
let worleyBasis (cell : BasisData) (count : FeatureCount) maxDistance (distance : CellDistance) (v : Vec3f) =

  let scanCell jx jy jz =
    let mutable h = cell.hash(jx, jy, jz)
    for __ = 1 to count h do
      h <- mangle32 h
      let P = cell.d - Vec3f.fromSeed(h) - Vec3f(float32 jx, float32 jy, float32 jz)
      let d = distance.vector(P)
      if d < cell.d0 then
        cell.d2 <- cell.d1; cell.h2 <- cell.h1
        cell.d1 <- cell.d0; cell.h1 <- cell.h0
        cell.d0 <- d; cell.h0 <- h
      elif d < cell.d1 then
        cell.d2 <- cell.d1; cell.h2 <- cell.h1
        cell.d1 <- d; cell.h1 <- h
      elif d < cell.d2 then
        cell.d2 <- d; cell.h2 <- h
        
  let xN, xd0 = if cell.d.x < 0.5f then 0, cell.d.x else 1, 1.0f - cell.d.x
  let yN, yd0 = if cell.d.y < 0.5f then 0, cell.d.y else 1, 1.0f - cell.d.y
  let zN, zd0 = if cell.d.z < 0.5f then 0, cell.d.z else 1, 1.0f - cell.d.z

  // The initial search box contains 8 cells.
  cell.x1 <- xN
  cell.x0 <- xN - 1
  cell.y1 <- yN
  cell.y0 <- yN - 1
  cell.z1 <- zN
  cell.z0 <- zN - 1

  let expand axis far =
    if axis = 0 then
      let x' = if xN ^^^ far = 0 then cell.x0 <- cell.x0 - 1; cell.x0 else cell.x1 <- cell.x1 + 1; cell.x1
      for y = cell.y0 to cell.y1 do
        for z = cell.z0 to cell.z1 do scanCell x' y z
    elif axis = 1 then
      let y' = if yN ^^^ far = 0 then cell.y0 <- cell.y0 - 1; cell.y0 else cell.y1 <- cell.y1 + 1; cell.y1
      for x = cell.x0 to cell.x1 do
        for z = cell.z0 to cell.z1 do scanCell x y' z
    else
      let z' = if zN ^^^ far = 0 then cell.z0 <- cell.z0 - 1; cell.z0 else cell.z1 <- cell.z1 + 1; cell.z1
      for x = cell.x0 to cell.x1 do
        for y = cell.y0 to cell.y1 do scanCell x y z'

  // Our search neighborhood is an axis-aligned box. The nearest face of the box is always expanded next.
  // The expansion sequence is fixed, given face distance ordering.
  cell.storeAxisOrder(xd0, yd0, zd0)

  // Initialize algorithm state and compute the first 8 cells.
  cell.d0 <- maxDistance
  cell.d1 <- maxDistance
  cell.d2 <- maxDistance
  cell.h0 <- 0
  cell.h1 <- 0
  cell.h2 <- 0
  for x = cell.x0 to cell.x1 do
    for y = cell.y0 to cell.y1 do
      for z = cell.z0 to cell.z1 do scanCell x y z

  // We can break early if the current maximum distance is lower than the distance of the next expansion.
  let inline earlyBreak expansionDistance =
    cell.d2 < distance.scalar(expansionDistance)

  // The initial search state corresponds to expansion of the nearest far face, from 8 cells to 12 cells.
  let mutable faceSign = -1
  let mutable faceIndex = 2
  let mutable faceDistance = 1.0f
  let mutable ready = false

  // Keep expanding until the first 3 distances are known with certainty.
  while not ready do
    if earlyBreak (faceDistance + float32 faceSign * cell.search.[faceIndex].fst) then
      ready <- true
    else
      expand cell.search.[faceIndex].snd (negativei faceSign)
      match faceSign, faceIndex with
      | (-1, 0) ->
        // Switch from far face expansion to near face expansion.
        faceSign <- 1
      | (1, 2) ->
        // Switch from near face expansion to far face expansion.
        faceSign <- -1
        faceDistance <- faceDistance + 1.0f
      | _ ->
        faceIndex <- faceIndex + faceSign

  cell.d0 <- distance.normalize cell.d0
  cell.d1 <- distance.normalize cell.d1
  cell.d2 <- distance.normalize cell.d2


/// Worley basis. Components contain Worley patterns p0, p1 and p2.
let worley (layout : LayoutFunction) (count : FeatureCount) p0 p1 p2 (distance : CellDistance) (fade : float32 -> float32) (frequency : float32) (v : Vec3f) =
  let data = layout frequency v
  worleyBasis data count (1.0f / distance.normalizationFactor) distance v
  let d = Vec3f(data.d0, data.d1, data.d2).map(min 1.0f >> fade)
  data.release()
  Vec3f(d *. worleyPattern.[p0], d *. worleyPattern.[p1], d *. worleyPattern.[p2])


/// Default Worley basis with the default layout function and unity Poisson feature distribution.
let inline worleyd p distance fade frequency v = worley hifiLayout unityPoisson p ((p + 1) % worleyPattern.size) ((p + 2) % worleyPattern.size) distance fade frequency v



/// Generic colored Worley cellular basis function.
let worleyColorBasis (cell : BasisData) (count : FeatureCount) maxDistance (distance : CellDistance) (color : CellColor) (colorSharpness : float32) (v : Vec3f) =

  let inline colorWeight d = colorSharpness * -d
  let inline isSignificant d = colorWeight d > cell.worleyWeight - 10.0f

  let scanCell jx jy jz =
    let mutable h = cell.hash(jx, jy, jz)
    for __ = 1 to count h do
      h <- mangle32 h
      let P = cell.d - Vec3f.fromSeed(h) - Vec3f(float32 jx, float32 jy, float32 jz)
      let d = P |> distance.vector |> distance.normalize
      if isSignificant d then
        let w = colorWeight d
        let c = color h
        let linearWeight = exp(min 20.0f (w - cell.worleyWeight))
        cell.worleyColor <- (cell.worleyColor + c * linearWeight) / (1.0f + linearWeight)
        if linearWeight > 1.0e4f then
          cell.worleyWeight <- w
        elif linearWeight > 1.0e-4f then
          cell.worleyWeight <- cell.worleyWeight + log (1.0f + linearWeight)
      if d < cell.d0 then
        cell.d2 <- cell.d1; cell.h2 <- cell.h1
        cell.d1 <- cell.d0; cell.h1 <- cell.h0
        cell.d0 <- d; cell.h0 <- h
      elif d < cell.d1 then
        cell.d2 <- cell.d1; cell.h2 <- cell.h1
        cell.d1 <- d; cell.h1 <- h
      elif d < cell.d2 then
        cell.d2 <- d; cell.h2 <- h
        
  let xN, xd0 = if cell.d.x < 0.5f then 0, cell.d.x else 1, 1.0f - cell.d.x
  let yN, yd0 = if cell.d.y < 0.5f then 0, cell.d.y else 1, 1.0f - cell.d.y
  let zN, zd0 = if cell.d.z < 0.5f then 0, cell.d.z else 1, 1.0f - cell.d.z

  // The initial search box contains 8 cells.
  cell.x1 <- xN
  cell.x0 <- xN - 1
  cell.y1 <- yN
  cell.y0 <- yN - 1
  cell.z1 <- zN
  cell.z0 <- zN - 1

  let expand axis far =
    if axis = 0 then
      let x' = if xN ^^^ far = 0 then cell.x0 <- cell.x0 - 1; cell.x0 else cell.x1 <- cell.x1 + 1; cell.x1
      for y = cell.y0 to cell.y1 do
        for z = cell.z0 to cell.z1 do scanCell x' y z
    elif axis = 1 then
      let y' = if yN ^^^ far = 0 then cell.y0 <- cell.y0 - 1; cell.y0 else cell.y1 <- cell.y1 + 1; cell.y1
      for x = cell.x0 to cell.x1 do
        for z = cell.z0 to cell.z1 do scanCell x y' z
    else
      let z' = if zN ^^^ far = 0 then cell.z0 <- cell.z0 - 1; cell.z0 else cell.z1 <- cell.z1 + 1; cell.z1
      for x = cell.x0 to cell.x1 do
        for y = cell.y0 to cell.y1 do scanCell x y z'

  // Our search neighborhood is an axis-aligned box. The nearest face of the box is always expanded next.
  // The expansion sequence is fixed, given plane distance ordering.
  cell.storeAxisOrder(xd0, yd0, zd0)

  // Initialize algorithm state and compute the first 8 cells.
  cell.d0 <- maxDistance
  cell.d1 <- maxDistance
  cell.d2 <- maxDistance
  cell.h0 <- 0
  cell.h1 <- 0
  cell.h2 <- 0
  cell.worleyColor <- Vec3f.zero
  cell.worleyWeight <- -infinityf
  for x = cell.x0 to cell.x1 do
    for y = cell.y0 to cell.y1 do
      for z = cell.z0 to cell.z1 do scanCell x y z

  // We can break early if the current maximum distance is lower than the distance of the next expansion.
  // Additionally, we require that the maximum color contribution of any unexpanded cell
  // is at most a small fraction of the largest weight.
  let earlyBreak expansionDistance =
    let d = distance.normalizationFactor * expansionDistance
    cell.d2 < d && isSignificant d = false

  // The initial search state corresponds to expansion of the nearest far face, from 8 cells to 12 cells.
  let mutable faceSign = -1
  let mutable faceIndex = 2
  let mutable faceDistance = 1.0f
  let mutable ready = false

  // Keep expanding until the first 2 distances are known with certainty, and the weighted cell color
  // is known to a high accuracy.
  while not ready do
    if earlyBreak (faceDistance + float32 faceSign * cell.search.[faceIndex].fst) then
      ready <- true
    else
      expand cell.search.[faceIndex].snd (negativei faceSign)
      match faceSign, faceIndex with
      | (-1, 0) ->
        // Switch from far face expansion to near face expansion.
        faceSign <- 1
      | (1, 2) ->
        // Switch from near face expansion to far face expansion.
        faceSign <- -1
        faceDistance <- faceDistance + 1.0f
      | _ ->
        faceIndex <- faceIndex + faceSign



// The following 8 weight patterns look appropriate in the colored Worley basis.
let camoPattern =
  [|
    Vec3f(1.0f, 0.0f, 0.0f)   // 0 cells
    Vec3f(1.0f, 1.0f, 0.0f)   // 1 cells with some subcells
    Vec3f(1.0f, 1.0f, 1.0f)   // 2 cells with more subcells
    Vec3f(-1.0f, 1.0f, 0.0f)  // 3 Voronoi
    Vec3f(-1.0f, 1.0f, 1.0f)  // 4 Voronoi + added diamond-like detail
    Vec3f(1.0f, 0.0f, 1.0f)   // 5 less sharp crystal pattern
    Vec3f(0.0f, 1.0f, 1.0f)   // 6 least sharp crystal pattern
    Vec3f(0.0f, 1.0f, 0.0f)   // 7 crystals with plateaus
  |]



/// Worley colored generator. Components contain camo weight patterns p0, p1 and p2
/// multiplied by closest cell color. Color fading distance between closest cells
/// is 0 < a < 1. It is typically a small value, somewhere around [0.02, 0.2].
let camo (layout : LayoutFunction) (count : FeatureCount) p0 p1 p2 (distance : CellDistance) (color : CellColor) (fade : float32 -> float32) a frequency v =
  let data = layout frequency v
  worleyColorBasis data count 1.0f distance color (3.0f / a) v
  // The colored basis stores normalized distances.
  let d = Vec3f(data.d0, data.d1, data.d2).map(fun d -> fade (max 0.0f (1.0f - d)))
  data.release()
  data.worleyColor * Vec3f(d *. camoPattern.[p0], d *. camoPattern.[p1], d *. camoPattern.[p2])




let measureDistances (seed : int) (distance : CellDistance) =

  let n = 2000000

  let mutable D0 = 0.0
  let mutable D1 = 0.0
  let mutable D2 = 0.0
  let rnd = Rnd(seed)

  for __ = 1 to n do

    let v = rnd.vec3f()
    let cell = hifiLayout (rnd.expf(2.0f, 128.0f)) v
    worleyBasis cell unityCount 100.0f distance v

    D0 <- D0 + float cell.d0
    D1 <- D1 + float cell.d1
    D2 <- D2 + float cell.d2

  printfn "Average d0: %f | 1/d0: %f" (D0 / float n) (float n / D0)
  printfn "Average d1: %f | 1/d1: %f" (D1 / float n) (float n / D1)
  printfn "Average d2: %f | 1/d2: %f" (D2 / float n) (float n / D2)

