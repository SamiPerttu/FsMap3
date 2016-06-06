/// The Worley cellular basis.
module FsMap3.Worley

open Common
open Mangle
open Basis3
open FeatureCount
open CellColor


// Ignoring signedness, there are 13 basic Worley cell distance (d0, d1, d2) weight patterns.
let worleyPattern =
  [|
    Vec3f(1.0f, 0.0f, 0.0f)   //  0-1  cells
    Vec3f(-1.0f, 0.0f, 0.0f)
    Vec3f(1.0f, 1.0f, 0.0f)   //  2-3  cells with some subcells
    Vec3f(-1.0f, -1.0f, 0.0f)
    Vec3f(1.0f, 1.0f, 1.0f)   //  4-5  cells with more subcells
    Vec3f(-1.0f, -1.0f, -1.0f)
    Vec3f(-1.0f, 1.0f, 0.0f)  //  6-7  Voronoi
    Vec3f(1.0f, -1.0f, 0.0f)
    Vec3f(-1.0f, 1.0f, 1.0f)  //  8-9  Voronoi + added diamond-like detail
    Vec3f(1.0f, -1.0f, -1.0f)
    Vec3f(0.0f, -1.0f, 1.0f)  // 10-11 more detailed Voronoi with somewhat web-like appearance
    Vec3f(0.0f, 1.0f, -1.0f)
    Vec3f(-1.0f, -1.0f, 1.0f) // 12-13 most detailed Voronoi (this is the only even pattern not guaranteed to have positive value)
    Vec3f(1.0f, 1.0f, -1.0f)
    Vec3f(1.0f, -1.0f, 1.0f)  // 14-15 crystal pattern
    Vec3f(-1.0f, 1.0f, -1.0f)
    Vec3f(1.0f, 0.0f, 1.0f)   // 16-17 less sharp crystal pattern
    Vec3f(-1.0f, 0.0f, -1.0f)
    Vec3f(0.0f, 1.0f, 1.0f)   // 18-19 least sharp crystal pattern
    Vec3f(0.0f, -1.0f, -1.0f)
    Vec3f(0.0f, 1.0f, 0.0f)   // 20-21 crystals with plateaus
    Vec3f(0.0f, -1.0f, 0.0f)
    Vec3f(0.0f, 0.0f, 1.0f)   // 22-23 web-like polygons
    Vec3f(0.0f, 0.0f, -1.0f)
    Vec3f(-1.0f, 0.0f, 1.0f)  // 24-25 weird web-like pattern
    Vec3f(1.0f, 0.0f, -1.0f)
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
    member __.vector(v) = v.norm1
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

  // Keep expanding until the first 3 distances are known with certainty up to the maximum distance.
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
let worley (layout : LayoutFunction) (count : FeatureCount) p0 p1 p2 (distance : CellDistance) (fade : float32 -> float32) seed (frequency : float32) =
  let layoutInstance = layout seed frequency
  fun (v : Vec3f) ->
    let data = layoutInstance.run v
    worleyBasis data count (2.0f / distance.normalizationFactor) distance v
    let d = Vec3f(data.d0, data.d1, data.d2).map(fun d -> fade (min 1.0f (0.5f * d)))
    data.release()
    Vec3f(d *. worleyPattern.[p0], d *. worleyPattern.[p1], d *. worleyPattern.[p2])


/// Default Worley basis with the default layout function and unity Poisson feature distribution.
/// The cell hash seed is derived from the frequency.
let inline worleyd p distance fade frequency =
  worley hifiLayout unityPoisson p ((p + 1) % worleyPattern.size) ((p + 2) % worleyPattern.size) distance fade (manglef32 frequency) frequency



/// Generic colored Worley cellular basis function.
let worleyColorBasis (cell : BasisData) (count : FeatureCount) maxDistance (distance : CellDistance) (color : CellColor) (colorSharpness : float32) (v : Vec3f) =

  let inline colorWeight d = colorSharpness * -d

  let scanCell jx jy jz =
    let mutable h = cell.hash(jx, jy, jz)
    for __ = 1 to count h do
      h <- mangle32 h
      let P = cell.d - Vec3f.fromSeed(h) - Vec3f(float32 jx, float32 jy, float32 jz)
      let d = P |> distance.vector |> distance.normalize
      let w = colorWeight d
      if w > cell.worleyWeight - 10.0f then
        let c = color h P
        let linearWeight = exp(min 20.0f (w - cell.worleyWeight))
        cell.worleyColor <- (cell.worleyColor + c * linearWeight) / (1.0f + linearWeight)
        if linearWeight > 1.0e5f then
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
    cell.d2 + 10.0f / colorSharpness < distance.normalizationFactor * expansionDistance

  // The initial search state corresponds to expansion of the nearest far face, from 8 cells to 12 cells.
  let mutable faceSign = -1
  let mutable faceIndex = 2
  let mutable faceDistance = 1.0f
  let mutable ready = false

  // Keep expanding until the first 3 distances are known with certainty up to the maximum distance,
  // and the cell color is known with sufficient accuracy.
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


/// Worley colored basis. Components contain Worley patterns p0, p1 and p2
/// adjusted by closest cell color. Color fade distance between closest cells
/// is typically a small value, somewhere around [0.02, 0.2].
let camo (layout : LayoutFunction)
         (count : FeatureCount)
         p0 p1 p2
         (distance : CellDistance)
         (color : CellColor)
         (fade : float32 -> float32)
         colorFadeDistance
         seed frequency =
  let layoutInstance = layout seed frequency
  fun (v : Vec3f) ->
    let data = layoutInstance.run v
    worleyColorBasis data count 2.0f distance color (3.0f / colorFadeDistance) v
    // The colored basis stores normalized distances.
    let d = Vec3f(data.d0, data.d1, data.d2).map(fun d -> fade (min 1.0f (0.5f * d)))
    let worleyColor = data.worleyColor
    data.release()
    (1G + worleyColor) * Vec3f(d *. worleyPattern.[p0], d *. worleyPattern.[p1], d *. worleyPattern.[p2])

