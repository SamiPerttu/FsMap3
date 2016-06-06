/// Map3 normalization and filtering by sampling.
module FsMap3.Map3Info

open Common
open Basis3
open Map3


type Map3InfoConfig =
  /// Resolution of the sampling grid.
  static member R = 16
  /// Number of tasks to split sampling into.
  static member T = 8



[<NoEquality; NoComparison>]
type Map3Info =
  {
    /// Fingerprint of the map. Needed only if we want to use the cache.
    fingerprint : int64
    /// Estimated minimum component values of the original map.
    min : Vec3f
    /// Estimated maximum component values of the original map.
    max : Vec3f
    /// The normalized map.
    map : Map3
    /// Sampled 90 percentile slope of the normalized map.
    slope90 : float32 Optionval
    /// Sampled 99 percentile slope of the normalized map.
    slope99 : float32 Optionval
    /// Sampled average standard deviation of component values in the normalized map.
    deviation : float32
    /// Sampling diameter. For tiling maps the optimal value is 1, as then the samples are stratified in the unit cube.
    sampleDiameter : float32
    /// Sample set of values from pseudo-random points in the normalized map. Can be used for fingerprinting.
    sampleArray : Vec3f[]
    /// Sample set of gradients from pseudo-random points in the normalized map. Can be used for fingerprinting.
    gradientArray : Vec3f[]
  }

  /// A normalizer for the source map.
  member inline this.normalizer =
    let Z = Vec3f(2.0f) / (this.max - this.min)
    let mean = average this.min this.max
    fun (v : Vec3f) -> (v - mean) * Z



let map3InfoCache = HashMap<int64, Map3Info>.create(int)

let map3InfoTasks =
  let taskSize = Map3InfoConfig.R / Map3InfoConfig.T
  enforce (taskSize * Map3InfoConfig.T = Map3InfoConfig.R) "Map3InfoConfig: Invalid task configuration."
  Array.init Map3InfoConfig.T (fun i -> (i * taskSize, i * taskSize + taskSize - 1))

  

type Map3Info with

  static member create(map : Map3, ?fingerprint, ?sampleDiameter, ?retainSamples, ?computeSlopes) =

    let retainSamples = retainSamples >? false
    let computeSlopes = computeSlopes >? false
    let sampleDiameter = sampleDiameter >? 7.0f

    let cache = map3InfoCache
    let existing = match fingerprint with | Some fingerprint -> lock cache (fun _ -> cache.find(fingerprint)) | None -> Noneval

    if false then fingerprint.apply(fun fingerprint ->
      match existing with
      | Someval info -> Log.infof "Map3Info: existing info found for fingerprint %d" fingerprint
      | Noneval -> Log.infof "Map3Info: no info found for fingerprint %d" fingerprint
      )

    let matching =
      existing.filter(fun info -> (not retainSamples || info.sampleArray.size > 0) && (not computeSlopes || info.slope90.isSome))

    if matching.isSome then
      // A matching Map3Info was found with all the information we need - use it.
      !matching

    else
      // There may be an existing Map3Info but it does not contain all the information we need.
      let R = Map3InfoConfig.R
      let Zi = sampleDiameter / float32 R
      let Ci = Vec3f(sampleDiameter * 0.5f)
      let N = cubed R
      let epsilon = 1.0e-4f

      let computeGradients = if existing.isSomeAnd(fun existing -> existing.slope90.isSome) then false else computeSlopes
      let gradientArray = if computeGradients then Array.zeroCreate N else Array.createEmpty
      let sampleArray = Array.zeroCreate N

      let sampleZ(z0, z1) =
        let rnd = Rnd(z0)
        let mutable minV = Vec3f(infinityf)
        let mutable maxV = Vec3f(-infinityf)
        for z = z0 to z1 do
          for x = 0 to R - 1 do
            for y = 0 to R - 1 do
              let i = (z * R + x) * R + y
              // We sample an origin centered cube in a stratified pattern.
              let P = (Vec3f(float32 x, float32 y, float32 z) + rnd.vec3f()) * Zi + Ci
              let v = map P
              minV <- Vec3f.minimize minV v
              maxV <- Vec3f.maximize maxV v
              sampleArray.[i] <- v
              if computeGradients then
                let u = map (P + epsilon * rnd.unitVec3f())
                let gradient = (u - v) / epsilon
                minV <- Vec3f.minimize minV u
                maxV <- Vec3f.maximize maxV u
                gradientArray.[i] <- gradient
        (minV, maxV)

      let mutable minV = Vec3f(infinityf)
      let mutable maxV = Vec3f(-infinityf)

      for (v0, v1) in Array.Parallel.map sampleZ map3InfoTasks do
        minV <- Vec3f.minimize minV v0
        maxV <- Vec3f.maximize maxV v1
      minV <- minV - abs minV * 1.0e-3f - Vec3f(1.0e-6f)
      maxV <- maxV + abs maxV * 1.0e-3f + Vec3f(1.0e-6f)

      let rangeV = maxV - minV
      let meanV = average minV maxV
      let rangeZ = Vec3f(2.0f) / rangeV

      // Normalize value and gradient samples.
      sampleArray.modify(fun v -> (v - meanV) * rangeZ)
      gradientArray.modify((*) rangeZ)

      let deviation =
        match existing with
        | Someval(existing) ->
          existing.deviation
        | Noneval ->
          let estimatorX = Mat.MomentEstimator()
          let estimatorY = Mat.MomentEstimator()
          let estimatorZ = Mat.MomentEstimator()
          sampleArray.iter(fun v ->
            estimatorX.add(float v.x)
            estimatorY.add(float v.y)
            estimatorZ.add(float v.z)
            )
          average3 estimatorX.deviation estimatorY.deviation estimatorZ.deviation |> float32

      let slope90, slope99 =
        if existing.isSomeAnd(fun existing -> existing.slope90.isSome) then
          (!existing).slope90, (!existing).slope99
        elif computeSlopes then
          let slopeArray = Array.init N (fun i -> gradientArray.[i].length)
          let rank90 = float N * 0.90 |> int
          let i90 = Fun.quickselect 0 (N - 1) slopeArray.at (Array.swap slopeArray) rank90
          let slope90 = slopeArray.[i90]
          let rank99 = float N * 0.99 |> int
          let i99 = Fun.quickselect 0 (N - 1) slopeArray.at (Array.swap slopeArray) rank99
          let slope99 = slopeArray.[i99]
          Someval(slope90), Someval(slope99)
        else
          Noneval, Noneval

      let info =
        {
          Map3Info.fingerprint = fingerprint >? 0L
          min = minV
          max = maxV
          map = fun (v : Vec3f) -> (map v - meanV) * rangeZ
          slope90 = slope90
          slope99 = slope99
          deviation = deviation
          sampleDiameter = sampleDiameter
          sampleArray = if retainSamples then sampleArray else Array.createEmpty
          gradientArray = if retainSamples then gradientArray else Array.createEmpty
        }

      // Insert or replace the info. The new info is always a superset of any existing info.
      fingerprint.apply(fun fingerprint -> lock cache (fun _ -> cache.[fingerprint] <- info))
      info


  /// Generates a map and its info. Cache aware.
  static member create(generator : Dna -> Map3, dna : Dna, ?extraId : int, ?retainSamples, ?computeSlopes) =
    let i0 = dna.size
    let map = generator dna
    let i1 = dna.last
    let hash = Mangle.Hash128.create()
    extraId.apply(hash.hash)
    for i = i0 to i1 do
      hash.hash(dna.[i].semanticId)
      hash.hash(dna.[i].value)
    hash.hashEnd()
    Map3Info.create(map, hash.a64, retainSamples = (retainSamples >? false), computeSlopes = (computeSlopes >? false))



/// Normalizes the map from the generator, including in the hash extraId, which summarizes
/// extra data already supplied to the generator. Cache aware.
let normalizeWithId extraId (generator : Dna -> Map3) (dna : Dna) =
  let info = Map3Info.create(generator, dna)
  info.map



/// Normalizes the map from the generator. Cache aware.
let normalize generator dna =
  normalizeWithId 0 generator dna



/// Normalizes the basis from the generator. Cache aware.
let normalizeBasis (basisGenerator : Dna -> float32 -> Map3) (dna : Dna) =

  let basis = ref nullRef

  let generator = fun (dna : Dna) ->
    basis := basisGenerator dna
    // Instantiate the basis with an arbitrary high frequency for sampling.
    !basis 200.0f

  let info = Map3Info.create(generator, dna, extraId = 0xba515)

  fun frequency ->
    let map = !basis frequency
    map >> info.normalizer


