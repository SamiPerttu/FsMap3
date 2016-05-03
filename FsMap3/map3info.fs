/// Normalization and filtering of 3-maps by sampling.
module FsMap3.Map3Info

open Common
open Basis3
open Map3


[<NoEquality; NoComparison>]
type Map3Info =
  {
    /// Fingerprint of the map.
    fingerprint : int64
    /// Estimated minimum component values of the original map.
    min : Vec3f
    /// Estimated maximum component values of the original map.
    max : Vec3f
    /// Sampled 90 percentile slope of the normalized map.
    slope90 : float32 Optionval
    /// Sampled 99 percentile slope of the normalized map.
    slope99 : float32 Optionval
    /// Sampled average standard deviation of component values in the normalized map.
    deviation : float32 Optionval
    /// Sample set of values from pseudo-random points in the normalized map. Can be used for fingerprinting.
    sampleArray : Vec3f[]
  }

  /// Returns a normalizer map for the source map. The slopes are defined in this map.
  member inline this.normalizer =
    let rZ = Vec3f(2.0f) / (this.max - this.min)
    let rM = (this.min + this.max) / 2.0f
    fun (v : Vec3f) -> ((v - rM) * rZ).map(clamp11)



let map3InfoCache = HashMap<int64, Map3Info>.create(int)



type Map3Info with

  static member create(map : Map3, ?fingerprint, ?retainSamples, ?computeDeviation, ?computeSlopes) =

    let retainSamples = retainSamples >? false
    let computeDeviation = computeDeviation >? false
    let computeSlopes = computeSlopes >? false

    let cache = map3InfoCache
    let existing = match fingerprint with | Some fingerprint -> lock cache (fun _ -> cache.find(fingerprint)) | None -> Noneval

    (*
    fingerprint.apply(fun fingerprint ->
      match existing with
      | Someval info -> Log.infof "Map3Info: existing info found for fingerprint %d" fingerprint
      | Noneval -> Log.infof "Map3Info: no info found for fingerprint %d" fingerprint
      )
    *)

    let matching = existing.filter(fun info ->
      (not retainSamples || info.sampleArray.size > 0)
      && (not computeDeviation || info.deviation.isSome)
      && (not computeSlopes || info.slope90.isSome)
      )

    if matching.isSome then
      // A matching Node3Info was found with all the information we need - use it.
      !matching

    else
      // There may be an existing Node3Info but it does not contain all the information we need.
      let R = 16
      let Ri = 1.0f / float32 R
      let N = cubed R
      let epsilon = 1.0e-4f

      let computeGradients = if existing.isSomeAnd(fun existing -> existing.slope90.isSome) then false else computeSlopes
      let gradientArray = if computeGradients then Array.zeroCreate N else Array.createEmpty
      let sampleArray = Array.zeroCreate N

      let sampleZ z =
        let rnd = Rnd(z)
        let mutable minV = Vec3f(infinityf)
        let mutable maxV = Vec3f(-infinityf)
        for x = 0 to R - 1 do
          for y = 0 to R - 1 do
            let i = (z * R + x) * R + y
            let P = (Vec3f(float32 x, float32 y, float32 z) + rnd.vec3f()) * Ri
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

      for (v0, v1) in Array.Parallel.map sampleZ [| 0 .. R - 1 |] do
        minV <- Vec3f.minimize minV v0
        maxV <- Vec3f.maximize maxV v1
      minV <- minV - abs minV * 1.0e-6f - Vec3f(1.0e-6f)
      maxV <- maxV + abs maxV * 1.0e-6f + Vec3f(1.0e-6f)

      let ranV = maxV - minV
      let meanV = average minV maxV

      let sd =
        if existing.isSomeAnd(fun existing -> existing.deviation.isSome) then
          (!existing).deviation
        elif computeDeviation then
          let estimatorX = Mat.MomentEstimator()
          let estimatorY = Mat.MomentEstimator()
          let estimatorZ = Mat.MomentEstimator()
          sampleArray.modify(fun v ->
            let v' = (v - meanV) / ranV * 2.0f
            estimatorX.add(float v'.x)
            estimatorY.add(float v'.y)
            estimatorZ.add(float v'.z)
            v'
            )
          let sd = (estimatorX.deviation + estimatorY.deviation + estimatorZ.deviation) / 3.0
          Someval(float32 sd)
        else
          Noneval

      let slope90, slope99 =
        if existing.isSomeAnd(fun existing -> existing.slope90.isSome) then
          (!existing).slope90, (!existing).slope99
        elif computeSlopes then
          let slopeArray = Array.init N (fun i -> (gradientArray.[i] / ranV).length)
          let rank90 = (int <| float N * 0.90)
          let i90 = Fun.quickselect 0 (N - 1) (fun i -> slopeArray.[i]) (fun i j -> slopeArray.swap(i, j)) rank90
          let slope90 = slopeArray.[i90]
          let rank99 = (int <| float N * 0.99)
          let i99 = Fun.quickselect 0 (N - 1) (fun i -> slopeArray.[i]) (fun i j -> slopeArray.swap(i, j)) rank99
          let slope99 = slopeArray.[i99]
          Someval(slope90), Someval(slope99)
        else
          Noneval, Noneval

      let info =
        {
          Map3Info.fingerprint = fingerprint >? 0L
          min = minV
          max = maxV
          slope90 = slope90
          slope99 = slope99
          deviation = sd
          sampleArray = if retainSamples then sampleArray else Array.createEmpty
        }

      // Insert or replace the info. The new info is always a superset of any existing info.
      fingerprint.apply(fun fingerprint -> lock cache (fun _ -> cache.[fingerprint] <- info))
      info


  /// Generates a map and its info. Cache aware.
  static member create(generator : Dna -> Map3, dna : Dna, ?retainSamples, ?computeDeviation, ?computeSlopes) =
    let i0 = dna.size
    let map = generator dna
    let i1 = dna.last
    let hash = Mangle.Hash128.create()
    for i = i0 to i1 do
      hash.hash(dna.[i].semanticId)
    hash.hashEnd()
    let info = Map3Info.create(map, hash.a64, retainSamples >? false, computeDeviation >? false, computeSlopes >? false)
    info, map



/// Normalizes the map produced by the generator. Cache aware.
let normalize (generator : Dna -> Map3) (dna : Dna) =
  let info, map = Map3Info.create(generator, dna)
  map >> info.normalizer


let normalizeBasis (basisGenerator : Dna -> float32 -> Map3) (dna : Dna) =

  let basis = ref nullRef

  let generator = fun (dna : Dna) ->
    basis := basisGenerator dna
    let dummy = dna.int("Dummy Basis", 1)
    !basis 200.0f

  let info, _ = Map3Info.create(generator, dna)

  fun frequency ->
    let map = !basis frequency
    fun (v : Vec3f) -> map v |> info.normalizer


