// Normalization and filtering of 3-maps by sampling.
namespace FsMap3

open Common
open Basis3
open Map3


[<NoEquality; NoComparison>]
type Map3Info =
  {
    /// Estimated minimum component values of the source map.
    min : Vec3f
    /// Estimated maximum component values of the source map.
    max : Vec3f
    /// Sampled 90 percentile slope of the normalized map.
    slope90 : float32
    /// Sampled 99 percentile slope of the normalized map.
    slope99 : float32
    /// Sampled average standard deviation of component values in the normalized map.
    deviation : float32
    /// Sample set of values from pseudo-random points in the normalized map. Can be used for fingerprinting.
    sampleArray : Vec3f[]
  }

  /// Returns a normalizer map for the source map. The slopes are defined in this map.
  member inline this.normalizer =
    let rZ = Vec3f(2.0f) / (this.max - this.min)
    let rM = (this.min + this.max) / 2.0f
    fun (v : Vec3f) -> ((v - rM) * rZ).map(clamp11)

  static member create (f : Map3) =
    let R = 16
    let Ri = 1.0f / float32 R
    let N = cubed R
    let epsilon = 1.0e-5f
    let gradientArray = Array.zeroCreate N
    let sampleArray = Array.zeroCreate N
    let sampleZ z =
      let rnd = Rnd(z)
      let mutable minV = Vec3f(infinityf)
      let mutable maxV = Vec3f(-infinityf)
      for x = 0 to R - 1 do
        for y = 0 to R - 1 do
          let P = (Vec3f(float32 x, float32 y, float32 z) + rnd.vec3f()) * Ri
          let v = f P
          minV <- Vec3f.minimize minV v
          maxV <- Vec3f.maximize maxV v
          let u = f (P + epsilon * rnd.unitVec3f())
          let gradient = (u - v) / epsilon
          minV <- Vec3f.minimize minV u
          maxV <- Vec3f.maximize maxV u
          let i = (z * R + x) * R + y
          sampleArray.[i] <- v
          gradientArray.[i] <- gradient
      (minV, maxV)
    let mutable minV = Vec3f(infinityf)
    let mutable maxV = Vec3f(-infinityf)
    for (v0, v1) in Array.Parallel.map sampleZ [| 0 .. R - 1 |] do
      minV <- Vec3f.minimize minV v0
      maxV <- Vec3f.maximize maxV v1
    minV <- minV - abs minV * 1.0e-6f - Vec3f 1.0e-6f
    maxV <- maxV + abs maxV * 1.0e-6f + Vec3f 1.0e-6f
    let ranV = maxV - minV
    let meanV = average minV maxV
    let slopeArray = Array.init N (fun i -> (gradientArray.[i] / ranV).length)
    let estimator = Array.init 3 (fun _ -> Mat.MomentEstimator())
    sampleArray.modify(fun v ->
      let v' = (v - meanV) / ranV * 2.0f
      estimator.[0].add(float v'.x)
      estimator.[1].add(float v'.y)
      estimator.[2].add(float v'.z)
      v'
      )
    let sd = (estimator.[0].deviation + estimator.[1].deviation + estimator.[2].deviation) / 3.0
    let rank90 = (int <| float N * 0.90)
    let i90 = Fun.quickselect 0 (N - 1) (fun i -> slopeArray.[i]) (fun i j -> slopeArray.swap(i, j)) rank90
    let slope90 = slopeArray.[i90]
    let rank99 = (int <| float N * 0.99)
    let i99 = Fun.quickselect 0 (N - 1) (fun i -> slopeArray.[i]) (fun i j -> slopeArray.swap(i, j)) rank99
    let slope99 = slopeArray.[i99]
    { Map3Info.min = minV; max = maxV; slope90 = slope90; slope99 = slope99; deviation = float32 sd; sampleArray = sampleArray }

