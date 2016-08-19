namespace FsMap3

open Common


/// Applies some statistical filters to a generated map.
[<NoEquality; NoComparison>]
type RichMap3Filter =
  {
    mutable minDetail : float32
    mutable maxDetail : float32
    mutable minDeviation : float32
    mutable minDifference : float32
    mutable maxDifference : float32
  }

  // Returns whether the map passes the filters.
  member this.filter(map : RichMap3, map0 : RichMap3 option) =
    
    let viewSize = max map.viewWidth map.viewHeight

    let detailLevel0 = map0.map(fun map0 -> map0.detailLevel)
    let deviation0 = map0.map(fun map0 -> map0.info.deviation)

    let detailMin = min this.minDetail (detailLevel0 >? infinityf |> (*) 0.9f)
    let detailMax = max this.maxDetail (detailLevel0 >? 0.0f |> (*) 1.1f)
    let deviationMin = min this.minDeviation (deviation0 >? infinityf |> (*) 0.9f)

    if map.detailLevel < detailMin || map.detailLevel > detailMax then
      Log.infof "Map detail level rejected: minimum %d actual %d maximum %d" (int detailMin) (int map.detailLevel) (int detailMax)
      false
    elif map.info.deviation < deviationMin then
      Log.infof "Map sample deviation too small: deviation minimum %.2f actual %.2f" deviationMin map.info.deviation
      false
    else
      match map0 with
      | Some map0 ->
        let sample0 = map0.info.sampleArray
        let sample1 = map.info.sampleArray
        let n = min sample0.size sample1.size
        if n > 0 && map.info.sampleDiameter = map0.info.sampleDiameter then
          let difference = Fun.sum 0 (n - 1) (fun i -> (sample0.[i] - sample1.[i]).norm1) / float32 n
          if difference < this.minDifference then
            Log.infof "Map is too similar: difference %.3f" difference
            false
          elif difference > this.maxDifference then
            Log.infof "Map is too dissimilar: difference %.3f" difference
            false
          else
            Log.infof "Map accepted: detail level %d deviation %.2f difference %.3f" (int map.detailLevel) map.info.deviation difference
            true
        else
          Log.infof "Map accepted: detail level %d deviation %.2f" (int map.detailLevel) map.info.deviation
          true
      | None ->
        Log.infof "Map accepted: detail level %d deviation %.2f" (int map.detailLevel) map.info.deviation
        true

