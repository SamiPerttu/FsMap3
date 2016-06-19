namespace FsMap3

open Common
open Basis3
open Map3
open Map3Info


/// A "rich" Map3 stores a 2-window for displaying an image and sampled information
/// for normalization and filtering.
[<NoEquality; NoComparison>]
type RichMap3 =
  {
    /// The colored map with the view window transformed to the XY unit square at Z = 0.
    map : Map3
    /// Palette transform.
    palette : Map3
    /// Map view center.
    center : Vec3f
    /// Map view zoom.
    zoom : float32
    /// Info that was gathered from uncolored map.
    info : Map3Info
  }

  member inline this.viewWidth = 1.0f / this.zoom

  member inline this.viewHeight = 1.0f / this.zoom

  member inline this.detailLevel =
    match this.info.slope99 with
    | Someval(slope) -> 2.0f * slope * max this.viewWidth this.viewHeight
    | Noneval -> 0.0f

  member this.viewBox =
    let w = this.viewWidth
    let h = this.viewHeight
    (this.center.x - w * 0.5f, this.center.y - h * 0.5f, this.center.x + w * 0.5f, this.center.y + h * 0.5f)

  member this.camera = fun x y -> Vec3f(x, y, 0.0f)

  static member inline pixmapCamera(w, h, x, y) =
    let Z = 1.0f / float32 (max w h)
    Vec3f(float32 x * Z, float32 y * Z, 0.0f)

  static member pixmapSourceWith(extraTransform : Vec3f -> Vec3f) =
    fun (rich : RichMap3) ->
      { new IPixmapSource with
        member __.start(w, h) = ()
        member __.getPixel(w, h, x, y) = rich.map (RichMap3.pixmapCamera(w, h, x, y) |> extraTransform) * 0.5f + Vec3f(0.5f)
        member __.finish() = ()
        member __.postFx(pixmap) = ()
      }

  static member pixmapSource =
    fun (rich : RichMap3) ->
      { new IPixmapSource with
        member __.start(w, h) = ()
        member __.getPixel(w, h, x, y) = rich.map (RichMap3.pixmapCamera(w, h, x, y)) * 0.5f + Vec3f(0.5f)
        member __.finish() = ()
        member __.postFx(pixmap) = ()
      }

  /// Generates a RichMap3. The palette and 2-window are generated and applied separately here.
  /// The map generator is supplied as an argument.
  static member generate (prepareToFilter : bool) (mapGenerator : Dna -> Map3) = fun (dna : Dna) ->
    let clerp = lerp -50.0f 50.0f
    let centerX = dna.float32("View Center X", clerp)
    let centerY = dna.float32("View Center Y", clerp)
    let centerZ = dna.float32("View Center Z", clerp)
    let center = Vec3f(centerX, centerY, centerZ)
    let zoom = dna.float32("View Zoom", xerp 0.5e-2f 0.5e3f)
    let offset = Vec3f(centerX - 0.5f / zoom, centerY - 0.5f / zoom, centerZ)
    let viewTransform (v : Vec3f) = v / zoom + offset

    let palette = dna.descend("Palette", ColorDna.genPalette 32)

    let info = Map3Info.create(mapGenerator, dna, retainSamples = true, computeSlopes = true)

    {
      RichMap3.map = viewTransform >> info.map >> palette
      palette = palette
      center = center
      zoom = zoom
      info = info
    }

    

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

