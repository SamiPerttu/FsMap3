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
    /// The map, normalized and colored, with the view window transformed to the XY unit square at Z = 0.
    map : Map3
    /// Map view center.
    center : Vec3f
    /// Map view zoom.
    zoom : float32
    /// View aspect ratio. TODO: This should be handled by the layout.
    aspectRatio : float32
    /// Info that was gathered during normalization. Can be useful for filtering.
    info : Map3Info
  }

  member inline this.viewWidth = 1.0f / this.zoom

  member inline this.viewHeight = 1.0f / this.zoom

  member this.viewBox =
    let w = this.viewWidth
    let h = this.viewHeight
    (this.center.x - w * 0.5f, this.center.y - h * 0.5f, this.center.x + w * 0.5f, this.center.y + h * 0.5f)

  member this.camera = fun x y -> Vec3f(x, y, 0.0f)

  static member inline pixmapCamera(w, h, x, y) = Vec3f(float32 x / float32 w, float32 y / float32 h, 0.0f)

  static member pixmapGenerator(extraTransform : Vec3f -> Vec3f) =
    fun (rich : RichMap3) ->
      { new IPixmapSource with
        member __.start(w, h) = ()
        member __.getPixel(w, h, x, y) = rich.map (RichMap3.pixmapCamera(w, h, x, y) |> extraTransform) * 0.5f + Vec3f(0.5f)
        member __.finish() = ()
        member __.postFx(pixmap) = ()
      }

  static member pixmapGenerator() =
    fun (rich : RichMap3) ->
      { new IPixmapSource with
        member __.start(w, h) = ()
        member __.getPixel(w, h, x, y) = rich.map (RichMap3.pixmapCamera(w, h, x, y)) * 0.5f + Vec3f(0.5f)
        member __.finish() = ()
        member __.postFx(pixmap) = ()
      }

  /// Applies some common sense filtering to a generated map. Returns true if the map seems acceptable.
  static member filter (map : RichMap3) (previous : RichMap3 option) =
    let slope99Max = max (512.0f / map.viewHeight) (previous.map(fun previous -> previous.info.slope99 >? 0.0f) >? 0.0f)
    let slope99Min = min (5.120f / map.viewHeight) (previous.map(fun previous -> previous.info.slope99 >? infinityf) >? infinityf)
    let deviationMin = min 0.03f (previous.map(fun previous -> previous.info.deviation >? infinityf) >? infinityf)
    if !map.info.slope99 > slope99Max || !map.info.slope99 < slope99Min || !map.info.deviation < deviationMin then
      Log.infof "Map detail level rejected. 99%% slope %d (maximum %d) | deviation %f (minimum %f)" (int !map.info.slope99) (int slope99Max) !map.info.deviation deviationMin
      false
    else
      Log.infof "Map detail level accepted. 99%% slope %d (maximum %d) | deviation %f (minimum %f)" (int !map.info.slope99) (int slope99Max) !map.info.deviation deviationMin
      match previous with
      | Some previous ->
        let sample0 = previous.info.sampleArray
        let sample1 = map.info.sampleArray
        let n = min sample0.size sample1.size
        let difference = Fun.sum 0 (n - 1) (fun i -> (sample0.[i] - sample1.[i]).norm1)
        if n > 0 && difference < 1.0f then
          Log.infof "Map is too similar. Absolute difference = %f" difference
          false
        else
          if n > 0 then Log.infof "Map is dissimilar enough. Absolute difference = %f" difference
          true
      | None -> true

  /// Generates a RichMap3. The palette and 2-window are generated and applied separately here.
  /// The map generator is supplied as an argument.
  static member generate(mapGenerator : Dna -> Map3) = fun (dna : Dna) ->
    let clerp = lerp -50.0f 50.0f
    let centerX = dna.float32("View Center X", clerp)
    let centerY = dna.float32("View Center Y", clerp)
    let centerZ = dna.float32("View Center Z", clerp)
    let center = Vec3f(centerX, centerY, centerZ)
    let zoom = dna.float32("View Zoom", xerp 0.5e-2f 0.5e3f)
    let offset = Vec3f(centerX - 0.5f / zoom, centerY - 0.5f / zoom, centerZ)
    let aspectRatio = 1.0f
    let palette = dna.descend("Palette", Color.genPalette 32)
    let viewTransform (v : Vec3f) = v / zoom + offset
    let info, map = Map3Info.create(mapGenerator, dna, retainSamples = true, computeDeviation = true, computeSlopes = true)
    {
      RichMap3.map = viewTransform >> map >> info.normalizer >> palette
      center = center
      zoom = zoom
      aspectRatio = aspectRatio
      info = info
    }
    
