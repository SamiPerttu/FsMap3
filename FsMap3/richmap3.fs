namespace FsMap3

open Common
open Basis3
open Map3


/// A "rich" Map3 stores a 2-window for displaying an image and sampled information
/// for normalization and filtering.
[<NoEquality; NoComparison>]
type RichMap3 =
  {
    /// The map, normalized and colored, with the view window transformed to the unit square at Z = 0.
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

  static member pixmapGenerator (extraTransform : Vec3f -> Vec3f) (rich : RichMap3) =
    { new IPixmapSource with
      member __.start(w, h) = ()
      member __.getPixel(w, h, x, y) =
        rich.map (Vec3f(float32 x / float32 w, float32 y / float32 h, 0.0f) |> extraTransform) * 0.5f + Vec3f(0.5f)
      member __.finish() = ()
      member __.postFx(pixmap) = ()
    }

  static member filterDetail(rich : RichMap3) =
    let maxSlope = 512.0f / rich.viewHeight
    let minSlope = maxSlope * 0.01f
    let minDeviation = 0.03f
    if rich.info.slope99 > maxSlope || rich.info.slope99 < minSlope || rich.info.deviation < minDeviation then
      Log.infof "Map was rejected. 99%% slope %d (maximum %d) | deviation %f (minimum %f)" (int rich.info.slope99) (int maxSlope) rich.info.deviation minDeviation
      false
    else
      Log.infof "Map was accepted. 99%% slope %d (maximum %d) | deviation %f (minimum %f)" (int rich.info.slope99) (int maxSlope) rich.info.deviation minDeviation
      true

  /// Generates a RichMap3. The palette and 2-window are generated separately here.
  /// The map generator is supplied as an argument.
  static member generate(mapGenerator : Dna -> Map3) = fun (dna : Dna) ->
    let clerp = lerp -1000.0f 1000.0f
    let centerX = dna.float32("View Center X", clerp)
    let centerY = dna.float32("View Center Y", clerp)
    let centerZ = dna.float32("View Center Z", clerp)
    let center = Vec3f(centerX, centerY, centerZ)
    let zoom = dna.float32("View Zoom", xerp 1.0e-4f 1.0e4f)
    let offset = Vec3f(centerX - 0.5f / zoom, centerY - 0.5f / zoom, centerZ)
    let aspectRatio = 1.0f
    let palette = Color.genPalette 32 dna
    let viewTransform (v : Vec3f) = v / zoom + offset
    let map = mapGenerator dna
    let info = Map3Info.create(map)
    {
      RichMap3.map = viewTransform >> map >> info.normalizer >> palette
      center = center
      zoom = zoom
      aspectRatio = aspectRatio
      info = info
    }
    
