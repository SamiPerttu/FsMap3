namespace FsMap3

open Common
open Basis3
open Map3
open Map3Info
open Map3Cache


/// A "rich" Map3 stores a 2-window for displaying an image and sampled information
/// for normalization and filtering.
[<NoEquality; NoComparison>]
type RichMap3 =
  {
    /// The uncolored map with the view window transformed to the XY unit square at Z = 0.
    uncolored : Map3
    /// Palette transform.
    palette : Map3
    /// Map view center.
    center : Vec3f
    /// Map view zoom.
    zoom : float32
    /// Info that was gathered from uncolored, untransformed map.
    info : Map3Info
  }

  /// The colored, transformed map.
  member this.map = this.uncolored >> this.palette

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

  static member pixmapSource =
    fun (rich : RichMap3) ->
      let mutable cache = None
      { new IPixmapSource with
        member __.start(w, h) =
          cache <- Some(Map3Cache.fill(rich.info.fingerprint, rich.center, rich.zoom, w, h))
        member __.getPixel(w, h, x, y) =
          let cache = !cache
          match cache.at(x, y) with
          | Someval(v) ->
            rich.palette v * 0.5f + Vec3f(0.5f)
          | Noneval ->
            let v = rich.uncolored (RichMap3.pixmapCamera(w, h, x, y))
            cache.set(x, y, v)
            rich.palette v * 0.5f + Vec3f(0.5f)
        member __.finish() =
          cache.apply(fun cache -> cache.insert())
        member __.postFx(pixmap) =
          ()
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
    let palette = dna.descend("Palette", ColorDna.genPalette 33)
    let info = Map3Info.create(mapGenerator, dna, retainSamples = true, computeSlopes = true)
    {
      RichMap3.uncolored = viewTransform >> info.map
      palette = palette
      center = center
      zoom = zoom
      info = info
    }

