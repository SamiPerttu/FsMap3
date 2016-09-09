module Fuse.Map3Cache

open Common
open Mangle


(*
Purpose of caching in the editor is:
-Accelerate palette editing.
-Accelerate panning.
-Accelerate undo/redo.
-In the future, accelerate channel editing.
*)


/// A cached XY pixmap of a Map3.
[<NoEquality; NoComparison>]
type Map3Cache =
  {
    /// Fingerprint of the slice raster, which includes the Z coordinate and zoom but not the X and Y coordinates.
    rasterprint : int64
    center : Vec2f
    /// The pixmap. An X component of infinity signifies an uncomputed pixel.
    pixmap : Pixmap
    /// Pixmaps become read-only when they are placed in the global cache.
    mutable readOnly : bool
    /// Number of cached pixels. This is not updated in parallel but is computed after the query phase.
    mutable cachedPixels : int
  }

  member inline this.width = this.pixmap.width

  member inline this.height = this.pixmap.height

  member inline this.isComplete = this.cachedPixels = this.pixmap.size

  member inline this.isCached(x, y) = this.pixmap.[x, y].x <> infinityf
  member inline this.isCached(i) = this.pixmap.at(i).x <> infinityf

  member this.updateCachedPixels() =
    this.cachedPixels <- Fun.count 0 this.pixmap.last this.isCached

  member inline this.at(x, y) =
    let v = this.pixmap.[x, y]
    if v.x <> infinityf then Someval(v) else Noneval

  member inline this.set(x, y, v) =
    this.pixmap.[x, y] <- v

  /// Raster density is the number of pixels per unit. Pixels are always square.
  static member density(zoom : float32, width : int, height : int) =
    zoom * float32 (max width height)

  static member getRasterprint(fingerprint, z : float32, zoom : float32, width : int, height : int) =
    fingerprint + manglef64 (Map3Cache.density(zoom, width, height)) |> mangle64 |> (+) (manglef64 z)

  static member create(fingerprint, center : Vec3f, zoom, width, height) : Map3Cache =
    {
      rasterprint  = Map3Cache.getRasterprint(fingerprint, center.z, zoom, width, height)
      center       = Vec2f(center.x, center.y)
      pixmap       = Pixmap.create(width, height, Vec3f(infinityf))
      readOnly     = false
      cachedPixels = 0
    }


/// Cached maps. The global cache is accessed synchronously.
let globalMap3Cache = Darray<Map3Cache>.create()



type AxisMatch =
  { start1 : int; start0 : int; size : int }
  static member Null = { start1 = 0; start0 = 0; size = 0 }


/// Matches maps on an axis.
let matchAxis x0 w0 x1 w1 density =
  let o1 = (x1 - x0) * density
  let i1 = int (round o1)
  let subPixelTolerance = 0.1f
  if abs (o1 - float32 i1) > subPixelTolerance || i1 >= w0 || i1 + w1 <= 0 then
    AxisMatch.Null
  else
    let start1 = max 0 -i1
    let start0 = max 0 i1
    let end0   = min w0 (i1 + w1)
    { start1 = start1; start0 = start0; size = end0 - start0 }


type Map3Cache with
  
  /// Fills a map with information from the global cache or creates a new one.
  static member fill(fingerprint, center : Vec3f, zoom, width, height) =
    /// Try to locate a matching map from the cache.
    let density = Map3Cache.density(zoom, width, height)
    let rasterprint = Map3Cache.getRasterprint(fingerprint, center.z, zoom, width, height)
    let cache = globalMap3Cache
    let mutable bestXmatch = AxisMatch.Null
    let mutable bestYmatch = AxisMatch.Null
    let mutable bestPixels = 0
    let existing = lock cache (fun _ ->
      let mutable best = 0
      for i = 0 to cache.last do
        if cache.[i].rasterprint = rasterprint then
          let xMatch = matchAxis center.x width cache.[i].center.x cache.[i].width density
          let yMatch = matchAxis center.y height cache.[i].center.y cache.[i].height density
          let pixels = xMatch.size * yMatch.size
          if pixels > bestPixels then
            bestPixels <- pixels
            bestXmatch <- xMatch
            bestYmatch <- yMatch
            best       <- i
      if bestPixels > 0 then
        let existing = cache.[best]
        cache.remove(best)
        cache.add(existing)
        Someval(existing)
      else
        Noneval
      )
    match existing with
    | Noneval ->
      Log.infof "Cache %16x: Not found." fingerprint
      Map3Cache.create(fingerprint, center, zoom, width, height)
    | Someval(existing) ->
      if existing.isComplete && bestPixels = width * height then
        Log.infof "Cache %16x: Complete match found." rasterprint
        existing
      else
        Log.infof "Cache %16x: Incomplete match found with %d/%d pixels." rasterprint bestPixels (width * height)
        let this = Map3Cache.create(fingerprint, center, zoom, width, height)
        for y = 0 to bestYmatch.size - 1 do
          for x = 0 to bestXmatch.size - 1 do
            match existing.at(x + bestXmatch.start1, y + bestYmatch.start1) with
            | Someval(v) -> this.pixmap.[x + bestXmatch.start0, y + bestYmatch.start0] <- v
            | _ -> ()
        this


  /// Places this map in the cache.
  member this.insert() =
    let cache = globalMap3Cache

    if this.isComplete = false && this.readOnly = false then this.updateCachedPixels()

    lock cache (fun _ ->
      // If a matching map is in the cache, replace it.
      match Fun.findArg 0 cache.last (fun i -> cache.[i].rasterprint = this.rasterprint) with
      | Someval(i) ->
        Log.infof "Cache %16x: %s" this.rasterprint (if cache.[i] === this then "Refreshing." else "Replacing previous.")
        cache.remove(i)
      | _ ->
        Log.infof "Cache %16x: Inserting new." this.rasterprint
        ()
      // If the cache is growing too big, remove the least recently accessed item.
      if cache.size > 100 then
        Log.infof "Cache %16x: Discarding." cache.[0].rasterprint
        cache.remove(0)
      this.readOnly <- true
      cache.add(this)
      )

