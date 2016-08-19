/// Foundations for procedural 3-textures. Cell topologies and layouts.
module FsMap3.Basis3

open Common
open Mangle


/// Basis functions accept octave number and frequency arguments. Invoking a basis
/// triggers the creation of a map for that particular octave and frequency. For that reason,
/// in a 3-texture, all basis functions should be instanced before use.
type Basis3 = int -> float32 -> Vec3f -> Vec3f


/// BasisData contains all kinds of information used during basis function calculations.
/// We employ thread-local allocators to minimize allocation of these objects.
[<NoComparison; NoEquality>]
type BasisData =
  {
    // Pixel information. px, py in [0, pw[ and [0, ph[, respectively.
    mutable px : int
    mutable py : int
    // Picture width and height.
    mutable pw : int
    mutable ph : int
    // Screen space coordinates in [0, 1[.
    mutable sx : float32
    mutable sy : float32

    /// Cell hash seed.
    mutable seed : int
    // Cell coordinates.
    mutable ix : int
    mutable iy : int
    mutable iz : int
    /// Intra-cell position with each component in [0, 1[.
    mutable d : Vec3f
    /// Integer frequencies. These are set to large numbers for non-tiling axes.
    mutable fix : int
    mutable fiy : int
    mutable fiz : int

    // Closest cell color for the colored Worley basis.
    mutable worleyColor : Vec3f

    // Closest point distances for the Worley basis.
    mutable d0 : float32
    mutable d1 : float32
    mutable d2 : float32

    // Current scan box. Also current search neighborhood for the Worley basis.
    mutable x0 : int
    mutable x1 : int
    mutable y0 : int
    mutable y1 : int
    mutable z0 : int
    mutable z1 : int

    /// Worley basis axis expansion order.
    search : Pair<float32, int> array
    /// Current closest cell weight for the colored Worley basis.
    mutable worleyWeight : float32
    /// Allocator where this object should be released after use.
    mutable allocator : Allocator<BasisData>
  }

  /// X coordinate of the cell at offset jx.
  member inline this.x(jx) = emod (this.ix + jx) this.fix

  /// Y coordinate of the cell at offset jy.
  member inline this.y(jy) = emod (this.iy + jy) this.fiy

  /// Z coordinate of the cell at offset jz.
  member inline this.z(jz) = emod (this.iz + jz) this.fiz

  /// Normalizes a modified cell X coordinate.
  member inline this.wrapX(ix) = emod ix this.fix

  /// Normalizes a modified cell Y coordinate.
  member inline this.wrapY(iy) = emod iy this.fiy

  /// Normalizes a modified cell Z coordinate.
  member inline this.wrapZ(iz) = emod iz this.fiz

  /// Sets the X axis to tile with integer frequency fx and sets the cell coordinate to ix.
  member inline this.tileX(fx, ix) = this.fix <- fx; this.ix <- this.wrapX(ix)

  /// Sets the Y axis to tile with integer frequency fy and sets the cell coordinate to iy.
  member inline this.tileY(fy, iy) = this.fiy <- fy; this.iy <- this.wrapY(iy)

  /// Sets the Z axis to tile with integer frequency fz and sets the cell coordinate to iz.
  member inline this.tileZ(fz, iz) = this.fiz <- fz; this.iz <- this.wrapZ(iz)

  /// Makes the X axis non-tiling and sets the cell coordinate to ix.
  member inline this.noTileX(ix) = this.fix <- 0x40000000; this.ix <- ix &&& 0x3fffffff

  /// Makes the Y axis non-tiling and sets the cell coordinate to iy.
  member inline this.noTileY(iy) = this.fiy <- 0x40000000; this.iy <- iy &&& 0x3fffffff

  /// Makes the Z axis non-tiling and sets the cell coordinate to iz.
  member inline this.noTileZ(iz) = this.fiz <- 0x40000000; this.iz <- iz &&& 0x3fffffff

  /// Hashes a cell. The coordinates are integer offsets from the center cell.
  member this.hash(jx, jy, jz) =
    let x = this.x(jx)
    let y = this.y(jy)
    let z = this.z(jz)
    this.seed + mangle32 (z ^^^ (x &&& 0xffff) ^^^ (mangle32fast ((x <<< 16) ^^^ y) ) )

  /// Hashes an X axis offset. The second argument can be used to combine axial hashes.
  member this.hashX(jx, h) =
    let x = this.x(jx)
    this.seed + mangle32fast (x ^^^ h)

  /// Hashes an X axis offset.
  member inline this.hashX(jx) = this.hashX(jx, 0)

  /// Hashes a Y axis offset. The second argument can be used to combine axial hashes.
  member this.hashY(jy, h) =
    let y = this.y(jy)
    this.seed + mangle32fast (y ^^^ h)

  /// Hashes a Y axis offset.
  member inline this.hashY(jy) = this.hashY(jy, 0)

  /// Hashes a Z axis offset. The second argument can be used to combine axial hashes.
  member this.hashZ(jz, h) =
    // This is Mangle.mangle32c inlined.
    let u = this.z(jz) + h |> uint
    let u = u ^^^ (u >>> 16)
    let u = u * 0x85ebca6bu
    let u = u ^^^ (u >>> 13)
    let u = u * 0xc2b2ae35u
    let u = u ^^^ (u >>> 16)
    int u

  /// Hashes a Z axis offset.
  member inline this.hashZ(jz) = this.hashZ(jz, 0)

  /// Computes the scan box in relative coordinates, given maximum feature radius.
  member this.scan(radius) =
    this.x0 <- int <| floor (this.d.x - radius)
    this.x1 <- int <| floor (this.d.x + radius)
    this.y0 <- int <| floor (this.d.y - radius)
    this.y1 <- int <| floor (this.d.y + radius)
    this.z0 <- int <| floor (this.d.z - radius)
    this.z1 <- int <| floor (this.d.z + radius)

  /// Computes the scan box in relative coordinates, given maximum feature radii
  /// in each direction.
  member this.scan(rxn, rxp, ryn, ryp, rzn, rzp) =
    this.x0 <- int <| floor (this.d.x - rxn)
    this.x1 <- int <| floor (this.d.x + rxp)
    this.y0 <- int <| floor (this.d.y - ryn)
    this.y1 <- int <| floor (this.d.y + ryp)
    this.z0 <- int <| floor (this.d.z - rzn)
    this.z1 <- int <| floor (this.d.z + rzp)

  /// Computes the scan box in relative coordinates, given maximum feature radius
  /// and minimum feature center point distance from cell border.
  member inline this.scan(radius, border) = this.scan(radius - border)

  member this.storeAxisOrder(xd0, yd0, zd0) =
    let search = this.search
    search.[0] <- Pair(xd0, 0)
    search.[1] <- Pair(yd0, 1)
    search.[2] <- Pair(zd0, 2)
    Fun.insertionSort 0 2 (fun i -> search.[i].x) (fun i j -> search.swap(i, j))

  member inline this.release() =
    this.allocator.release(this)

  static member create(allocator) =
    {
      px = 0; py = 0
      pw = 0; ph = 0
      sx = 0.0f; sy = 0.0f
      seed = 0
      ix = 0; iy = 0; iz = 0
      d = Vec3f.zero
      fix = 0; fiy = 0; fiz = 0
      worleyColor = Vec3f.zero
      d0 = 0.0f
      d1 = 0.0f
      d2 = 0.0f
      x0 = 0; x1 = 0
      y0 = 0; y1 = 0
      z0 = 0; z1 = 0
      search = Array.zeroCreate 3
      worleyWeight = 0.0f
      allocator = allocator
    }


let threadLocalBasisData = new System.Threading.ThreadLocal<Allocator<BasisData>>(fun _ ->
                             Allocator.create(BasisData.create, fun allocator data -> data.allocator <- allocator))


/// Layout-specific data for an instanced basis. Instancing a basis fixes the frequency and the layout.
[<NoComparison; NoEquality>]
type LayoutInstance =
  {
    f : float32
    seed : int
    fix : int
    fiy : int
    fiz : int
    offset : Vec3f
    rotation : Quaternionf
    runf : LayoutInstance -> Vec3f -> BasisData
  }
  member this.run(v : Vec3f) = this.runf this v


(*
Given a cell hash seed, an octave number, a frequency and a 3-point, a basis layout makes available:

-coordinates of the 3-cell where the point is located.
-fractional position inside the 3-cell.
-hash value and coordinates of any other 3-cell.
*)
type LayoutFunction = int -> int -> float32 -> LayoutInstance


/// Standard, high quality layout. Each instance has a distinct cell grid offset and rotation.
let hifiLayout seed octave (f : float32) =
  let a = mangle64 (int64 seed)
  let b = mangle64 a
  {
    LayoutInstance.f = f
    seed = seed + octave
    fix = 0
    fiy = 0
    fiz = 0
    offset = Vec3f.fromSeed(int a)
    rotation = Convert.unitQuaternion (int (a >>> 32)) (int b) (int (b >>> 32))
    runf = fun instance v ->
      let data = (!threadLocalBasisData).allocate()
      let v = instance.f * (v * instance.rotation) + instance.offset
      let vi = v.map(floor)
      data.seed <- instance.seed
      data.d <- v - vi
      data.noTileX(int vi.x)
      data.noTileY(int vi.y)
      data.noTileZ(int vi.z)
      data
  }


/// Constant orientation layout. Each frequency has a distinct cell grid offset.
let offsetLayout seed octave (f : float32) =
  {
    LayoutInstance.f = f
    seed = seed + octave
    fix = 0
    fiy = 0
    fiz = 0
    offset = Vec3f.fromSeed(mangle32 seed)
    rotation = Quaternionf.zero
    runf = fun instance v ->
      let data = (!threadLocalBasisData).allocate()
      let v = instance.f * v + instance.offset
      let vi = v.map(floor)
      data.seed <- instance.seed
      data.d <- v - vi
      data.noTileX(int vi.x)
      data.noTileY(int vi.y)
      data.noTileZ(int vi.z)
      data
  }


/// Layout that does no processing of the coordinates.
let passLayout seed octave (f : float32) =
  {
    LayoutInstance.f = f
    seed = seed + octave
    fix = 0
    fiy = 0
    fiz = 0
    offset = Vec3f.zero
    rotation = Quaternionf.zero
    runf = fun instance v ->
      let data = (!threadLocalBasisData).allocate()
      let v = instance.f * v
      let vi = v.map(floor)
      data.seed <- instance.seed
      data.d <- v - vi
      data.noTileX(int vi.x)
      data.noTileY(int vi.y)
      data.noTileZ(int vi.z)
      data
  }


/// A tiling layout. This layout tiles 3-space with copies of the unit cube.
/// The frequency is rounded to an integer to tile correctly. The full frequency is still used to seed
/// the frequency hash, so that every distinct frequency has a unique appearance and cell grid offset.
let tileLayout seed octave (f : float32) =
  let fi = maxi 1 (int (round f))
  {
    LayoutInstance.f = float32 fi
    seed = seed + octave
    fix = fi
    fiy = fi
    fiz = fi
    offset = Vec3f.fromSeed(mangle32 seed)
    rotation = Quaternionf.zero
    runf = fun instance v ->
      let data = (!threadLocalBasisData).allocate()
      let v = instance.f * v + instance.offset
      let vi = v.map(floor)
      data.seed <- instance.seed
      data.d <- v - vi
      data.tileX(instance.fix, int vi.x)
      data.tileY(instance.fiy, int vi.y)
      data.tileZ(instance.fiz, int vi.z)
      data
  }


/// This layout tiles the X dimension only by repeating the unit interval.
/// The X frequency is rounded to the nearest integer to tile correctly.
/// Each frequency has a distinct cell grid offset.
let tileXLayout seed octave (f : float32) =
  let fi = maxi 1 (int (round f))
  {
    LayoutInstance.f = float32 fi
    seed = seed + octave
    fix = fi
    fiy = 0
    fiz = 0
    offset = Vec3f.fromSeed(mangle32 seed)
    rotation = Quaternionf.zero
    runf = fun instance v ->
      let data = (!threadLocalBasisData).allocate()
      // We round the frequency on all axes to maintain aspect ratio.
      let v = instance.f * v + instance.offset
      let vi = v.map(floor)
      data.seed <- instance.seed
      data.d <- v - vi
      data.tileX(instance.fix, int vi.x)
      data.noTileY(int vi.y)
      data.noTileZ(int vi.z)
      data
  }


/// This layout tiles the Y dimension only by repeating the unit interval.
/// The Y frequency is rounded to the nearest integer to tile correctly.
/// Each frequency has a distinct cell grid offset.
let tileYLayout seed octave (f : float32) =
  let fi = maxi 1 (int (round f))
  {
    LayoutInstance.f = float32 fi
    seed = seed + octave
    fix = 0
    fiy = fi
    fiz = 0
    offset = Vec3f.fromSeed(mangle32 seed)
    rotation = Quaternionf.zero
    runf = fun instance v ->
      let data = (!threadLocalBasisData).allocate()
      // We round the frequency on all axes to maintain aspect ratio.
      let v = instance.f * v + instance.offset
      let vi = v.map(floor)
      data.seed <- instance.seed
      data.d <- v - vi
      data.noTileX(int vi.x)
      data.tileY(instance.fiy, int vi.y)
      data.noTileZ(int vi.z)
      data
  }


/// This layout tiles the Z dimension only by repeating the unit interval.
/// The Z frequency is rounded to the nearest integer to tile correctly.
/// Each frequency has a distinct cell grid offset.
let tileZLayout seed octave (f : float32) =
  let fi = maxi 1 (int (round f))
  {
    LayoutInstance.f = float32 fi
    seed = seed + octave
    fix = 0
    fiy = 0
    fiz = fi
    offset = Vec3f.fromSeed(mangle32 seed)
    rotation = Quaternionf.zero
    runf = fun instance v ->
      let data = (!threadLocalBasisData).allocate()
      // We round the frequency on all axes to maintain aspect ratio.
      let v = instance.f * v + instance.offset
      let vi = v.map(floor)
      data.seed <- instance.seed
      data.d <- v - vi
      data.noTileX(int vi.x)
      data.noTileY(int vi.y)
      data.tileZ(instance.fiz, int vi.z)
      data
  }


type Layout =
  Hifi | Offset | Pass | Tile | TileX | TileY | TileZ


let layoutFunction = function
  | Hifi -> hifiLayout
  | Offset -> offsetLayout
  | Pass -> passLayout
  | Tile -> tileLayout
  | TileX -> tileXLayout
  | TileY -> tileYLayout
  | TileZ -> tileZLayout

