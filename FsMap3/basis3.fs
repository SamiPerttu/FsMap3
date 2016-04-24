/// Foundations for procedural 3-textures. Cell topologies and layouts.
module FsMap3.Basis3

open Common
open Mangle


/// Basis functions are 3-textures that accept a frequency argument.
type Basis3 = float32 -> Vec3f -> Vec3f


/// BasisData contains all kinds of information used during basis function calculations.
/// We employ thread-local storage to minimize allocation of these objects.
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

    /// Frequency hash.
    mutable fh : int
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

    // Closest point distances and hashes for the Worley basis.
    mutable d0 : float32
    mutable h0 : int
    mutable d1 : float32
    mutable h1 : int
    mutable d2 : float32
    mutable h2 : int

    // Current scan box. Also current search neighborhood for the Worley basis.
    mutable x0 : int
    mutable x1 : int
    mutable y0 : int
    mutable y1 : int
    mutable z0 : int
    mutable z1 : int

    // Worley basis axis expansion order.
    search : Pair<float32, int> array
    // Current closest cell weight for the colored Worley basis.
    mutable worleyWeight : float32

    mutable allocator : ThreadLocalAllocator<BasisData>
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
    this.fh + mangle32 (z ^^^ (x &&& 0xffff) ^^^ (mangle32fast ((x <<< 16) ^^^ y) ) )

  /// Hashes an X axis offset. The second argument can be used to combine axial hashes.
  member this.hashX(jx, h) =
    let x = this.x(jx)
    this.fh + mangle32 (x ^^^ h)

  /// Hashes an X axis offset.
  member inline this.hashX(jx) = this.hashX(jx, 0)

  /// Hashes a Y axis offset. The second argument can be used to combine axial hashes.
  member this.hashY(jy, h) =
    let y = this.y(jy)
    this.fh + mangle32fast (y ^^^ h)

  /// Hashes a Y axis offset.
  member inline this.hashY(jy) = this.hashY(jy, 0)

  /// Hashes a Z axis offset. The second argument can be used to combine axial hashes.
  member inline this.hashZ(jz, h) =
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

  member inline this.release() = this.allocator.release(this)

  static member create(allocator) =
    {
      px = 0; py = 0
      pw = 0; ph = 0
      sx = 0.0f; sy = 0.0f
      fh = 0
      ix = 0; iy = 0; iz = 0
      d = Vec3f.zero
      fix = 0; fiy = 0; fiz = 0
      worleyColor = Vec3f.zero
      d0 = 0.0f; h0 = 0
      d1 = 0.0f; h1 = 0
      d2 = 0.0f; h2 = 0
      x0 = 0; x1 = 0
      y0 = 0; y1 = 0
      z0 = 0; z1 = 0
      search = Array.zeroCreate 3
      worleyWeight = 0.0f
      allocator = allocator
    }


let threadLocalBasisData = ThreadLocalAllocator<BasisData>.create(BasisData.create, fun allocator data -> data.allocator <- allocator)


(*
Given frequency and 3-point, a basis layout makes available:

-hash of the frequency.
-coordinates of the 3-cell where the point is located.
-fractional position inside the 3-cell.
-hash value and coordinates of any other 3-cell.
*)
type LayoutFunction = float32 -> Vec3f -> BasisData



/// Standard, high-quality layout. Each frequency has a distinct cell grid offset
/// and rotation.
let hifiLayout (f : float32) (v : Vec3f) =
  let data = threadLocalBasisData.allocate()
  let fh = manglef64 f
  let a = mangle64 fh
  let v = v * mangle12Rotation (int a)
  let v = f * v + Vec3f.fromSeed(int (fh >>> 32))
  let vf = v.map(floor)
  data.fh <- int fh
  data.d <- v - vf
  data.noTileX(int vf.x)
  data.noTileY(int vf.y)
  data.noTileZ(int vf.z)
  data


/// Constant orientation layout. Each frequency has a distinct cell grid offset.
let offsetLayout (f : float32) (v : Vec3f) =
  let data = threadLocalBasisData.allocate()
  let fh = manglef64 f
  let v = f * v + Vec3f.fromSeed(int (fh >>> 32))
  let vf = v.map(floor)
  data.fh <- int fh
  data.d <- v - vf
  data.noTileX(int vf.x)
  data.noTileY(int vf.y)
  data.noTileZ(int vf.z)
  data


/// Layout that passes the parameters through as is.
let passLayout (f : float32) (v : Vec3f) =
  let data = threadLocalBasisData.allocate()
  let fh = manglef64 f
  let v = f * v
  let vf = v.map(floor)
  data.fh <- int fh
  data.d <- v - vf
  data.noTileX(int vf.x)
  data.noTileY(int vf.y)
  data.noTileZ(int vf.z)
  data


/// A tiling layout. This layout tiles 3-space with copies of the unit cube.
/// The frequency is truncated to an integer to tile correctly. The full frequency is still used to seed
/// the frequency hash, so that every distinct frequency has a unique appearance and cell grid offset.
let tileLayout (f : float32) (v : Vec3f) =
  let data = threadLocalBasisData.allocate()
  let fi = maxi 1 (int (round f))
  let fh = manglef64 f
  let v = float32 fi * (v + Vec3f.fromSeed(int (fh >>> 32)))
  let vf = v.map(floor)
  data.fh <- int fh
  data.d <- v - vf
  data.tileX(fi, int vf.x)
  data.tileY(fi, int vf.y)
  data.tileZ(fi, int vf.z)
  data


/// This layout tiles the X dimension only by repeating the unit interval.
/// The X frequency is rounded to the nearest integer to tile correctly. Each frequency
/// has a distinct cell grid offset.
let tileXLayout (f : float32) (v : Vec3f) =
  let data = threadLocalBasisData.allocate()
  let fx = maxi 1 (int (round f))
  let fh = manglef64 f
  let v = Vec3f(float32 fx, f, f) * (v + Vec3f.fromSeed(int (fh >>> 32)))
  let vf = v.map(floor)
  data.fh <- int fh
  data.d <- v - vf
  data.tileX(fx, int vf.x)
  data.noTileY(int vf.y)
  data.noTileZ(int vf.z)
  data


/// This layout tiles the Y dimension only by repeating the unit interval.
/// The Y frequency is rounded to the nearest integer to tile correctly. Each frequency
/// has a distinct cell grid offset.
let tileYLayout (f : float32) (v : Vec3f) =
  let data = threadLocalBasisData.allocate()
  let fy = maxi 1 (int (round f))
  let fh = manglef64 f
  let v = Vec3f(f, float32 fy, f) * (v + Vec3f.fromSeed(int (fh >>> 32)))
  let vf = v.map(floor)
  data.fh <- int fh
  data.d <- v - vf
  data.noTileX(int vf.x)
  data.tileY(fy, int vf.y)
  data.noTileZ(int vf.z)
  data


/// This layout tiles the Z dimension only by repeating the unit interval.
/// The Z frequency is rounded to the nearest integer to tile correctly. Each frequency
/// has a distinct cell grid offset.
let tileZLayout (f : float32) (v : Vec3f) =
  let data = threadLocalBasisData.allocate()
  let fz = maxi 1 (int (round f))
  let fh = manglef64 f
  let v = Vec3f(f, f, float32 fz) * (v + Vec3f.fromSeed(int (fh >>> 32)))
  let vf = v.map(floor)
  data.fh <- int fh
  data.d <- v - vf
  data.noTileX(int vf.x)
  data.noTileY(int vf.y)
  data.tileZ(fz, int vf.z)
  data



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

