// 3-channel single precision pixel images.
namespace FsMap3

open Common


/// A 3-channel pixel image stored in row-major order with 32-bit floating point components.
/// The components are customarily in the unit range if they are intended to be interpreted as colors.
/// Note that this is different from Map3 functions, where the standard range is [-1, 1].
[<NoEquality; NoComparison>]
type Pixmap =
  {
    a : Vec3f[]
    width : int
    height : int
  }

  member inline this.sizeX = this.width
  member inline this.sizeY = this.height
  member inline this.lastX = this.width - 1
  member inline this.lastY = this.height - 1
  member inline this.offset(x, y) = y * this.width + x
  member inline this.offsetX(x) = x
  member inline this.offsetY(y) = y * this.width

  /// Aspect ratio of the pixmap. Assumes square pixels.
  member inline this.aspectRatio = float this.width / float this.height
  
  member inline this.at(x, y) = this.a.[this.offset(x, y)]
  member inline this.set(x, y, v : Vec3f) = this.a.[this.offset(x, y)] <- v
  member inline this.add(x, y, v : Vec3f) = let i = this.offset(x, y) in this.a.[i] <- this.a.[i] + v
  member inline this.mul(x, y, v : Vec3f) = let i = this.offset(x, y) in this.a.[i] <- this.a.[i] * v
  member inline this.map(x, y, f : Vec3f -> Vec3f) = let i = this.offset(x, y) in this.a.[i] <- (f this.a.[i])
    
  member inline this.Item
    with get (x, y) = this.at(x, y)
    and set (x, y) v = this.set(x, y, v)
    
  member inline this.size = this.a.size
  member inline this.last = this.a.last
  member inline this.at(i) = this.a.[i]
  member inline this.set(i, v : Vec3f) = this.a.[i] <- v
  member inline this.add(i, v : Vec3f) = this.a.[i] <- this.a.[i] + v
  member inline this.mul(i, v : Vec3f) = this.a.[i] <- this.a.[i] * v


  /// Retrieves a pixel value with bicubic filtering. Pixmap height and width must be greater than 1.
  /// Clamps coordinates outside pixmap boundaries.
  member this.atCubic(x : float32, y : float32) =
    assert (this.width > 1 && this.height > 1)
    let ix = clampi 0 (this.sizeX - 2) (int x)
    let iy = clampi 0 (this.sizeY - 2) (int y)
    let dx = clamp01 (x - float32 ix)
    let dy = clamp01 (y - float32 iy)
    let ix0 = this.offsetX(maxi0 (ix - 1))
    let ix1 = this.offsetX(ix)
    let ix2 = this.offsetX(ix + 1)
    let ix3 = this.offsetX(mini this.lastX (ix + 2))
    let iy0 = this.offsetY(maxi0 (iy - 1))
    let iy1 = this.offsetY(iy)
    let iy2 = this.offsetY(iy + 1)
    let iy3 = this.offsetY(mini this.lastY (iy + 2))
    let a = this.a
    let dx3 = Vec3f(dx)
    let py0 = cubicSpline a.[ix0 + iy0] a.[ix1 + iy0] a.[ix2 + iy0] a.[ix3 + iy0] dx3
    let py1 = cubicSpline a.[ix0 + iy1] a.[ix1 + iy1] a.[ix2 + iy1] a.[ix3 + iy1] dx3
    let py2 = cubicSpline a.[ix0 + iy2] a.[ix1 + iy2] a.[ix2 + iy2] a.[ix3 + iy2] dx3
    let py3 = cubicSpline a.[ix0 + iy3] a.[ix1 + iy3] a.[ix2 + iy3] a.[ix3 + iy3] dx3
    cubicSpline py0 py1 py2 py3 (Vec3f dy)


  /// Transforms each pixel with a function.
  member this.modify(f : Vec3f -> Vec3f) = 
    this.a.modify(f)


  /// Transforms each pixel with a function.
  member this.modify(f : int -> int -> Vec3f -> Vec3f) = 
    for y = 0 to this.lastY do
      for x = 0 to this.lastX do
        this.[x, y] <- f x y this.[x, y]


  /// Fills the pixmap with a color.    
  member this.fill(color : Vec3f) =
    this.a.fill(color)


  /// Interprets this pixmap as a Map3 centered inside the XY plane unit square.
  /// Points are retrieved with bicubic interpolation.
  member this.cubicMap3 =
    let Z = float32 (max this.width this.height)
    let cx = 0.5f * float32 this.width
    let cy = 0.5f * float32 this.height
    fun (v : Vec3f) ->
      this.atCubic(cx + (v.x - 0.5f) * Z, cy + (v.y - 0.5f) * Z)


  /// Creates a pixmap.
  static member create(width, height, ?color : Vec3f) = {
    Pixmap.width = width
    height = height
    a = Array.create (width * height) (color >? Vec3f.zero)
  }


  /// Creates a copy of a pixmap.
  static member createCopy(pixmap : Pixmap) = { pixmap with a = Array.copy pixmap.a }


  /// Creates a pixmap, retrieving pixel values from the supplied function.
  static member create(width, height, f : int -> int -> Vec3f) = {
    Pixmap.width = width
    height = height
    a = Array.init (width * height) (fun i -> f (i % width) (i / width))
  }


  /// Copies the contents of another Pixmap here.
  member this.copyFrom(source : Pixmap) =
    enforce (this.width = source.width && this.height = source.height) "Pixmap.copyFrom: Pixmap dimensions must be identical."
    Array.blit source.a 0 this.a 0 this.size


  /// Filters the pixmap with a horizontal kernel, which should have an odd number of entries.
  member this.filterHorizontal(kernel : float32 array) =
    enforce (kernel.size &&& 1 = 1) "Pixmap.filterHorizontal: Kernel must have an odd number of entries."
    let order = kernel.size >>> 1
    let pixmap = Pixmap.create(this.width, this.height)
    for y = 0 to this.lastY do
      for cx = 0 to this.lastX do
        let x0 = max 0 (cx - order)
        let x1 = min this.lastX (cx + order)
        let mutable v = Vec3f.zero
        let mutable W = 0.0f
        for x = x0 to x1 do
          let w = kernel.[order + x - cx]
          v <- v + w * this.[x, y]
          W <- W + w
        pixmap.[cx, y] <- if W > 0.0f then v / W else Vec3f.zero
    pixmap

    
  /// Filters the pixmap with a vertical kernel, which should have an odd number of entries.
  member this.filterVertical(kernel : float32 array) =
    enforce (kernel.size &&& 1 = 1) "Pixmap.filterVertical: Kernel must have an odd number of entries."
    let order = kernel.size >>> 1
    let pixmap = Pixmap.create(this.width, this.height)
    for cy = 0 to this.lastY do
      let y0 = max 0 (cy - order)
      let y1 = min this.lastY (cy + order)
      for x = 0 to this.lastX do
        let mutable v = Vec3f.zero
        let mutable W = 0.0f
        for y = y0 to y1 do
          let w = kernel.[order + y - cy]
          v <- v + w * this.[x, y]
          W <- W + w
        pixmap.[x, cy] <- if W > 0.0f then v / W else Vec3f.zero
    pixmap


  /// Filters the pixmap with Gaussian blur. The radius argument is the radius, in pixels, of one standard deviation.
  member this.gaussianBlur(radius : float) =
    let order = int <| ceil (radius * 3.0)
    let kernel = Array.init (order * 2 + 1) (fun i ->
      let x = float (i - order) / radius
      float32 <| gaussianCdf (x + 0.5 / radius) - gaussianCdf (x - 0.5 / radius)
      )
    this.filterHorizontal(kernel).filterVertical(kernel)


  /// Filters the pixmap with a super-fast exponential blur. 0 < amount < 1, which is the weighting factor
  /// of successive pixels in a moving average.
  member this.exponentialBlur(amount : float32) =
    let weight = Array.create (max this.width this.height) 0.0f

    // Horizontal pass. We create a temporary pixmap to hold the results of the horizontal pass.
    let pixmap = Pixmap.create(this.width, this.height)
    for y = 0 to this.lastY do
      let mutable W = 0.0f
      let mutable v = Vec3f.zero
      for x = 0 to this.lastX do
        v <- v * amount + this.[x, y]
        W <- W * amount + 1.0f
        weight.[x] <- W
        pixmap.[x, y] <- v
      let mutable W = 0.0f
      let mutable v = Vec3f.zero
      for x = this.lastX downto 0 do
        v <- v * amount + this.[x, y]
        W <- W * amount + 1.0f
        pixmap.[x, y] <- (pixmap.[x, y] + v) / (weight.[x] + W)

    // Vertical pass.
    for x = 0 to this.lastX do
      let mutable W = 0.0f
      let mutable v = Vec3f.zero
      for y = 0 to this.lastY do
        v <- v * amount + pixmap.[x, y]
        W <- W * amount + 1.0f
        weight.[y] <- W
        this.[x, y] <- v
      let mutable W = 0.0f
      let mutable v = Vec3f.zero
      for y = this.lastY downto 0 do
        v <- v * amount + pixmap.[x, y]
        W <- W * amount + 1.0f
        this.[x, y] <- (this.[x, y] + v) / (weight.[y] + W)
    

  override this.ToString() = sprintf "Pixmap(%d x %d)" this.width this.height



/// Source of pixel data. This can be anything from a procedural texture to a ray tracer.
type IPixmapSource =

  /// This is called whenever we start rendering an image. Arguments are: width, height.
  abstract start : int * int -> unit

  /// Retrieves a pixel. This can be called in parallel. Arguments are: width, height, x, y.
  abstract getPixel : int * int * int * int -> Vec3f

  /// Signals the end of the parallel phase. This call is optional.
  abstract finish : unit -> unit

  /// Applies synchronous post-processing effects. In post-processing the pixels can have dependencies.
  abstract postFx : Pixmap -> unit



/// Utility IPixmapSource stuff.
type PixmapSource() =

  static let zeroSource = {
    new IPixmapSource with
      member this.start(_, _) = ()
      member this.getPixel(_, _, _, _) = Vec3f.zero
      member this.finish() = ()
      member this.postFx(_) = ()
    }

  static member zero = zeroSource

  static member toPixmap(source : IPixmapSource, width, height, ?rowReadyCallback : int -> unit) =
    let callback = rowReadyCallback >? ignore
    source.start(width, height)
    let pixmap = Pixmap.create(width, height)
    let renderRow y =
      for x = 0 to width - 1 do
        pixmap.[x, y] <- source.getPixel(width, height, x, y)
      callback y
    Array.Parallel.iter renderRow [| 0 .. height - 1 |]
    source.finish()
    source.postFx(pixmap)
    pixmap


