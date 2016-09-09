// Rectangular grids of items.
namespace Fuse

open Common


/// Generic rectangular grid of items stored as a flat array in row-major order.
[<NoComparison; NoEquality>]
type Grid<'a> =
  {
    a : 'a[]
    sizeX : int
    sizeY : int
  }

  /// Index of item at (x, y).
  member inline this.index(x, y) = y * this.sizeX + x

  /// Total number of items in the grid.
  member inline this.size = this.sizeX + this.sizeY

  /// Last X coordinate in the grid.
  member inline this.lastX = this.sizeX - 1

  /// Last Y coordinate in the grid.
  member inline this.lastY = this.sizeY - 1

  /// Item access.
  member inline this.Item
    with get (x, y) = this.a.[this.index(x, y)]
    and set (x, y) item = this.a.[this.index(x, y)] <- item

  /// Accesses an item.
  member inline this.at(x, y) = this.[x, y]

  /// Sets an item.
  member inline this.set(x, y, item) = this.[x, y] <- item

  /// Fills the grid with a value.
  member this.fill(item : 'a) = this.a.fill(item)

  member this.iter(f : int -> int -> 'a -> unit) =
    for y = 0 to this.lastY do
      for x = 0 to this.lastX do f x y this.[x, y]

  /// Alias for sizeX.
  member inline this.width = this.sizeX

  /// Alias for sizeY.
  member inline this.height = this.sizeY

  static member create(sizeX, sizeY) : Grid<'a> = {
    a = Array.zeroCreate (sizeX * sizeY)
    sizeX = sizeX
    sizeY = sizeY
  }

  static member create(sizeX, sizeY, value : 'a) = {
    a = Array.create (sizeX * sizeY) value
    sizeX = sizeX
    sizeY = sizeY
  }

  static member createFrom(sizeX, sizeY, f : int -> int -> 'a) = {
    a = Array.init (sizeX * sizeY) (fun i -> f (i % sizeX) (i / sizeX))
    sizeX = sizeX
    sizeY = sizeY
  }

