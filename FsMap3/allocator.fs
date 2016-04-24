// Allocators.
namespace FsMap3

open Common


/// Thread local allocator. Note that the initializer must be re-entrant.
[<NoComparison; NoEquality>]
type ThreadLocalAllocator<'a> =
  {
    pool : System.Threading.ThreadLocal<'a Darray>
    allocatef : ThreadLocalAllocator<'a> -> 'a
    resetf : ThreadLocalAllocator<'a> -> 'a -> unit
    maximumSize : int
  }

  /// Allocates an object.
  member this.allocate() =
    let pool = !this.pool
    if pool.size > 0 then
      pool.pull()
    else 
      this.allocatef this

  /// Releases an object.
  member this.release(a : 'a) =
    this.resetf this a
    let pool = !this.pool
    if pool.size < this.maximumSize then pool.push(a)

  /// Number of objects currently in the pool.
  member this.size = (!this.pool).size

  /// Resets the pool, emptying it.
  member this.reset() =
    (!this.pool).reset()

  /// Creates an allocator pool. The default maximum size is unlimited.
  static member create(allocatef, resetf, ?maximumSize) : ThreadLocalAllocator<'a> =
    {
      pool = new System.Threading.ThreadLocal<_>(fun _ -> Darray.create(autoTrim = false))
      allocatef = allocatef
      resetf = resetf
      maximumSize = maximumSize >? maxValue int
    }
