// Weighted sets.
namespace FsMap3

open Common


/// A weighted set of elements.
/// Supports weight modification and proportional sampling in O(log N) time.
/// Keeps the weights in a hierarchy with w(i) = w(2i) + w(2i + 1).
type Wset<'a>() =

  // Note that the Cset module contains a generalization of this type.

  let mutable wa = Array.zeroCreate<float> 2 // weight array; always power of two size; first element is unused
  let mutable ea = Darray<'a>.create() // element array

  /// The number of elements in the set.
  member this.size = ea.size

  /// The index of the last element in the set.
  member this.last = ea.last

  /// The total weight of the set.
  member this.total = wa.[1]

  /// Element accessor.
  member this.at
    with get(i) = ea.[i]
    and set(i) e = ea.[i] <- e

  /// Weight accessor.
  member this.weight
    with get(i) = wa.[i + (wa.size >>> 1)]
    and set(i) w =
      assert (w >= 0.0)
      let mutable j = i + (wa.size >>> 1)
      let d = w - wa.[j]
      // Update all the relevant weights in the hierarchy.
      while j > 0 do
        wa.[j] <- wa.[j] + d
        j <- j >>> 1

  /// Allocates space for at least n elements.
  member this.allocate(n) =
    let capacity = wa.size >>> 1
    if capacity < n then
      let capacity' = int <| Bits.ceilPower2 (uint32 n)
      ea.allocate(capacity')
      let wa' = Array.zeroCreate (capacity' * 2)
      // Copy element weights.
      for i = 0 to this.last do wa'.[capacity' + i] <- this.weight(i)
      // Compute hierarchical weights.
      for i = capacity' - 1 downto 1 do wa'.[i] <- wa'.[i * 2] + wa'.[i * 2 + 1]
      wa <- wa'

  /// Adds an element to the set.
  member this.add(w, e) =
    assert (w >= 0.0)
    this.allocate(ea.size + 1)
    this.weight(this.size) <- w
    ea.add(e)

  /// Picks an element proportionally from the set. Returns its index. The argument is in the range [0, 1[.
  member this.pickIndex(x : float) =
    enforce (this.size > 0) "Wset.pickIndex: The set is empty."
    let mutable x = x * this.total
    let mutable i = 1
    let elementOffset = wa.size >>> 1
    // Find the element by descending the hierarchy of weights.
    while i < elementOffset do
      let i2 = i <<< 1
      if x < wa.[i2] then
        i <- i2
      else
        x <- x - wa.[i2]
        i <- i2 + 1
    // If argument x was very close to 1, we may have picked an element outside the set, so make sure it does not happen.
    min this.last (i - elementOffset)

  /// Picks an element proportionally from the set. The argument is in the range [0, 1[.
  member this.pick(x : float) = ea.[this.pickIndex(x)]

  /// Picks an element proportionally from the set while excluding a single element.
  /// Returns the index of the picked element. The argument x is in the range [0, 1[.
  member this.pickIndexExcluding(exclude : int, x : float) =
    enforce (this.size > 0) "Wset.pickIndexExcluding: The set is empty."
    let ew = this.weight(exclude)
    enforce (ew < this.total) "Wset.pickIndexExcluding: No non-zero elements found."
    let elementLevel = Bits.bitNumber32 wa.size - 1
    let elementOffset = wa.size >>> 1
    let excludeIndex = elementOffset + exclude
    let mutable x = x * (this.total - ew)
    let inline weightAt i level = if i = (excludeIndex >>> (elementLevel - level)) then wa.[i] - ew else wa.[i]
    let mutable i = 1
    for level = 1 to elementLevel do
      let i2 = i <<< 1
      let w = weightAt i2 level
      if x < w then
        i <- i2
      else
        x <- x - w
        i <- i2 + 1
    min this.last (i - elementOffset)

  /// Picks an element proportionally from the set while excluding a single element. The argument is in the range [0, 1[.
  member this.pickExcluding(exclude : int, x : float) = ea.[this.pickIndexExcluding(exclude, x)]


