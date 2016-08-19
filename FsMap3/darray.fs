// Dynamic arrays.
namespace FsMap3

open Common


type DarrayTrim =
  static member inline mask(autoTrim) = if autoTrim then ~~~0 else 0



/// Dynamic array with amortized constant time insertion and removal from the end of the array.
type [<NoEquality; NoComparison>] Darray<'a> =
  {
    mutable a : 'a[]
    /// Logical size of the array.
    mutable n : int
    /// This mask is all ones if auto-trim is enabled, and all zeros if auto-trim is disabled.
    mutable autoTrimMask : int
  }

  /// The current capacity of the dynamic array.
  member inline this.capacity = this.a.size

  /// The number of items in the dynamic array.
  member inline this.size = this.n

  /// Index of the last item in the array.
  member inline this.last = this.n - 1

  /// Item access.
  member inline this.Item with get i = this.a.[i] and set i x = this.a.[i] <- x

  /// Reads an item.
  member inline this.at(i) = this.a.[i]

  /// Sets an item.
  member inline this.set(i, x) = this.a.[i] <- x

  /// The first item in the array.
  member inline this.firstItem
    with get() = 
      assert (this.size > 0)
      this.a.[0]
    and set(x) =
      assert (this.size > 0)
      this.a.[0] <- x

  /// The last item in the array.
  member inline this.lastItem
    with get() = 
      assert (this.size > 0)
      this.a.[this.last]
    and set(x) =
      assert (this.size > 0)
      this.a.[this.last] <- x

  /// Returns whether the array is empty.
  member inline this.isEmpty = this.n = 0

  /// Swaps items i and j.
  member inline this.swap(i, j) =
    let tmp = this.[i]
    this.[i] <- this.[j]
    this.[j] <- tmp

  /// Auto-trim accessor. If auto-trim is enabled, the array is trimmed automatically if it gets too empty.
  member inline this.autoTrim
    with get() = this.autoTrimMask <> 0
    and set trim = this.autoTrimMask <- DarrayTrim.mask(trim)

  /// Allocates space for at least n' elements. Does not alter the logical size of the array.
  member this.allocate(n') =
    if n' > this.capacity then
      let capacity' = max3 4 n' (this.capacity <<< 1)
      let cdelta = capacity' - this.capacity
      let expansion = Array.zeroCreate cdelta
      if this.capacity > 0 then
        let a = this.a
        this.a <- Array.append this.a expansion
        Array.scrub a
      else this.a <- expansion

  /// Shrinks the number of items in the array to n'. The new size must not be larger than the current size.
  /// Trims the array as well, if necessary. However, the array is never completely deallocated.
  /// Clients typically invoke Darray.resize, which then calls this method.
  member this.shrink(n') =
    enforce (n' <= this.size) "Darray.shrink: The new size cannot be larger than the current size."
    // Trimming comes before recycling, as trimmed items do not need to be recycled.
    let trimCapacity = this.capacity >>> 2
    if (trimCapacity &&& this.autoTrimMask) > n' then
      let a = this.a
      this.a <- Array.init trimCapacity a.at
      Array.scrub a
      // Adjust logical size in the interim before recycling.
      this.n <- min this.n this.a.size
    // Scrub unused items.
    for i = n' to this.last do this.[i] <- Unchecked.defaultof<'a>
    this.n <- n'
    
  /// Adds an element to the end of the array.
  member inline this.add(v) =
    if this.size = this.capacity then this.allocate (this.size + 1)
    this.[this.size] <- v
    this.n <- this.n + 1

  /// Discards an element from the array by replacing it with the last element of the array.
  /// The order of the elements is thus not preserved.
  member inline this.discard(i) =
    assert (this.size > 0)
    if i < this.last then this.swap(i, this.last)
    this.shrink(this.size - 1)

  /// Inserts an item before the ith item in the array. The order of the items is preserved.
  member this.insert(i, v) =
    enforce (i >= 0 && i <= this.size) "Darray.insert: Invalid position."
    if this.size = this.capacity then this.allocate(this.size + 1)
    for j = this.n downto i + 1 do this.[j] <- this.[j - 1]
    this.[i] <- v
    this.n <- this.n + 1

  /// Inserts a sequence of items before the ith item in the array. The order of the items is preserved.
  member this.insertFrom(i, v : #seq<'a>) =
    enforce (i >= 0 && i <= this.size) "Darray.insertFrom: Invalid position."
    let v = Array.ofSeq v
    let n = v.Length
    if n > 0 then
      this.allocate(this.size + n)
      for j = this.n + n - 1 downto i + n do this.[j] <- this.[j - n]
      for j = 0 to n - 1 do this.[i + j] <- v.[j]
    this.n <- this.n + n

  /// Adds a sequence of items to the end of the array.
  member this.addFrom(v) =
    this.insertFrom(this.size, v)

  /// Removes an item from the array. The order of the items is preserved.
  member this.remove(i) =
    enforce (i >= 0 && i < this.n) "Darray.remove: Invalid index."
    if i < this.last then
      let item = this.[i]
      for j = i + 1 to this.last do this.[j - 1] <- this.[j]
      this.[this.last] <- item
    this.shrink(this.size - 1)

  /// Resizes the array. If there is no initializer, then any new elements are zero allocated.
  member this.resize(n') =
    enforce (n' >= 0) "Darray.resize: Size cannot be negative."
    if n' > this.size then
      this.allocate(n')
      this.n <- n'
    elif n' < this.size then
      this.shrink(n')

  /// Makes the size of the array no larger than the argument.
  member inline this.retain(n') =
    if n' < this.size then this.shrink(n')

  /// Makes this a shallow copy of another array.
  member this.copyFrom(a : 'a Darray) =
    this.resize(a.size)
    this.autoTrimMask <- a.autoTrimMask
    for i = 0 to a.last do this.[i] <- a.[i]

  /// Makes this a shallow copy of another array.
  member this.copyFrom(a : 'a[]) =
    this.resize(a.size)
    for i = 0 to a.last do this.[i] <- a.[i]

  /// Initializes the array with n elements from an indexed function.
  member this.initialize(n, f : int -> 'a) =
    this.resize(n)
    for i = 0 to this.last do this.[i] <- f i

  /// Initializes the array with n cloned items.
  member this.initialize(n, a) =
    this.resize(n)
    for i = 0 to this.last do this.[i] <- a

  /// Swaps this array with another array.
  member this.swap(a : 'a Darray) =
    let a' = a.a
    let n' = a.n
    let trim = a.autoTrimMask
    a.a <- this.a
    a.n <- this.n
    a.autoTrimMask <- this.autoTrimMask
    this.a <- a'
    this.n <- n'
    this.autoTrimMask <- trim

  /// Calls the function for each item in the array.
  member inline this.iter(f : 'a -> unit) =
    for i = 0 to this.last do f this.[i]

  /// Fills the array with items from an indexed function.
  member inline this.fill(f : int -> _) =
    for i = 0 to this.last do this.[i] <- f i

  /// Fills the array with the item.
  member inline this.fill(x : 'a) =
    for i = 0 to this.last do this.[i] <- x

  /// Modifies each item with the function, performing an in-place map operation.
  member inline this.modify(f) =
    for i = 0 to this.last do this.[i] <- f this.[i]

  /// Retains only items that fulfill the predicate. The order of items is preserved.
  member this.filter(predicate : 'a -> bool) =
    let mutable n = 0
    for i = 0 to this.last do
      if predicate this.[i] then
        if n < i then this.[n] <- this.[i]
        n <- n + 1
    if n < this.size then
      for k = n to this.last do this.[k] <- Unchecked.defaultof<'a>
      this.n <- n
      // Scrubbing has been done but we may still want to trim the array.
      this.shrink(n)

  /// Resets the array to an empty state.
  member inline this.reset() =
    if this.size > 0 then this.shrink(0)

  /// Returns true iff the predicate is true for some item in the array.
  member inline this.exists(predicate) = Fun.exists 0 this.last (fun i -> predicate this.[i])

  /// Returns true if the predicate is true for all items in the array, or if the array is empty.
  member inline this.forall(predicate) = Fun.forall 0 this.last (fun i -> predicate this.[i])

  /// Returns the index of the first item satisfying the predicate or Noneval if there are none.
  member inline this.find(predicate) = Fun.findArg 0 this.last (fun i -> predicate this.[i])

  /// Empties and deallocates the array.
  member this.deallocate() =
    if this.capacity > 0 then
      this.a <- Array.createEmpty
      this.n <- 0

  /// Trims the array. Array size equals capacity after this call.
  member this.trim() =
    if this.size < this.capacity then
      this.a <- this.a.[0 .. this.last]

  /// Returns a copy of this array as a native array.
  member this.toArray =
    if this.size > 0 then
      this.a.[0 .. this.last]
    else
      Array.createEmpty

  // SORTED ARRAY INTERFACE (LINEAR TIME)

  /// Adds an item to an array sorted by the projection.
  member inline this.addSorted(v : 'a, projection : 'a -> 'b) =
    let vc = projection v
    let mutable i = 0
    while i < this.size do
      if vc > projection this.[i] then
        i <- i + 1
      else
        this.insert(i, v)
        i <- this.size + 1
    if i = this.size then this.add(v)


  // SET INTERFACE (LINEAR TIME)

  /// Adds an item to the array, unless a matching item exists in the array already.
  member inline this.addOnce(v : 'a, equalf : 'a -> 'a -> bool) =
    let mutable i = 0
    while i < this.size do
      if equalf this.[i] v then i <- this.size + 1 else i <- i + 1
    if i = this.size then this.add(v)

  /// Adds an item to the array, unless a matching item exists in the array already.
  member inline this.addOnce(v : 'a, equalf : 'a * 'a -> bool) =
    let mutable i = 0
    while i < this.size do
      if equalf(this.[i], v) then i <- this.size + 1 else i <- i + 1
    if i = this.size then this.add(v)

  /// Attempts to remove a matching item from the array. Ordering of the items is not preserved.
  /// Returns whether an item was found and removed.
  member inline this.discardOnce(v : 'a, equalf : 'a -> 'a -> bool) =
    let mutable i = 0
    while i < this.size do
      if equalf this.[i] v then
        this.discard(i)
        i <- this.size + 1
      else i <- i + 1
    i > this.size

  /// Attempts to remove a matching item from the array. Ordering of the items is not preserved.
  /// Returns whether an item was found and removed.
  member inline this.discardOnce(v : 'a, equalf : 'a * 'a -> bool) =
    let mutable i = 0
    while i < this.size do
      if equalf(this.[i], v) then
        this.discard(i)
        i <- this.size + 1
      else i <- i + 1
    i > this.size

  /// Removes one instance of a matching item from the array; raises an exception if it is not found.
  /// Ordering of the items is not preserved.
  member inline this.forceDiscardOnce(v : 'a, equalf : 'a -> 'a -> bool) =
    enforce (this.discardOnce(v, equalf)) "Darray.forceDiscardOnce: Item not found."

  /// Removes one instance of a matching item from the array; raises an exception if it is not found.
  /// Ordering of the items is not preserved.
  member inline this.forceDiscardOnce(v : 'a, equalf : 'a * 'a -> bool) =
    enforce (this.discardOnce(v, equalf)) "Darray.forceDiscardOnce: Item not found."


  // STACK INTERFACE

  /// Pushes an item to the end of the array.
  member inline this.push(v) = this.add(v)

  /// Removes the last item from the array and returns it.
  member inline this.pull() =
    assert (this.size > 0)
    let n' = this.size - 1
    let top = this.[n']
    this.shrink(n')
    top

  /// Discards an item from the end of the array.
  member inline this.pop() =
    assert (this.size > 0)
    this.shrink(this.size - 1)

  /// Sorts the array with the given projection.
  member inline this.sortBy(projection) = Fun.quicksort 0 this.last (fun i -> projection this.[i]) (fun i j -> this.swap(i, j))

  /// Reduces the array with the given projection and binary operator.
  /// The array must be non-empty.
  member inline this.reduceBy(projection, binop) = Fun.reduce 0 this.last (fun i -> projection this.[i]) binop


  // STATIC CONSTRUCTORS

  /// Creates a dynamic array of length n. Each item is initialized to the given value.
  static member createN(n, v : 'a) =
    { a = Array.create n v; n = n; autoTrimMask = ~~~0 }

  /// Creates an empty dynamic array.
  static member create(?autoTrim) : Darray<'a> =
    { a = Array.createEmpty; n = 0; autoTrimMask = DarrayTrim.mask(autoTrim >? true) }

  /// Creates a dynamic array of length n. Each item is initialized to the zero value.
  static member createZero(n, ?autoTrim) : Darray<'a> =
    { a = Array.zeroCreate n; n = n; autoTrimMask = DarrayTrim.mask(autoTrim >? true) }

  /// Creates a dynamic array of length n with its contents initialized with the supplied function.
  static member createWith(n, f : int -> 'a, ?autoTrim) =
    { a = Array.init n f; n = n; autoTrimMask = DarrayTrim.mask(autoTrim >? true) }

  /// Creates a (shallow) copy of a dynamic array.
  static member createCopy(a : 'a Darray) =
    { a = Array.init a.size a.at; n = a.size; autoTrimMask = a.autoTrimMask }

  /// Creates a dynamic array containing a single item.
  static member createSingle(v : 'a, ?autoTrim) =
    { a = [| v |]; n = 1; autoTrimMask = DarrayTrim.mask(autoTrim >? true) }

  /// Builds a dynamic array from a sequence.
  static member createFrom(a : #seq<'a>, ?autoTrim) =
    let a = Array.ofSeq a
    { a = a; n = a.size; autoTrimMask = DarrayTrim.mask(autoTrim >? true) }

  /// Builds a dynamic array from a list.
  static member createFrom(a : list<'a>, ?autoTrim) =
    let a = Array.ofList a
    { a = a; n = a.size; autoTrimMask = DarrayTrim.mask(autoTrim >? true) }

  /// Builds a dynamic array from a native array.
  static member createFrom(a : array<'a>, ?autoTrim) =
    { a = Array.copy a; n = a.size; autoTrimMask = DarrayTrim.mask(autoTrim >? true) }


  interface System.Collections.IEnumerable with
    member this.GetEnumerator() = { DarrayEnumerator.i = -1; a = this } :> System.Collections.IEnumerator


  interface System.Collections.Generic.IEnumerable<'a> with
    member this.GetEnumerator() = { DarrayEnumerator.i = -1; a = this } :> System.Collections.Generic.IEnumerator<'a>



and [<NoEquality; NoComparison>] DarrayEnumerator<'a> =
  {
    mutable i : int
    a : 'a Darray
  }

  interface System.Collections.IEnumerator with
    member this.Current = box this.a.[this.i]
    member this.MoveNext() =
      if this.i < this.a.last then
        this.i <- this.i + 1
        true
      else
        this.i <- this.a.size
        false
    member this.Reset() = this.i <- -1

  interface System.Collections.Generic.IEnumerator<'a> with
    member this.Dispose() = ()
    member this.Current = this.a.[this.i]

