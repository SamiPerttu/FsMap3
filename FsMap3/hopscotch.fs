// Hash maps via hopscotch open addressing. Among open addressing techniques
// this is about as fast as cuckoo hashing, but far simpler.
namespace FsMap3

open Common


/// Bucket is a slot for storing an item in our open addressed hash table.
/// We use Buckets in the stash as well, even though flags are not needed there.
[<NoEquality; NoComparison>]
type Bucket<'k, 'v> = struct
  /// Bit field in LSBs indicating which of the 31 nearby buckets contain
  /// an item hashing to this bucket, plus occupancy flag in MSB.
  val mutable flags : uint
  /// The key.
  val key : 'k
  /// The value.
  val mutable value : 'v

  new(flags, key, value) = { flags = flags; key = key; value = value }

  /// The hop bit field.
  member inline this.hop = this.flags &&& 0x7fffffffu
  /// The occupied flag is non-zero if this bucket contains an item.
  member inline this.occupiedFlag = this.flags &&& 0x80000000u
  member inline this.isEmpty = this.occupiedFlag = 0u
  member inline this.isOccupied = this.occupiedFlag <> 0u

  override this.ToString() = sprintf "(%A -> %A)" this.key this.value
end



type HashConfig =
  /// Minimum table capacity (this must be a power of two).
  static member inline minimumCapacity = 4

  /// Maximum load factor before more capacity is allocated.
  static member inline maximumLoad = 0.80

  /// When automatic trim is enabled, a hash table is shrunk if its load factor falls below this minimum.
  static member inline minimumLoad = 0.25



/// A hash map using hopscotch open addressing.
[<ReferenceEquality>]
type HashMap<'k, 'v when 'k : equality> =
  {
    /// The hash function.
    hashf : 'k -> int
    /// Hash table. Size is always a power of two.
    mutable table : Bucket<'k, 'v>[]
    /// Bucket mask = size of table minus one.
    mutable mask : int
    /// Number of items in table + stash.
    mutable size : int
    /// Extra items that could not be placed into table.
    /// In practice, this sees use in pathological situations only.
    stash : Bucket<'k, 'v> Darray
    /// Default value that, if set, applies to get_Item.
    mutable defaultValue : 'v option
    /// Whether the table is shrunk automatically if it gets too sparse.
    mutable autoTrim : bool
  }

  // INTERNAL METHODS

  /// Hashes the key.
  member inline internal this.hash(key) = this.hashf key

  /// Tests two keys for equality.
  member inline internal this.equal(key1, key2) = key1 = key2

  /// Neighborhood size (fixed to number of bits in int, minus 1 occupancy bit).
  member inline internal this.neighborhood = 31

  /// Null keys are never accessed.
  member inline internal this.nullKey = nullValue<'k>

  /// Null values are never accessed.
  member inline internal this.nullValue = nullValue<'v>

  /// Returns the index of the bucket associated with the hash value.
  member inline internal this.bucket(hash) = hash &&& this.mask

  /// Modular (i.e., hash table) distance between two buckets.
  member inline internal this.distance(bucket1, bucket2) = (bucket2 - bucket1) &&& this.mask

  /// Hop value (mask) when bucket2 contains an item hashing to bucket1.
  member inline internal this.hop(bucket1, bucket2) = 1u <<< this.distance(bucket1, bucket2)

  /// Hop value (mask), given bucket distance.
  member inline internal this.hop(distance) = 1u <<< distance

  /// Current capacity of the table.
  member inline this.capacity = this.table.Length

  /// Resizes the table. Capacity is always a power of two.
  member private this.resize(capacity') =
    enforce (Bits.isPowerOf2 capacity') "HashMap.resize: Internal error: capacity is not a power of two."
    let map' = { this with table = Array.zeroCreate capacity'; mask = capacity' - 1; size = 0; stash = Darray.create() }
    for i = 0 to this.capacity - 1 do
      if this.table.[i].isOccupied then map'.insert(this.table.[i].key, this.table.[i].value)
    for i = 0 to this.stash.size - 1 do
      map'.insert(this.stash.[i].key, this.stash.[i].value)
    Array.scrub this.table
    this.table <- map'.table
    this.mask <- map'.mask
    enforce (this.size = map'.size) "HashMap.resize: Internal error."
    this.stash.swap(map'.stash)
    // Anything we can do to help the GC.
    map'.stash.deallocate()
    map'.table <- Array.createEmpty


  // PUBLIC INTERFACE
  
  /// Resets the map. If automatic trim is enabled, the map is shrunk as well.
  member this.reset() =
    if this.autoTrim then
      this.table <- Array.zeroCreate HashConfig.minimumCapacity
      this.mask <- this.table.size - 1
    else
      this.table.scrub()
    this.stash.reset()
    this.size <- 0


  /// Tries to locate the value associated with the given key. Returns Noneval if it is not found.
  /// Does not return the default value.
  member this.find(key : 'k) =
    let hash = this.hash(key)
    let rec searchStash i =
      if i < this.stash.size then
        if this.equal(this.stash.[i].key, key) then
          Someval(this.stash.[i].value)
        else searchStash (i + 1)
      else Noneval
    let rec locateKey hop =
      if hop <> 0u then
        let lowest = Bits.lowestBit32 hop
        let i = this.bucket(hash + lowest)
        if this.equal(this.table.[i].key, key) then
          Someval(this.table.[i].value)
        else locateKey (hop ^^^ this.hop(lowest))
      else searchStash 0
    locateKey this.table.[this.bucket(hash)].hop


  /// Returns the value associated with the given key. If not found, returns the default value if there is one,
  /// otherwise raises an exception.
  member this.at(key : 'k) =
    let hash = this.hash(key)
    let rec searchStash i =
      if i < this.stash.size then
        if this.equal(this.stash.[i].key, key) then
          this.stash.[i].value
        else searchStash (i + 1)
      elif this.defaultValue.isSome then
        !this.defaultValue
      else
        failwithf "HashMap.at: Key %A not found." key
    let rec locateKey hop =
      if hop <> 0u then
        let lowest = Bits.lowestBit32 hop
        let i = this.bucket(hash + lowest)
        if this.equal(this.table.[i].key, key) then
          this.table.[i].value
        else locateKey (hop ^^^ this.hop(lowest))
      else searchStash 0
    locateKey this.table.[this.bucket(hash)].hop


  member this.Item
    /// Returns the value associated with the key. If we attempt to index an unknown key, then we
    /// return the default value if it is set, otherwise we throw an exception. The default value
    /// is not inserted into the map.
    with get key =
      // This imperative version is clearly faster than the recursive version in HashMap.at.
      let hash = this.hash(key)
      let mutable hop = this.table.[this.bucket(hash)].hop
      let mutable value = this.nullValue
      let mutable found = false
      while hop <> 0u do
        let lowest = Bits.lowestBit32 hop
        let i = this.bucket(hash + lowest)
        if this.equal(this.table.[i].key, key) then
          value <- this.table.[i].value
          found <- true
          hop <- 0u
        else hop <- hop ^^^ this.hop(lowest)
      if found then
        value
      else
        match Fun.findArg 0 this.stash.last (fun i -> this.equal(this.stash.[i].key, key)) with
        | Someval(i) ->
          this.stash.[i].value
        | Noneval -> 
          match this.defaultValue with
          | Some(x) -> x
          | None -> failwithf "HashMap.get_Item: Key %A not found." key

    /// Sets the value associated with the key. If the mapping is already in the table
    /// its value is modified, otherwise a new mapping is created.
    and set key value =
      let hash = this.hash(key)
      let mutable hop = this.table.[this.bucket(hash)].hop
      let mutable found = false
      while hop <> 0u do
        let lowest = Bits.lowestBit32 hop
        let i = this.bucket(hash + lowest)
        if this.equal(this.table.[i].key, key) then
          this.table.[i].value <- value
          found <- true
          hop <- 0u
        else hop <- hop ^^^ this.hop(lowest)
      if not found then
        let mutable i = 0
        while i < this.stash.size do
          if this.equal(this.stash.[i].key, key) then
            this.stash.[i] <- Bucket(0u, this.stash.[i].key, value)
            found <- true
            i <- this.stash.size
          else i <- i + 1
        if not found then this.insert(key, value)
      

  /// Returns the value associated with the given key. If the key is not in the table yet, it is inserted
  /// into the table together with a value obtained from the default function. The difference between default
  /// values and default functions is that values from default functions are inserted into the table.
  member this.at(key : 'k, defaultf : unit -> 'v) =
    let hash = this.hash(key)
    let rec searchStash i =
      if i < this.stash.size then
        if this.equal(this.stash.[i].key, key) then this.stash.[i].value else searchStash (i + 1)
      else
        let value = defaultf()
        this.insert(key, value)
        value
    let rec locateKey hop =
      if hop <> 0u then
        let lowest = Bits.lowestBit32 hop
        let i = this.bucket(hash + lowest)
        if this.equal(this.table.[i].key, key) then
          this.table.[i].value
        else locateKey (hop ^^^ this.hop(lowest))
      else searchStash 0
    locateKey this.table.[this.bucket(hash)].hop


  /// Inserts a mapping into the table. Does not check whether the key is already contained in the table.
  /// Entering duplicate keys is legal but may degrade performance.
  member this.insert(key : 'k, value : 'v) =
    if float this.size >= float this.capacity * HashConfig.maximumLoad then
      this.resize(this.capacity * 2)
    let hash = this.hash(key)
    // First we need to find an empty bucket. This is guaranteed to succeed.
    let rec findEmpty i =
      if this.table.[i].isOccupied then
        // All arguments to the recursive functions are masked, i.e., legal table indices.
        findEmpty (this.bucket(i + 1))
      else
        i
    // We recursively move the empty bucket closer to the target, until it fulfills the neighborhood criterion.
    let rec moveCloser i =
      // We need to use modular arithmetic when computing distances.
      if this.distance(hash, i) < this.neighborhood then
        i
      else
        let rec findNew j =
          // We can compare i and j for equality, as both are already masked.
          if j = i then
            -1
          else
            let b = this.bucket(this.hash(this.table.[j].key))
            if this.table.[j].isOccupied && this.distance(b, i) < this.neighborhood then
              this.table.[i] <- Bucket(this.table.[i].flags ||| 0x80000000u, this.table.[j].key, this.table.[j].value)
              this.table.[j] <- Bucket(this.table.[j].hop, this.nullKey, this.nullValue)
              this.table.[b].flags <- this.table.[b].flags ^^^ this.hop(b, i) ^^^ this.hop(b, j)
              moveCloser j
            else findNew (this.bucket(j + 1))
        findNew (this.bucket(i - this.neighborhood + 1))

    let bucket = this.bucket(hash)
    let i = moveCloser (findEmpty bucket)
    if i < 0 then
      // Place the element in the stash. In practice this happens in pathological situations only.
      this.stash.add(Bucket(0u, key, value))
      this.size <- this.size + 1
    else
      this.table.[i] <- Bucket(this.table.[i].flags ||| 0x80000000u, key, value)
      this.table.[bucket].flags <- this.table.[bucket].flags ||| this.hop(bucket, i)
      this.size <- this.size + 1


  /// Removes a mapping from the table. Raises an exception if the key is not found.
  member this.remove(key : 'k) =
    if this.autoTrim && this.capacity > HashConfig.minimumCapacity && float this.size < float this.capacity * HashConfig.minimumLoad then
      this.resize(this.capacity >>> 1)
    let hash = this.hash(key)
    let bucket = this.bucket(hash)
    let rec searchStash i =
      if i < this.stash.size then
        if this.equal(this.stash.[i].key, key) then
          this.stash.discard(i)
          this.size <- this.size - 1
        else searchStash (i + 1)
      else failwithf "HopMap.remove: Cannot find key %A." key
    let rec locateKey hop =
      if hop <> 0u then
        let lowest = Bits.lowestBit32 hop
        let i = this.bucket(bucket + lowest)
        if this.equal(this.table.[i].key, key) then
          // Note that when we build the new bucket with just the hop value, it clears the occupancy flag.
          this.table.[i] <- Bucket(this.table.[i].hop, this.nullKey, this.nullValue)
          this.table.[bucket].flags <- this.table.[bucket].flags ^^^ this.hop(bucket, i)
          this.size <- this.size - 1
        else locateKey (hop ^^^ this.hop(lowest))
      else searchStash 0
    locateKey this.table.[bucket].hop


  /// Calls the supplied function for each item in the table. The ordering is arbitrary.
  member inline this.iter(f : 'k -> 'v -> unit) =
    for i = 0 to this.capacity - 1 do
      if this.table.[i].isOccupied then f this.table.[i].key this.table.[i].value
    for i = 0 to this.stash.last do f this.stash.[i].key this.stash.[i].value


  /// Calls the supplied function for each item in the table. Additionally, numbers the items
  /// starting from zero. The ordering is arbitrary.
  member inline this.iteri(f : int -> 'k -> 'v -> unit) =
    let mutable n = 0
    for i = 0 to this.capacity - 1 do
      if this.table.[i].isOccupied then
        f n this.table.[i].key this.table.[i].value
        n <- n + 1
    for i = 0 to this.stash.last do
      f n this.stash.[i].key this.stash.[i].value
      n <- n + 1


  /// Returns the keys in the table as an array. The ordering is arbitrary.
  member this.keys =
    let a = Array.zeroCreate this.size
    this.iteri(fun i k _ -> a.[i] <- k)
    a


  /// Returns the values in the table as an array. The ordering is arbitrary.
  member this.values =
    let a = Array.zeroCreate this.size
    this.iteri(fun i _ v -> a.[i] <- v)
    a


  /// Returns the items in the table as a Pair(key, value) array. The ordering is arbitrary.
  member this.items =
    let a = Array.zeroCreate this.size
    this.iteri(fun i k v -> a.[i] <- Pair(k, v))
    a


  /// Retains only items that match the predicate.
  member this.filter(predicate : 'k -> 'v -> bool) =
    // TODO: Check auto-trim.
    for i = 0 to this.capacity - 1 do
      if this.table.[i].isOccupied then
        if predicate this.table.[i].key this.table.[i].value = false then
          let bucket = this.bucket(this.hash(this.table.[i].key))
          this.table.[i] <- Bucket(this.table.[i].hop, this.nullKey, this.nullValue)
          this.table.[bucket].flags <- this.table.[bucket].flags ^^^ this.hop(bucket, i)
          this.size <- this.size - 1
    let mutable i = 0
    while i < this.stash.size do
      if predicate this.stash.[i].key this.stash.[i].value = false then
        this.stash.discard(i)
        this.size <- this.size - 1
      else i <- i + 1


  // CONSTRUCTION

  /// Creates an empty hash map. The hash function hashes a key.
  /// The optional default value will be returned from some accessors if the queried key is not present in the map.
  /// The default value is never inserted in the map.
  static member create(hashf, ?defaultValue, ?autoTrim) : HashMap<'k, 'v> =
    let autoTrim = autoTrim >? true
    {
      HashMap.hashf = hashf
      table = Array.zeroCreate HashConfig.minimumCapacity
      mask = HashConfig.minimumCapacity - 1
      size = 0
      stash = Darray.create(autoTrim = autoTrim)
      defaultValue = defaultValue
      autoTrim = autoTrim
    }


  // DIAGNOSTICS
  
  member this.check() =
    Log.infof "HashMap.check(): size %d, capacity %d." this.size this.capacity
    // Check that hop information is correct.
    for i = 0 to this.capacity - 1 do
      let hop = this.table.[i].hop
      for d = 0 to min this.capacity this.neighborhood - 1 do
        let j = this.bucket(i + d)
        let hopFlag = this.hop(d)
        if hop &&& hopFlag = 0u then
          enforce (this.table.[j].isEmpty || this.bucket(this.hash(this.table.[j].key)) <> i) "HashMap: Hop flag is 0, yet entry matches the bucket."
        else
          enforce (this.table.[j].isOccupied && this.bucket(this.hash(this.table.[j].key)) = i) "HashMap: Hop flag is 1 but entry does not match the bucket."
    Log.info "HashMap.check(): OK."



/// An empty structure.
[<NoEquality; NoComparison>]
type EmptyStruct = struct end



/// A hash (multi)set.
[<ReferenceEquality>]
type HashSet<'k when 'k : equality> =
  {
    map : HashMap<'k, EmptyStruct>
  }
  /// The number of keys in the set. Each instance of an identical key is counted separately.
  member inline this.size = this.map.size
  /// Returns whether the set contains the key.
  member inline this.exists(key) = this.map.find(key).isSome
  /// Adds a key to the set. Duplicates are not checked for; entering duplicates is legal but may reduce performance.
  member inline this.add(key) = this.map.insert(key, EmptyStruct())
  /// Removes (an instance of) the key from the set. Throws an exception if it is not found.
  member inline this.remove(key) = this.map.remove(key)
  /// Retains only keys that fulfill the predicate.
  member inline this.filter(predicate : 'k -> bool) = this.map.filter(fun key _ -> predicate key)
  /// Calls a function for each key in the set. The ordering is arbitrary.
  member inline this.iter(f : 'k -> unit) = this.map.iter(fun key _ -> f key)
  /// Empties the set. The table is deallocated if 
  member inline this.reset() = this.map.reset()
  /// Adds the key to the set, if it is not in it yet.
  member inline this.set(key) = if this.map.find(key).isNone then this.add(key)
  /// Removes (an instance of) the key from the set, if it is contained in it.
  member inline this.unset(key) = if this.map.find(key).isSome then this.remove(key)
  /// Creates an empty hash set.
  static member create(hashf, ?autoTrim : bool) : HashSet<'k> = { HashSet.map = HashMap.create(hashf, autoTrim = (autoTrim >? true)) }


