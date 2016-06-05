// Memory for storing Dna parameter codes.
namespace FsMap3

open Common
open Mangle
open DnaParameter


/// Occurrence of a parameter code.
type CodeInstance = struct
  /// Value of the parameter.
  val value : uint
  /// How many times have we seen the value for this code.
  val count : int
  /// Maximum fitness of genotypes containing the parameter.
  val fitness : float
  new(value, count) = { value = value; count = count; fitness = 0.0 }
  new(value, count, fitness) = { value = value; count = count; fitness = fitness }
end



/// Stores occurrences for a single code of a parameter hash.
[<NoEquality; NoComparison>]
type CodeMemory =
  {
    /// Instances.
    instance : CodeInstance Darray
    /// Whether the instance array is sorted by decreasing fitness.
    mutable sorted : bool
  }
  
  member inline this.size = this.instance.size
  member inline this.last = this.instance.last
  member inline this.Item
    with get i = this.instance.[i]
    and set i x = this.instance.[i] <- x

  member this.add(x : CodeInstance) =
    match Fun.findArg 0 this.last (fun i -> this.[i].value = x.value) with
    | Someval(i) -> this.[i] <- CodeInstance(x.value, x.count + this.[i].count, max x.fitness this.[i].fitness)
    | Noneval    -> this.instance.add(x)
    this.sorted <- false

  member this.remove(x : CodeInstance) =
    match Fun.findArg 0 this.last (fun i -> this.[i].value = x.value) with
    | Someval(i) -> if x.count >= this.[i].count then
                      this.instance.discard(i)
                      this.sorted <- false
                    else
                      this.[i] <- CodeInstance(this.[i].value, this.[i].count - x.count, this.[i].fitness)
    | Noneval    -> ()

  member this.sort() =
    if this.sorted = false then
      if this.size > 1 then this.instance.sortBy(fun occurrence -> -occurrence.fitness)
      this.sorted <- true

  member this.trim(n) =
    this.sort()
    if this.size > n then this.instance.resize(n)

  member this.fittest =
    this.sort()
    this.instance.firstItem

  static member create() =
    {
      CodeMemory.instance = Darray.create()
      sorted = true
    }

  static member createCopy(memory : CodeMemory) =
    {
      CodeMemory.instance = Darray.createCopy(memory.instance)
      sorted = memory.sorted
    }



/// Records a moderate number of code instances of a parameter hash.
[<NoEquality; NoComparison>]
type CodePool =
  {
    storeHash : ParameterHash
    retrieveHash : ParameterHash
    hashMap : HashMap<int64, CodeMemory>
    maxValuesPerKey : int
    hash128 : Hash128
  }

  member inline this.key = this.hash128.a64

  member private this.hashStore(dna : Dna, i) =
    this.hash128.reset()
    if this.storeHash this.hash128 dna i then
      this.hash128.hashEnd()
      true
    else false

  member private this.hashRetrieve(dna : Dna, i) =
    this.hash128.reset()
    if this.retrieveHash this.hash128 dna i then
      this.hash128.hashEnd()
      true
    else false

  member this.reset() = this.hashMap.reset()

  member this.value(dna, i) =
    if this.hashRetrieve(dna, i) then
      match this.hashMap.find(this.key) with
        | Someval(memory) when memory.size > 0 ->
          memory.sort()
          Someval(memory.fittest.value)
        | _ -> Noneval
    else Noneval

  member this.find(dna, i) =
    if this.hashRetrieve(dna, i) then
      this.hashMap.find(this.key)
    else Noneval

  member this.add(dna, i, fitness) =
    if this.hashStore(dna, i) then
      let memory = this.hashMap.at(this.key, CodeMemory.create)
      memory.add(CodeInstance(dna.[i].value, 1, fitness))
      memory.trim(this.maxValuesPerKey)

  member this.removeRetrieved(dna, i, value) =
    if this.hashRetrieve(dna, i) then
      match this.hashMap.find(this.key) with
      | Someval(memory) -> memory.remove(CodeInstance(value, 1))
      | Noneval -> ()

  member this.removeRetrieved(dna, i) =
    this.removeRetrieved(dna, i, dna.[i].value)

  member this.record(dna : Dna, fitness) =
    for i = 0 to dna.last do this.add(dna, i, fitness)

  static member create(parameterHash, maxValuesPerKey) =
    {
      CodePool.storeHash = parameterHash
      retrieveHash = parameterHash
      hashMap = HashMap.create(int)
      maxValuesPerKey = maxValuesPerKey
      hash128 = Hash128.create()
    }

  static member create(storeHash, retrieveHash, maxValuesPerKey) =
    {
      CodePool.storeHash = storeHash
      retrieveHash = retrieveHash
      hashMap = HashMap.create(int)
      maxValuesPerKey = maxValuesPerKey
      hash128 = Hash128.create()
    }

  static member createCopy(pool : CodePool) =
    let this = CodePool.create(pool.storeHash, pool.retrieveHash, pool.maxValuesPerKey)
    pool.hashMap.iter(fun code memory -> this.hashMap.insert(code, CodeMemory.createCopy(memory)))
    this

