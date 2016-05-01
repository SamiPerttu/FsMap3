// Dna source for interactive editing.
namespace FsMap3

open Common
open Mangle
open DnaParameter


/// Occurrence of a parameter code.
type CodeInstance = struct
  /// Index of the parameter in the Dna.
  val index : int
  /// Value of the parameter.
  val value : uint
  /// Fingerprint of the Dna containing the parameter.
  val fingerprint : int64
  /// Fitness of the Dna containing the parameter.
  val fitness : float
  new(index, value, fingerprint, fitness) = { index = index; value = value; fingerprint = fingerprint; fitness = fitness }
end



/// Stores occurrences for a single code of a parameter hash.
[<NoEquality; NoComparison>]
type CodeMemory =
  {
    /// Instance array.
    mutable value : CodeInstance Darray
    /// Whether the instance array is sorted by decreasing fitness.
    mutable sorted : bool
  }
  
  member inline this.size = this.value.size
  member inline this.last = this.value.last
  member inline this.Item
    with get i = this.value.[i]
    and set i x = this.value.set(i, x); this.sorted <- false

  member this.add(x : CodeInstance) =
    match Fun.findArg 0 this.last (fun i -> this.[i].value = x.value) with
    | Someval(i) -> if x.fitness >= this.[i].fitness then this.[i] <- x
    | Noneval    -> this.value.add(x)
                    this.sorted <- false

  member this.sort() =
    if this.sorted = false then
      if this.size > 1 then this.value.sortBy(fun occurrence -> -occurrence.fitness)
      this.sorted <- true

  member this.removeUnfittest() =
    this.sort()
    this.value.pop()

  static member create() =
    {
      CodeMemory.value = Darray.create()
      sorted = true
    }



/// Records a moderate number of code instances of a parameter hash.
[<NoEquality; NoComparison>]
type CodePool =
  {
    parameterHash : ParameterHash
    hashMap : HashMap<int64, CodeMemory>
    mutable maxValuesPerKey : int
    hash128 : Hash128
  }

  member inline this.key = this.hash128.a64

  member private this.hash(dna : Dna, i) =
    this.hash128.reset()
    if this.parameterHash this.hash128 dna i then
      this.hash128.hashEnd()
      true
    else false

  member private this.map(dna : Dna, i) =
    if this.hash(dna, i) then
      Someval(this.hashMap.at(this.key, CodeMemory.create))
    else Noneval

  member this.reset() = this.hashMap.reset()

  member this.value(dna, i) =
    if this.hash(dna, i) then
      match this.hashMap.find(this.key) with
        | Someval(memory) -> Someval(memory.[0].value)
        | Noneval -> Noneval
    else Noneval

  member this.find(dna, i) =
    if this.hash(dna, i) then this.hashMap.find(this.key) else Noneval

  member this.add(dna : Dna, i, fitness) =
    match this.map(dna, i) with
    | Someval(memory) -> memory.add(CodeInstance(i, dna.parameter(i).value, dna.fingerprint, fitness))
                         while memory.size > this.maxValuesPerKey do memory.removeUnfittest()
    | _ -> ()

  member this.record(dna : Dna, fitness) =
    for i = 0 to dna.last do this.add(dna, i, fitness)

  static member create(parameterHash, maxValuesPerKey) =
    {
      CodePool.parameterHash = parameterHash
      hashMap = HashMap.create(int)
      maxValuesPerKey = maxValuesPerKey
      hash128 = Hash128.create()
    }



[<NoComparison; NoEquality>]
type ParameterAction =
  /// Retain previous value, if it exists. Otherwise pick a random value.
  | Retain
  /// Select a random value. If a previous value exists, avoid selecting it.
  | Randomize
  /// Attempt to select a specific value. If not possible, pick a random value.
  | Select of value : uint
  /// If ordered, alter value by a fraction no larger than the given amount (amount > 0).
  /// If categorical, randomize.
  | Jolt of amount : float
  /// If ordered, modify (untransformed) value fractionally by delta (delta <> 0).
  /// If categorical, cycle value in the direction indicated by delta.
  | Modify of delta : float
  /// Modify existing transformed float with the function. Pick the closest legal value.
  /// In the absence of a previous value, transform a random value.
  | ModifyFloat of f : (float -> float)
  /// Attempt to select a transformed float. Pick the closest legal value.
  | SelectFloat of x : float



/// A mutation predicate decides what to do with each Dna parameter during generation.
type MutationPredicate = Rnd -> Dna -> int -> ParameterAction



/// Dna source for interactive editing and evolution.
type InteractiveSource(seed) =
  inherit DnaSource()

  /// Primary hash pool keeps track of user selected parameter values.
  let primary = CodePool.create(structuralHash, 1)

  /// Secondary hash pools help retain user selected values even when some parameters move around a little bit.
  let secondary1 = CodePool.create(approximateStructuralHash 1, 1)
  let secondary2 = CodePool.create(approximateStructuralHash 2, 1)

  member val rnd = Rnd(seed) with get, set

  /// Whether some amount of approximation is allowed when constructing new genotypes. This is typically set false for
  /// genotype level mutations, and true for individual, user selected parameter edits.
  member val approximate = true with get, set

  /// Generic mutation predicate.
  member val mutationPredicate = fun _ _ _ -> ParameterAction.Retain with get, set

  /// Resets source memory.
  member this.reset() =
    primary.reset()
    secondary1.reset()
    secondary2.reset()

  member private this.value(dna, i) =
    match primary.value(dna, i) with
    | Noneval ->
      if this.approximate then
        match secondary1.value(dna, i) with
        | Noneval -> secondary2.value(dna, i)
        | x -> x
      else Noneval
    | x -> x

  member private this.chooseCategorical(dna : Dna, i, choices : Choices<_> Option) =
    let maxValue = dna.parameter(i).maxValue
    let existing = this.value(dna, i).filter(fun v -> match choices with | Some(c) -> c.isLegal(v) | None -> v <= maxValue)
    existing.apply(fun v -> dna.[i].value <- v)
    let pickAny() = match choices with | Some(c) -> c.pick(this.rnd.float()) | None -> this.rnd.uint(0u, maxValue)
    match this.mutationPredicate this.rnd dna i with
    | Retain ->
      match existing with | Someval(v) -> v | _ -> pickAny()
    | Randomize | ModifyFloat _ | SelectFloat _ ->
      pickAny()
    | Select v ->
      match choices with
      | Some(c) -> if c.isLegal(v) then v else pickAny()
      | None -> if v <= maxValue then v else pickAny()
    | Jolt _ ->
      match choices, existing with
      | Some(c), Someval(v) -> c.pickExcluding(v, this.rnd.float())
      | _ -> pickAny()
    | Modify delta ->
      match existing with
      | Someval(v) ->
        match choices with
        | Some(c) ->
          let wrap x = if x > maxValue then x - maxValue else x
          (if delta > 0.0 then Fun.find else Fun.findBack) 1u maxValue
            (fun offset -> c.weight(wrap (v + offset)) > 0.0)
            (fun offset -> wrap (v + offset)) (always v)
        | None ->
          match sign delta, v with
          |  1, Eq(maxValue) -> 0u
          | -1, 0u           -> maxValue
          |  s, _            -> v + uint s
      | Noneval -> pickAny()

  member private this.chooseOrdered(dna : Dna, i, choices : Choices<_> Option, floatTransform : (uint -> float) option) =
    let maxValue = dna.parameter(i).maxValue
    let range = float maxValue + 1.0
    let existing = this.value(dna, i).filter(fun v -> match choices with | Some(c) -> c.isLegal(v) | None -> v <= maxValue)
    existing.apply(fun v -> dna.[i].value <- v)
    let pickAny() = match choices with | Some(c) -> c.pick(this.rnd.float()) | None -> this.rnd.uint(0u, maxValue)
    let tryPick(v : uint) =
      match choices with
      | Some(c) -> if c.isLegal(v) then v else pickAny()
      | None -> if v <= maxValue then v else pickAny()
    match this.mutationPredicate this.rnd dna i with
    | Retain ->
      match existing with | Someval(v) -> v | Noneval -> pickAny()
    | Randomize ->
      pickAny()
    | Select v ->
      tryPick(v)
    | Jolt amount ->
      match existing with
      | Someval(v) ->
        let v' = uint <| lerp (max 0.0 (float v - range * amount)) (min (float maxValue - 0.1) (float v + range * amount)) (this.rnd.float())
        tryPick(if v' <> v then v' else this.rnd.choose((if v > 0u then 1.0 else 0.0), v - 1u, (if v < maxValue then 1.0 else 0.0), v + 1u))
      | _ -> pickAny()
    | Modify delta ->
      match existing with
      | Someval(v) ->
        if delta < 0.0 && v > 0u then
          tryPick(uint <| clamp 0.0 (float v - 0.1) (float v + range * delta))
        elif delta > 0.0 && v < maxValue then
          tryPick(uint <| clamp (float v + 1.0) (float maxValue - 0.1) (float v + range * delta))
        else
          tryPick(v)
      | _ -> pickAny()
    | ModifyFloat f ->
      match floatTransform with
      | Some(transform) ->
        let v = match existing with | Someval(v) -> v | Noneval -> pickAny()
        Fun.binarySearchClosest 0u maxValue transform (f (transform v))
      | _ -> pickAny()
    | SelectFloat x ->
      match floatTransform with
      | Some(transform) ->
        Fun.binarySearchClosest 0u maxValue transform x
      | _ -> pickAny()

  override this.observe(dna, fitness) =
    for i = 0 to dna.last do
      primary.add(dna, i, fitness)
      secondary1.add(dna, i, fitness)
      secondary2.add(dna, i, fitness)

  override this.choose(dna, i, choices) =
    match choices.singular with
    | Someval(v) ->
      uint v
    | Noneval ->
      match dna.[i].format with
      | Categorical -> this.chooseCategorical(dna, i, Some(choices))
      | Ordered -> this.chooseOrdered(dna, i, Some(choices), None)

  override this.choose(dna, i) =
    if dna.[i].maxValue = 0u then
      0u
    else
      match dna.[i].format with
      | Categorical -> this.chooseCategorical(dna, i, None)
      | Ordered -> this.chooseOrdered(dna, i, None, None)

  override this.chooseFloat(dna, i, transform) =
    if dna.[i].maxValue = 0u then
      0u
    else
      match dna.[i].format with
      | Categorical -> this.chooseCategorical(dna, i, None)
      | Ordered -> this.chooseOrdered(dna, i, None, Some(transform))


