namespace FsMap3

open Common
open DnaParameter


/// Dna source for mutation and crossover.
type RecombinationSource(seed) =
  inherit DnaSource()

  let bins = 64
  let memorySize = 16

  let poolSet =
    [| 5.0, CodePool.create(structuralHash, memorySize)
       1.0, CodePool.create(semanticHash, memorySize)
       1.0, CodePool.create(approximateStructuralHash 2, memorySize)
       1.0, CodePool.create(markovHash 1 bins, memorySize)
       1.0, CodePool.create(markovHash 2 bins, memorySize)
       1.0, CodePool.create(markovHash 3 bins, memorySize)
       1.0, CodePool.create(parentGrandparentHash bins, memorySize)
     |]

  let transientSet = Array.copy poolSet

  /// Generic parameter predicate.
  member val parameterPredicate = fun _ _ _ -> ParameterAction.Retain with get, set

  member val rnd = Rnd(seed) with get, set

  member private this.value(dna, i) =
    let valueSet = HashMap.create(int >> Mangle.mangle32, 0.0)
    for weight, pool in transientSet do
      match pool.find(dna, i) with
      | Someval(memory) when memory.size > 0 ->
        let value = memory.[this.rnd.int(memory.size)].value
        valueSet.[value] <- valueSet.[value] + weight
      | _ -> ()
    if valueSet.size = 0 then
      Noneval
    else
      let (Pair(_, value)) = valueSet.fold(Pair(0.0, 0u), fun (Pair(W, V)) v w -> Pair(W + w, if this.rnd.float(W + w) < W then V else v))
      Someval(value)

  member private this.forget(dna, i, value) =
    // Forget one instance of the value.
    for _, pool in transientSet do
      pool.removeRetrieved(dna, i, value)

  member this.reset() =
    for _, pool in poolSet do
      pool.reset()

  override this.start() =
    poolSet |> Array.iteri (fun i (weight, pool) -> transientSet.[i] <- weight, CodePool.createCopy(pool))

  override this.observe(dna, fitness) =
    for _, pool in poolSet do
      for i = 0 to dna.last do
        pool.add(dna, i, fitness)

  override this.choose(dna, i, choices) =
    let action = this.parameterPredicate this.rnd dna i
    let value = this.value(dna, i)
    match action, value with
    | Retain, Someval(value) when choices.isLegal(value) ->
      this.forget(dna, i, value)
      value
    | _ ->
      action.apply(this.rnd, dna, i, choices, value)

  override this.choose(dna, i) =
    let action = this.parameterPredicate this.rnd dna i
    let value = this.value(dna, i)
    match action, value with
    | Retain, Someval(value) when dna.[i].isLegal(value) ->
      this.forget(dna, i, value)
      value
    | _ ->
      action.apply(this.rnd, dna, i, value)

  override this.chooseFloat(dna, i, valueTransform, priorTransform) =
    let action = this.parameterPredicate this.rnd dna i
    let value = this.value(dna, i)
    match action, value with
    | Retain, Someval(value) when dna.[i].isLegal(value) ->
      this.forget(dna, i, value)
      value
    | _ ->
      action.apply(this.rnd, dna, i, valueTransform, priorTransform, value)

