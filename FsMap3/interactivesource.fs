namespace FsMap3

open Common
open DnaParameter


/// Dna source for interactive editing.
type InteractiveSource(seed) =
  inherit DnaSource()

  let poolSet =
    [| CodePool.create(structuralHash, 1)
       CodePool.create(approximateStructuralHash 1, 1)
       CodePool.create(approximateStructuralHash 2, 1)
       CodePool.create(addressHash, relativeAddressHash (fun a -> a.parent), 1)
       CodePool.create(addressHash, relativeAddressHash (fun a -> a.ancestor(2)), 1)
       CodePool.create(relativeAddressHash (fun a -> a.parent), addressHash, 1)
       CodePool.create(relativeAddressHash (fun a -> a.ancestor(2)), addressHash, 1)
    |]

  member val rnd = Rnd(seed) with get, set

  /// Generic parameter predicate.
  member val parameterPredicate = fun _ _ _ -> ParameterAction.Retain with get, set

  /// Resets source memory.
  member this.reset() =
    for pool in poolSet do pool.reset()

  member private this.value(dna, i) =
    let rec value j =
      if j < poolSet.size then
        match poolSet.[j].value(dna, i) with
        | Noneval -> value (j + 1)
        | x -> x
      else Noneval
    value 0

  override this.observe(dna, fitness) =
    for i = 0 to dna.last do
      for pool in poolSet do pool.add(dna, i, fitness)

  override this.choose(dna, i, choices) =
    let existing = this.value(dna, i)
    let action = this.parameterPredicate this.rnd dna i
    action.apply(this.rnd, dna, i, choices, existing)

  override this.choose(dna, i) =
    let existing = this.value(dna, i)
    let action = this.parameterPredicate this.rnd dna i
    action.apply(this.rnd, dna, i, existing)

  override this.chooseFloat(dna, i, valueTransform, priorTransform) =
    let existing = this.value(dna, i)
    let action = this.parameterPredicate this.rnd dna i
    action.apply(this.rnd, dna, i, valueTransform, priorTransform, existing)

