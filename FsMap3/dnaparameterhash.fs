/// Dna parameter hashes and related definitions.
module Fuse.DnaParameterHash

open Common
open Mangle


/// Discretizes the given parameter into at most bins equivalence classes. Does not bin categorical parameters.
/// Discretization is used to conflate related values when computing parameter hashes; they are not typically
/// used to store parameter values.
let valueBinner (bins : int) (dna : Dna) i =
  let parameter = dna.parameter(i)
  if parameter.format = DnaParameterFormat.Ordered && parameter.range > uint64 bins then
    int (float parameter.value * float bins / float parameter.range)
  else int parameter.value



(*
Parameter hashes always track backwards to find (more) context to hash. Otherwise causality is violated.
For example, if we follow a parent link up the tree, we might look at parameters in the same node as the parent,
but we would only track backwards, as any later parameters in the parent node would be drawn after the child.

It would be possible to create another model to evaluate the quality of a genotype after generation,
when all parameters are available. The argument here is that we want to avoid evaluation of the phenotype
to the last, as it is likely to be a quite expensive process. However, the amount of extra information
that could be gleaned from the full genotype may be too small to realize tangible benefits.
*)


/// A parameter hash function divides parameters into equivalence classes that we call codes. It is given
/// a Hash128 object for hashing, Dna, and the parameter being hashed. It returns true if a code was obtained.
type DnaParameterHash = Hash128 -> Dna -> int -> bool



/// A semantic hash of parameter name, format and number of categories.
/// This is the smallest (least informative) hash possible.
let semanticHash (hash : Hash128) (dna : Dna) i =
  hash.hash(dna.[i].semanticId)
  true 


/// A structural hash of parameter address, number and semantics. This hash is suitable for keeping track
/// of Dna parameters in interactive editing because it identifies them uniquely.
let structuralHash (hash : Hash128) (dna : Dna) i =
  hash.hash(dna.[i].structuralId)
  true


/// A structural hash of parameter address and semantics. It is like structuralHash except it does not
/// include node number.
let addressHash (hash : Hash128) (dna : Dna) i =
  hash.hash(dna.[i].semanticId)
  hash.hash(dna.[i].address.value)
  true


/// A structural hash of a transformed address and parameter semantics. Intended for asymmetric hashing
/// together with addressHash.
let relativeAddressHash (f : DnaAddress -> DnaAddress) (hash : Hash128) (dna : Dna) i =
  hash.hash(dna.[i].semanticId)
  let relativeAddress = f dna.[i].address
  if relativeAddress.isNull = false then
    hash.hash(relativeAddress.value)
    true
  else false


/// A structural hash of semantics and ancestor parameter address. More approximative than structuralHash.
/// ancestorLevel > 0 (1 = parent, 2 = grandparent, and so on).
let approximateStructuralHash (ancestorLevel : int) (hash : Hash128) (dna : Dna) i =
  hash.hash(dna.[i].semanticId)
  hash.hash(dna.[i].level)
  hash.hash(dna.[i].address.ancestor(ancestorLevel).value)
  true


/// A familial hash that conflates addresses of many generations (generations > 1, typically 2-4)
/// from the same ancestral line. Phase is in [0, generations[.
let familyAddressHash (generations : int) (phase : int) (hash : Hash128) (dna : Dna) i =
  hash.hash(dna.[i].semanticId)
  let familyLevel = if dna.[i].level < phase then 0 else ((dna.[i].level - phase) / generations) * generations + phase
  hash.hash(familyLevel)
  hash.hash(dna.[i].address.ancestor(dna.[i].level - familyLevel).value)
  true


/// A structural hash of local parameter address and semantics. addressLevels is the maximum number of
/// local levels that are hashed from target.
let localAddressHash
  (addressLevels : int)
  (hash : Hash128) (dna : Dna) i =
  hash.hash(dna.[i].semanticId)
  let levels = min dna.[i].level addressLevels
  hash.hash(levels)
  hash.hash(dna.[i].address.localId(addressLevels))
  true


/// Hashes the semantics and values of N previous parameters.
let markovHash N bins (hash : Hash128) (dna : Dna) i =
  if i >= N then
    hash.hash(dna.[i].semanticId)
    for j = i - N to i - 1 do
      hash.hash(dna.[j].semanticId)
      hash.hash(valueBinner bins dna j)
    true
  else false


/// Hashes the semantics and value of the parameter in the same or parent node whose number is target minus delta.
/// Use case: a more distant parameter than the previous has an important correlation with the target.
let deltaHash delta bins (hash : Hash128) (dna : Dna) i =
  let j = i - delta
  if j >= 0 then
    let thisAddress = dna.[i].address
    let parentAddress = thisAddress.parent
    if dna.[j].address = thisAddress || dna.[j].address = parentAddress then
      hash.hash(dna.[i].semanticId)
      hash.hash(dna.[j].semanticId)
      hash.hash(valueBinner bins dna j)
      true
    else false
  else false


/// Hashes the semantics and value of the parent parameter.
let parentHash bins (hash : Hash128) (dna : Dna) i =
  match dna.[i].parent with
  | Someval(j) ->
    hash.hash(dna.[i].semanticId)
    hash.hash(dna.[j].semanticId)
    hash.hash(valueBinner bins dna j)
    true
  | Noneval ->
    false


/// Hashes the semantics and value of the grandparent parameter.
let grandparentHash bins (hash : Hash128) (dna : Dna) i =
  match dna.[i].parent with
  | Someval(j) ->
    match dna.[j].parent with
    | Someval(k) ->
      hash.hash(dna.[i].semanticId)
      hash.hash(dna.[k].semanticId)
      hash.hash(valueBinner bins dna k)
      true
    | Noneval ->
      false
  | Noneval ->
    false


/// Hashes the semantics and values of the parent and grandparent parameters.
let parentGrandparentHash bins (hash : Hash128) (dna : Dna) i =
  match dna.[i].parent with
  | Someval(j) ->
    match dna.[j].parent with
    | Someval(k) ->
      hash.hash(dna.[i].semanticId)
      hash.hash(dna.[j].semanticId)
      hash.hash(valueBinner bins dna j)
      hash.hash(dna.[k].semanticId)
      hash.hash(valueBinner bins dna k)
      true
    | Noneval ->
      false
  | Noneval ->
    false


/// Hashes the semantics and value of N parameters starting from the parent parameter.
let parentContextHash N bins (hash : Hash128) (dna : Dna) i =
  match dna.[i].parent with
  | Someval(j0) ->
    hash.hash(dna.[i].semanticId)
    for j = j0 to min (j0 + N - 1) (i - 1) do
      hash.hash(dna.[j].semanticId)
      hash.hash(valueBinner bins dna j)
    true
  | Noneval ->
    false


/// Hashes the semantics and values of parameters back to, and including, the given ancestor
/// (1 = parent, 2 = grandparent, etc.)
let ancestorContextHash ancestorLevel bins (hash : Hash128) (dna : Dna) i =
  let rec trackAncestor levels j =
    if levels = 0 then Someval(j) else match dna.[j].parent with | Someval(j') -> trackAncestor (levels - 1) j' | Noneval -> Noneval
  match trackAncestor ancestorLevel i with
  | Someval(i0) ->
    hash.hash(dna.[i].semanticId)
    for j = i - 1 downto i0 do
      hash.hash(dna.[j].semanticId)
      hash.hash(valueBinner bins dna j)
    true
  | Noneval ->
    false
      


/// A parameter filter selects which context parameters to include in a hash of a target parameter.
/// The arguments are Dna, target parameter, and context parameter. Returns whether context parameter is accepted.
type ParameterFilter = Dna -> int -> int -> bool


/// If the Hamming distance between semanticTarget and (lowest 32 bits of) parameter semantic hash
/// is at most hammingLimit, accept parameter.
let hammingFilter semanticTarget hammingLimit (dna : Dna) _ context =
  Bits.hamming32 (uint semanticTarget) (uint dna.[context].semanticId) <= hammingLimit


/// Accepts parameters that have the same semantics as the target.
let similarFilter (dna : Dna) target context =
  dna.[context].semanticId = dna.[target].semanticId


/// Accepts parameters that do not have the same semantics as the target.
let dissimilarFilter (dna : Dna) target context =
  dna.[context].semanticId <> dna.[target].semanticId


/// Accepts parameters that have the same semantics as the parameter whose index is target minus delta.
let similarDeltaFilter delta (dna : Dna) target context =
  let deltaTarget = target - delta
  deltaTarget >= 0 && dna.[context].semanticId = dna.[deltaTarget].semanticId


/// Accepts categorical parameters.
let categoricalFilter (dna : Dna) _ context = dna.[context].format = Categorical


/// Accepts ordered parameters.
let orderedFilter (dna : Dna) _ context = dna.[context].format = Ordered


/// Accepts parameters in a sibling node.
let siblingFilter (dna : Dna) target context =
  let a1 = dna.[target].address
  let a2 = dna.[context].address
  a1 <> a2 && a1.parent = a2.parent


/// Accepts parameters in the parent node.
let parentFilter (dna : Dna) target context =
  dna.[context].address = dna.[target].address.parent


/// Accepts parameters in the grandparent node.
let grandparentFilter (dna : Dna) target context =
  dna.[context].address = dna.[target].address.ancestor(2)


/// Accepts parameters in any ancestor node.
let ancestorFilter (dna : Dna) target context =
  let delta = dna.[target].level - dna.[context].level
  delta > 0 && dna.[context].address = dna.[target].address.ancestor(delta)


/// Accepts parameters from the same node as the target.
let nodeFilter (dna : Dna) target context =
  dna.[context].address = dna.[target].address


/// Accepts any parameter.
let anyFilter (dna : Dna) _ _ = true


/// Logical AND of two filters.
let andFilter (filter1 : ParameterFilter) (filter2 : ParameterFilter) (dna : Dna) target context = filter1 dna target context && filter2 dna target context


/// Hashes (exactly) N context parameters inside window that all pass the given parameter filter.
let filterHash
  (window : int) // number of parameters to backtrack from target to find context
  (N : int) // number of context parameters to hash
  (filter : ParameterFilter)
  (bins : int)
  (hash : Hash128) (dna : Dna) i =
  let j0 = max 0 (i - window)
  let mutable n = 0
  let mutable j = i - 1
  while n < N && j >= j0 do
    if filter dna i j then
      hash.hash(dna.[j].semanticId)
      hash.hash(valueBinner bins dna j)
      n <- n + 1
    j <- j - 1
  if n < N then
    false
  else
    hash.hash(dna.[i].semanticId)
    true


/// Generates a parameter filter.
let genParameterFilter (dna : Dna) =
  // Parameter filters are a combination of content and position filters.
  // Generate content filter first.
  let content =
    dna.branch("Content filter",
      C(2.0, "Hamming filter", fun (dna : Dna) -> 
        let target = dna.data("Semantic target")
        let limit = dna.int("Hamming limit", 12, 15)
        hammingFilter target limit),
      C(1.5, "Similar filter", fun _ -> similarFilter),
      C(0.5, "Dissimilar filter", fun _ -> dissimilarFilter),
      C(0.5, "Similar delta filter", fun (dna : Dna) -> similarDeltaFilter (dna.int("Similar delta", 1, 3))),
      C(1.0, "Categorical filter", fun _ -> categoricalFilter)
      )
  // Generate position filter.
  let position =
    dna.branch("Position filter",
      C(1.0, "Sibling filter", fun _ -> siblingFilter),
      C(1.0, "Parent filter", fun _ -> parentFilter),
      C(1.0, "Grandparent filter", fun _ -> grandparentFilter),
      C(1.0, "Ancestor filter", fun _ -> ancestorFilter),
      C(1.0, "Node filter", fun _ -> nodeFilter),
      C(3.0, "No filter", fun _ -> anyFilter)
      )
  andFilter content position


/// Generates a bin count. The counts are primes in order to produce non-overlapping partition points.
let genBinCount (dna : Dna) = dna.int("Bins", 6, fun i -> [| 2; 3; 5; 7; 11; 17 |].[i])


/// Generates a parameter hash.
let genParameterHash (dna : Dna) =
  dna.branch("Parameter hash",
    C(2.5, "Structural hash", fun _ -> structuralHash),
    C(2.5, "Semantic hash", fun _ -> semanticHash),
    C(2.5, "Filter hash", fun (dna : Dna) -> filterHash 32 (dna.int("Context parameter count", 1, 2)) (dna.descend("Parameter filter", genParameterFilter)) (genBinCount dna)),
    C(2.0, "Family address hash", fun (dna : Dna) ->
      let generations = dna.int("Family generations", 2, 4)
      let phase = dna.int("Family phase", generations)
      familyAddressHash generations phase),
    C(2.0, "Markov hash", fun (dna : Dna) -> markovHash (dna.ordered("Markov order", C(1.0, 1), C(Q 1 2, 2), C(Q 1 3, 3))) (genBinCount dna)),
    C(1.5, "Local address hash", fun (dna : Dna) -> localAddressHash (dna.int("Address levels", 1, 3))),
    C(1.0, "Delta hash", fun (dna : Dna) -> deltaHash (dna.int("Index delta", 2, 4)) (genBinCount dna)),
    C(1.0, "Parent hash", fun (dna : Dna) -> parentHash (genBinCount dna)),
    C(0.5, "Parent context hash", fun (dna : Dna) -> parentContextHash (dna.int("Context size", 2, 3)) (genBinCount dna)),
    C(0.5, "Parent-grandparent hash", fun (dna : Dna) -> parentGrandparentHash (genBinCount dna)),
    C(0.5, "Grandparent hash", fun (dna : Dna) -> grandparentHash (genBinCount dna))
    )


/// Runs a parameter hash for a parameter. Returns success. The 128-bit hash is ready to use if successful.
let runHash (hash : Hash128) dna i (parameterHash : DnaParameterHash) =
  hash.reset()
  if parameterHash hash dna i then
    hash.hashEnd()
    true
  else false


/// Computes a distance between two genotypes, as seen through the given parameter hash.
/// Returns distance in unit range. This is a simple pairwise match - if the parameter hash
/// can produce intra-Dna duplicates, then a more advanced measure such as edit distance may be needed
/// to handle each duplicate set.
let dnaDistance (dnaA : Dna) (dnaB : Dna) (parameterHash : DnaParameterHash) =
  let hash = Hash128.create()

  let runDna (dna : Dna) =
    // We handle ordered and categorical parameters separately.
    let ord = Darray<Pair<int64, float>>.create()
    let cat = Darray<Pair<int64, uint>>.create()
    for i = 0 to dna.last do
      let parameter = dna.parameter(i)
      if parameter.format = DnaParameterFormat.Ordered then
        if runHash hash dna i parameterHash then ord.add(Pair(hash.a64, float parameter.value / float (max 1u parameter.maxValue)))
      else
        if runHash hash dna i parameterHash then cat.add(Pair(hash.a64, parameter.value))
    ord.sortBy id
    cat.sortBy id
    (ord, cat)

  let aord, acat = runDna dnaA
  let bord, bcat = runDna dnaB

  let mutable distance = 0.0

  // Calculate ordered parameter distances.
  let mutable a = 0
  let mutable b = 0
  while a < aord.size && b < bord.size do
    if aord.[a].x = bord.[b].x then
      // The square root norm is a pragmatic guess at a suitable distance function.
      distance <- distance + 2.0 * sqrt(abs(aord.[a].y - bord.[b].y))
      a <- a + 1
      b <- b + 1
    else
      distance <- distance + 1.0
      if aord.[a].x < bord.[b].x then a <- a + 1 else b <- b + 1
  distance <- distance + float (aord.size - a + bord.size - b)

  // Calculate categorical parameter distances.
  let mutable a = 0
  let mutable b = 0
  while a < acat.size && b < bcat.size do
    if acat.[a].x = bcat.[b].x then
      // We grant a small structural bonus here, as the keys are the same, even though the values do not match.
      if acat.[a].y <> bcat.[b].y then distance <- distance + 1.8
      a <- a + 1
      b <- b + 1
    else
      distance <- distance + 1.0
      if acat.[a].x < bcat.[b].x then a <- a + 1 else b <- b + 1
  distance <- distance + float (acat.size - a + bcat.size - b)

  if distance > 0.0 then
    distance / float (aord.size + bord.size + acat.size + bcat.size)
  else
    0.0

