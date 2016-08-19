// Rough-and-ready actions for easy biasing of procedural generators.
namespace FsMap3

open Common


[<NoComparison; NoEquality>]
type ParameterAction =
  /// Retain the previous value, if it exists. Otherwise pick a random value.
  | Retain
  /// Select a random value. If a previous value exists, avoid selecting it.
  | Randomize
  /// Select a default value. If the parameter comes with a prior, select the value
  /// with the largest weight. Otherwise, for ordered parameters, pick the middlemost value;
  /// for categorical parameters, pick the first value.
  | SelectDefault
  /// Attempt to select a specific value. If not possible, pick a random value.
  | Select of value : uint
  /// Attempt to select a specific choice. If not possible, pick a random value.
  | SelectChoice of name : string
  /// If ordered, attempt to select a specific fractional value (in unit range). If categorical, randomize.
  | Select01 of value : float
  /// If ordered, alter value by a fraction no larger than the given amount (0 < amount <= 1).
  /// If categorical, randomize.
  | Jolt01 of amount : float
  /// If ordered, adjust value fractionally by delta (-1 <= delta <= 1), always changing the value
  /// if delta <> 0. If categorical, cycle value in the direction indicated by delta.
  | Adjust01 of delta : float
  /// Modify existing transformed float with the function. Pick the closest legal value.
  /// In the absence of a previous value, transform a random value.
  | ModifyFloat of f : (float -> float)
  /// Attempt to select a transformed float. Pick the closest legal value.
  | SelectFloat of x : float

  member private this.applyCategorical(rnd : Rnd, dna : Dna, i, choices : Choices<_> option, existing : uint Optionval) =
    let maxValue = dna.parameter(i).maxValue
    let existing = existing.filter(match choices with | Some(choices) -> choices.isLegal | None -> dna.[i].isLegal)
    let pickAny() = match choices with | Some(choices) -> choices.pick(rnd.float()) | None -> rnd.uint(0u, maxValue)
    match this with
    | Retain ->
      match existing with | Someval(v) -> v | _ -> pickAny()
    | Randomize | Select01 _ | ModifyFloat _ | SelectFloat _ ->
      pickAny()
    | SelectDefault ->
      match choices with
      | Some(choices) -> choices.pickDefault()
      | None -> 0u
    | Select v ->
      match choices with
      | Some(choices) -> if choices.isLegal(v) then v else pickAny()
      | None -> if v <= maxValue then v else pickAny()
    | SelectChoice name ->
      match choices with
      | Some(choices) -> match Fun.findArg 0 choices.last (fun i -> choices.weight(i) > 0G && choices.name(i) = name) with
                         | Someval(i) -> uint i
                         | Noneval -> pickAny()
      | None -> pickAny()
    | Jolt01 _ ->
      match choices, existing with
      | Some(choices), Someval(v) -> choices.pickExcluding(v, rnd.float())
      | _ -> pickAny()
    | Adjust01 delta ->
      match existing with
      | Someval(v) ->
        match choices with
        | Some(choices) ->
          let wrap x = if x > maxValue then x - maxValue - 1u else x
          match (if delta > 0.0 then Fun.find else Fun.findBack) 1u maxValue (fun offset -> wrap (v + offset)) (fun v -> choices.weight(v) > 0.0) with
          | Someval(v') -> v'
          | Noneval -> v
        | None ->
          match sign delta, v with
          |  1, Eq(maxValue) -> 0u
          | -1, 0u           -> maxValue
          |  s, _            -> v + uint s
      | Noneval -> pickAny()

  member private this.applyOrdered(rnd : Rnd, dna : Dna, i, choices : Choices<_> Option, valueTransform : (uint -> float) option, priorTransform : (uint -> float) option, existing : uint Optionval) =
    let maxValue = dna.parameter(i).maxValue
    let range = float maxValue + 1.0
    let existing = existing.filter(match choices with | Some(choices) -> choices.isLegal | None -> dna.[i].isLegal)
    let pickAny() =
      match choices, valueTransform, priorTransform with
      | Some(choices), _, _ ->
        choices.pick(rnd.float())
      | _, Some(valueTransform), Some(priorTransform) ->
        let v = rnd.uint(0u, maxValue)
        Fun.binarySearchClosest 0u maxValue valueTransform (priorTransform v)
      | _ ->
        rnd.uint(0u, maxValue)
    let tryPick(v : uint) =
      match choices with
      | Some(choices) -> if choices.isLegal(v) then v else pickAny()
      | None -> if v <= maxValue then v else pickAny()
    match this with
    | Retain ->
      match existing with | Someval(v) -> v | Noneval -> pickAny()
    | Randomize ->
      pickAny()
    | SelectDefault ->
      match choices with
      | Some(choices) -> choices.pickDefault()
      | None -> maxValue / 2u
    | Select v ->
      tryPick(v)
    | SelectChoice name ->
      match choices with
      | Some(choices) -> match Fun.findArg 0 choices.last (fun i -> choices.weight(i) > 0G && choices.name(i) = name) with
                         | Someval(i) -> uint i
                         | Noneval -> pickAny()
      | None -> pickAny()
    | Select01 v ->
      tryPick(lerp 0.0 (float maxValue) v |> clamp 0.1 (float maxValue - 0.1) |> uint)
    | Jolt01 amount ->
      match existing with
      | Someval(v) ->
        let v' = uint <| lerp (max 0.01 (float v - range * amount)) (min (float maxValue - 0.01) (float v + range * amount)) (rnd.float())
        tryPick(if v' <> v then v' else rnd.choose((if v > 0u then 1.0 else 0.0), v - 1u, (if v < maxValue then 1.0 else 0.0), v + 1u))
      | _ -> pickAny()
    | Adjust01 delta ->
      match existing with
      | Someval(v) ->
        match choices with
        | Some(choices) ->
          if delta > 0.0 && v < maxValue then
            match Fun.findArg (v + 1u) maxValue (fun v -> choices.weight(v) > 0.0) with
            | Someval(v') -> v'
            | Noneval -> v
          elif delta < 0.0 && v > 0u then
            match Fun.findArgBack 0u (v - 1u) (fun v -> choices.weight(v) > 0.0) with
            | Someval(v') -> v'
            | Noneval -> v
          else v
        | None ->
          if delta < 0.0 && v > 0u then
            tryPick(uint <| clamp 0.1 (float v - 0.1) (float v + 0.5 + range * delta))
          elif delta > 0.0 && v < maxValue then
            tryPick(uint <| clamp (float v + 1.1) (float maxValue - 0.1) (float v + 0.5 + range * delta))
          else v
      | _ -> pickAny()
    | ModifyFloat f ->
      match valueTransform with
      | Some(transform) ->
        let v = match existing with | Someval(v) -> v | Noneval -> pickAny()
        Fun.binarySearchClosest 0u maxValue transform (v |> transform |> f)
      | _ -> pickAny()
    | SelectFloat x ->
      match valueTransform with
      | Some(transform) ->
        Fun.binarySearchClosest 0u maxValue transform x
      | _ -> pickAny()

  /// Applies this action with a set of choices.
  member this.apply(rnd, dna : Dna, i, choices : Choices<_>, existing) =
    match choices.singular with
    | Someval(v) ->
      uint v
    | Noneval ->
      match dna.[i].format with
      | Categorical -> this.applyCategorical(rnd, dna, i, Some(choices), existing)
      | Ordered -> this.applyOrdered(rnd, dna, i, Some(choices), None, None, existing)

  /// Applies this action with a uniform prior.
  member this.apply(rnd, dna : Dna, i, existing) =
    if dna.[i].maxValue = 0u then
      0u
    else
      match dna.[i].format with
      | Categorical -> this.applyCategorical(rnd, dna, i, None, existing)
      | Ordered -> this.applyOrdered(rnd, dna, i, None, None, None, existing)

  /// Applies this action with the given float transformations.
  member this.apply(rnd, dna : Dna, i, valueTransform, priorTransform, existing) =
    if dna.[i].maxValue = 0u then
      0u
    else
      match dna.[i].format with
      | Categorical -> this.applyCategorical(rnd, dna, i, None, existing)
      | Ordered -> this.applyOrdered(rnd, dna, i, None, Some(valueTransform), Some(priorTransform), existing)



/// A parameter predicate decides what the overall plan is with each Dna parameter during generation.
type ParameterPredicate = Rnd -> Dna -> int -> ParameterAction

