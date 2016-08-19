// Human friendly serialization of Dna genotypes.
namespace FsMap3

open Common


/// A Dna source that serializes genotypes in a YAML-like format.
type SerializerSource(genotype : Dna) =
  inherit DnaSource()

  let data = Array.init genotype.size (fun i -> genotype.[i].value)
  let builder = System.Text.StringBuilder()
  let mutable yaml = ""

  let indent (dna : Dna) = dna.level * 2

  override this.ready(dna) =
    yaml <- builder.ToString()
    builder.Clear() |> ignore

  override this.choose(dna, i, choices) =
    enforce (i < data.size && data.[i] <= dna.[i].maxValue && choices.weight(data.[i]) > 0.0) "SerializerSource.choose: Data mismatch."
    builder.AppendLine(sprintf "%*s%s: %s" (indent dna) "" dna.[i].name (choices.name(data.[i]))) |> ignore
    data.[i]

  override this.choose(dna, i) =
    enforce (i < data.size && data.[i] <= dna.[i].maxValue) "SerializerSource.choose: Data mismatch."
    if dna.[i].maxValue = 0u then
      builder.AppendLine(sprintf "%*s%s:" (indent dna) "" dna.[i].name) |> ignore
    else
      builder.AppendLine(sprintf "%*s%s: %d" (indent dna) "" dna.[i].name data.[i]) |> ignore
    data.[i]

  override this.chooseInt(dna, i, transform, _) =
    enforce (i < data.size && data.[i] <= dna.[i].maxValue) "SerializerSource.chooseInt: Data mismatch."
    builder.AppendLine(sprintf "%*s%s: %d" (indent dna) "" dna.[i].name (transform data.[i])) |> ignore
    data.[i]
    
  override this.chooseFloat(dna, i, transform, _) =
    let v = data.[i]
    enforce (i < data.size && v <= dna.[i].maxValue) "SerializerSource.chooseFloat: Data mismatch."
    let x = transform v
    let x0 = if v = 0u then infinity else transform (v - 1u)
    let x1 = if v = dna.[i].maxValue then infinity else transform (v + 1u)
    let retainDecimals digits x = let p = pow 10.0 digits in truncate (x * p) / p
    // We want to serialize floats with sufficient precision that the raw parameter value
    // can be recovered.
    let isAdmissiblePrecision digits =
      let a = retainDecimals digits x
      let error = abs(x - a)
      error < abs(x0 - a) && error < abs(x1 - a)
    let mutable precision = if x = 0.0 then 1 else max 1.0 (1.0 - log10 (abs x)) |> int
    while isAdmissiblePrecision precision = false && precision < 15 do
      precision <- precision + 1
    builder.AppendLine(sprintf "%*s%s: %.*g" (indent dna) "" dna.[i].name precision (retainDecimals precision x)) |> ignore
    v

  member this.yamlString = yaml



[<NoEquality; NoComparison>]
type YamlRecord =
  {
    level : int
    name : string
    value : string
    uintValue : uint option
    intValue : int option
    floatValue : float option
  }



/// A Dna source that deserializes genotypes from the YAML-like format
/// produced by SerializerSource.
type DeserializerSource(readLine : unit -> string option) =
  inherit DnaSource()

  /// Returns a longest common subsequence similarity score between two strings.
  /// The score is in unit range.
  let similarity (x : string) (y : string) =
    if x = y then 1.0 else
      1.0 - Fun.editDistance x.size (fun i -> x.[i]) y.size (fun i -> y.[i]) 1.0 1.0 (fun a b -> if a = b then 0.0 else infinity) / (float x.size + float y.size)

  let selection = Darray.create()

  let fillSelection i = while selection.size < i do selection.add(Noneval)

  let record =
    let indentStack = Darray.createSingle(0)
    let rec loop() = seq {
      match readLine() with
      | Some(line) ->
        match line with
        | Regex("^( *)(.*\S) *: *([^#]*[^ #])? *(?:#.*)?$") [ indent; name; value ] ->
          let indent = indent.size
          let uintValue = match value, System.UInt32.TryParse(value) with | _, (true, x) -> Some(x) | "", _ -> Some(0u) | _ -> None
          let intValue = match System.Int32.TryParse(value) with | true, x -> Some(x) | _ -> None
          let floatValue = match System.Double.TryParse(value) with | true, x -> Some(x) | _ -> None
          while indent < indentStack.lastItem do indentStack.pop()
          if indent > indentStack.lastItem then indentStack.push(indent)
          yield { YamlRecord.level = indentStack.last; name = name; value = value; uintValue = uintValue; intValue = intValue; floatValue = floatValue }
        | _ -> ()
        yield! loop()
      | _ -> ()
    }
    loop() |> Seq.toArray

  /// Parents of each YAML record.
  let parent =
    Array.init record.size (fun i ->
      let rec seek j =
        if j < 0 then Noneval else
          match record.[i].level - record.[j].level with
          | 1 -> Someval(j)
          | x when x <= 0 -> seek (j - 1)
          | _ -> Noneval
      seek (i - 1)
      )

  /// Some kind of heuristic score that helps matching YAML records to Dna parameters.
  /// The intention is to give us a chance to recover a serialized genotype
  /// even when there have been changes to the corresponding generator.
  let heuristicScore (dna : Dna) i j =
    let rec parentScore i j generation score =
      if parent.[j].isSome && dna.[i].hasParent then
        let j' = !parent.[j]
        let i' = !dna.[i].parent
        let selectionScore = match selection.[i'] with | Someval(x) when x = j' -> 1.0 | _ -> 0.0
        parentScore i' j' (generation + 1) (score + (selectionScore + similarity record.[j'].name dna.[i'].name) / pow 2.0 generation)
      else score
    let mutable score = 0.0
    score <- score + 4.0 * similarity record.[j].name dna.[i].name
    if record.[j].level = dna.[i].level then score <- score + 0.5
    score <- score + parentScore i j 1 0.0
    score

  override this.ready(dna) =
    selection.reset()

  override this.choose(dna, i, choices) =
    fillSelection i

    let mutable bestChoice = choices.pickDefault()
    let mutable bestRecord = Noneval
    let mutable bestScore  = 0.0

    for j = 0 to record.last do
      let mutable score = heuristicScore dna i j
      let (Pair(choice, choiceScore)) =
        Fun.maxBy 0 choices.last
          (fun k -> Pair(k, if choices.weight(k) > 0.0 then similarity (choices.name(k)) record.[j].value else 0.0))
          (fun (Pair(choice, score)) -> score)
      if choiceScore > 0.0 then score <- score + 2.0 * choiceScore else score <- -infinity

      if score > bestScore then
        bestChoice <- uint choice
        bestRecord <- Someval(j)
        bestScore <- score

    selection.add(bestRecord)
    bestChoice

  override this.choose(dna, i) =
    fillSelection i

    let mutable bestChoice = 0u
    let mutable bestRecord = Noneval
    let mutable bestScore  = 0.0

    if dna.[i].maxValue > 0u then
      for j = 0 to record.last do
        if record.[j].uintValue.isSomeAnd(dna.[i].isLegal) then
          let mutable score = heuristicScore dna i j
          if score > bestScore then
            bestChoice <- !record.[j].uintValue
            bestRecord <- Someval(j)
            bestScore <- score

    selection.add(bestRecord)
    bestChoice

  override this.chooseInt(dna, i, transform, _) =
    fillSelection i

    let mutable bestValue  = 0u
    let mutable bestRecord = Noneval
    let mutable bestScore  = 0.0

    for j = 0 to record.last do
      match record.[j].intValue with
      | Some(v) ->
        let mutable score = heuristicScore dna i j
        let v' = Fun.binarySearchClosest 0u dna.[i].maxValue transform v
        score <- score + 2.0 / (1.0 + float (abs (transform v' - v)))
        if score > bestScore then
          bestValue <- v'
          bestRecord <- Someval(j)
          bestScore <- score
      | _ -> ()

    selection.add(bestRecord)
    bestValue
    
  override this.chooseFloat(dna, i, transform, _) =
    fillSelection i

    let mutable bestValue  = 0u
    let mutable bestRecord = Noneval
    let mutable bestScore  = 0.0

    for j = 0 to record.last do
      match record.[j].floatValue with
      | Some(v) ->
        let mutable score = heuristicScore dna i j
        let v' = Fun.binarySearchClosest 0u dna.[i].maxValue transform v
        score <- score + 2.0 / (1.0 + abs (transform v' - v))
        if score > bestScore then
          bestValue <- v'
          bestRecord <- Someval(j)
          bestScore <- score
      | _ -> ()

    selection.add(bestRecord)
    bestValue
    

