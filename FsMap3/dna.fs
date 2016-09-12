// A generic parameter space for procedural generators.
namespace Fuse

(*
Dna System Overview

Procedural generators draw trees of parameters to generate objects. Parameter values are converted
from raw 32-bit data values. Given parameter data, the output of a generator is deterministic.

Raw data is provided by DnaSource objects. These can implement pseudo-random data, replicated data,
or sophisticated sampling and optimization methods. DnaSource has a feedback mechanism for guiding evolution.

As parameters are drawn, their details are recorded in the genotype, including tree structure
and parameter grouping. The details can be printed or visualized easily.

One of the advantages of the approach is that implementation of generators is easy: only the generator function
needs to be written (and a visualizer, if GUI interaction is desired). Mutation, optimization, serialization
and so on operate on parameter data, for which generic facilities are available. (In genetic programming,
this approach is known as "Grammar Evolution"). 

There is some overhead because we have to generate an object from Dna each time we need it. In optimization
these overheads are typically insignificant compared to fitness evaluation, and in user interfaces they are
unnoticeable.

Contrast this with a custom procedural approach, for example, an expression tree engine. A custom engine can
be more powerful but also more costly to implement. Not only generation functions need to be implemented
for the engine, but also custom mutation, crossover and serialization facilities for the resulting trees. 

The generator can attach a "prior" distribution, in the form of weights, to a set of parameter values.
One of the main uses of weights is in implementing dynamic constraints: if some value is not possible
at some point during generation (for example, a branching operator in an expression tree whose maximum height
has been reached), its weight is set to zero. This way, the meaning of the values does not change,
which is important when we want to slice and dice pieces of different genotypes into new organisms.

Given a generator, the meaning of the values of a parameter should be uniquely identifiable
from the format, name and range of the parameter. This is summarized in a semantic ID value.
*)


open Common
open Tome
open Convert
open Mangle


/// Level state for a Dna object. Internal type.
[<NoComparison; NoEquality>]
type DnaLevelState = struct
  /// Current node address on this level.
  val address : DnaAddress
  /// Index of latest parameter drawn on this level.
  val parameterIndex : int Optionval
  /// Current number of child nodes (equals the sibling number of next child).
  val children : int
  /// Number of parameters in this node (equals the number of next parameter).
  val number : int

  new(address, parameterIndex, children, number) = { address = address; parameterIndex = parameterIndex; children = children; number = number }
end



/// Dna objects store genotypes, which consist of parameters placed in a tree structure.
/// Dna objects also provide an interface for procedural generators to draw parameters, the specifics of which
/// are stored for later access for, e.g., visualization, optimization, serialization or replication.
type [<ReferenceEquality>] Dna =
  {
    /// Dna parameters. These define the genotype.
    parameterArray : DnaParameter Darray

    /// Fingerprint of the genotype.
    mutable fingerprint : int64

    /// The data source for this generation. Used during generation only.
    mutable source : DnaSource Optionval

    /// Current set of injectors for this generation. Used during generation only.
    injectorArray : DnaInjector Darray

    /// Level states. The array always contains at least the root level during generation.
    /// Used during generation only.
    state : DnaLevelState Darray
  }


  // PUBLIC ACCESSORS


  member inline this.size = this.parameterArray.size
  member inline this.last = this.parameterArray.last
  member inline this.parameter(i) = this.parameterArray.[i]
  member inline this.parentParameter(i) = this.parameterArray.[i].parent.map(this.parameter)
  member inline this.lastParameter = this.parameterArray.[this.last]
  member inline this.Item with get(i) = this.parameterArray.[i]

  /// Current level (root level is zero). Valid during generation only.
  member inline this.level = this.state.size - 1

  /// Current sibling number on level i (i must be equal to or smaller than the current level).
  /// At root level - zero - the sibling number is always zero, i.e., there is only one root node.
  member this.siblingNumberOnLevel(i) = if i > 0 then this.state.[i - 1].children else 0

  /// Returns whether parameters i and j are on the same path starting from root.
  member this.isSameBranch(i, j) =
    if this.[i].level < this.[j].level then
      this.[j].address.ancestor(this.[j].level - this.[i].level) = this.[i].address
    else
      this.[i].address.ancestor(this.[i].level - this.[j].level) = this.[j].address

  /// Returns whether parameter i is in an ancestor node of the node containing parameter j.
  member this.isAncestor(i, j) =
    this.[i].level < this.[j].level && this.[j].address.ancestor(this.[j].level - this.[i].level) = this.[i].address

  /// Returns whether parameter i is located in the subtree rooted at the node containing parameter j.
  member this.isInSubtree(i, j) =
    this.[i].level >= this.[j].level && this.[i].address.ancestor(this.[i].level - this.[j].level) = this.[j].address


  // INTERNAL METHODS


  /// Does bookkeeping after all parameter attributes have been set. Updates the Dna fingerprint
  /// and the subtree fingerprint of the parent parameter (if any).
  /// This is called exactly once per created parameter.
  member private this.updateFingerprint(parameter : DnaParameter) =
    this.fingerprint <- mangle64 (this.fingerprint + parameter.structuralId)
    this.fingerprint <- mangle64 (this.fingerprint + int64 parameter.value)
    parameter.parent.apply(fun i -> this.[i].updateSubtreeId(parameter))


  /// Chooses a value for a new parameter using the given monotonic transforms.
  member private this.choose(valueTransform : uint -> 'a, priorTransform : uint -> 'a) =
    let rec tryFilter i =
      if i < 0 then Noneval else
        match this.injectorArray.[i].choose(this, this.last) with
        | Noneval -> tryFilter (i - 1)
        | x -> x
    let value =
      match tryFilter this.injectorArray.last with
      | Noneval ->
        match box valueTransform with
        | :? (uint -> int) as valueTransform -> (!this.source).chooseInt(this, this.last, valueTransform, priorTransform |> box |> unbox)
        | :? (uint -> float) as valueTransform -> (!this.source).chooseFloat(this, this.last, valueTransform, priorTransform |> box |> unbox)
        | _ -> (!this.source).choose(this, this.last)
      | Someval(x) ->
        x
    enforce (value <= this.lastParameter.maxValue) "Dna.choose: DnaSource returned an out-of-bounds value."
    value


  /// Chooses a value for a new parameter.
  member private this.choose() =
    let rec tryFilter i =
      if i < 0 then Noneval else
        match this.injectorArray.[i].choose(this, this.last) with
        | Noneval -> tryFilter (i - 1)
        | x -> x
    let value =
      match tryFilter this.injectorArray.last with
      | Noneval -> (!this.source).choose(this, this.last)
      | Someval(x) -> x
    enforce (value <= this.lastParameter.maxValue) "Dna.choose: DnaSource returned an out-of-bounds value."
    value


  /// Chooses a value for a new parameter from a set of choices.
  member private this.choose(choices : Choices<_>) =
    enforce (choices.total > 0G) "Dna.choose: Set of choices has zero weight."
    let value =
      let rec tryFilter i =
        if i < 0 then Noneval else
          match this.injectorArray.[i].choose(this, this.last, choices) with
          | Noneval -> tryFilter (i - 1)
          | x -> x
      match tryFilter this.injectorArray.last with
      | Noneval ->
        (!this.source).choose(this, this.last, choices)
      | Someval(x) ->
        x
    enforce (value <= this.lastParameter.maxValue) "Dna.choose: DnaSource or DnaFilter returned an out-of-bounds value."
    enforce (choices.weight(value) > 0G) "Dna.choose: DnaSource or DnaFilter chose a zero weight value."
    value


  /// Creates a new parameter. If the prior transform is specified, then both transforms must be monotonic.
  /// The value can be optionally injected directly from the generator.
  member private this.addParameter(format, name, maximumValue, valueTransform : uint -> 'a, stringConverter : 'a -> string, ?priorTransform : uint -> 'a, ?value : uint) =
    let i = this.size
    let a = this.state.[this.level]
    let semanticId = DnaParameter.getSemanticId(format, name, maximumValue)
    let parameter = {
      format       = format
      name         = name
      maxValue     = maximumValue
      level        = this.level
      address      = a.address
      number       = a.number
      parent       = if this.level > 0 then this.state.[this.level - 1].parameterIndex else Noneval
      semanticId   = semanticId
      structuralId = DnaParameter.getStructuralId(semanticId, this.level, a.address, a.number)
      subtreeId    = semanticId
      value        = 0u
      valueString  = ""
      choices      = None
      generated    = None
      }
    this.parameterArray.add(parameter)
    parameter.value <- value >? match priorTransform with
                                | Some(priorTransform) -> this.choose(valueTransform, priorTransform)
                                | None -> this.choose()
    let generated = valueTransform parameter.value
    parameter.valueString <- stringConverter generated
    parameter.generated <- Some(box generated)
    this.updateFingerprint(parameter)
    this.state.[this.level] <- DnaLevelState(a.address, Someval(i), a.children, a.number + 1)
    generated


  /// Creates a new parameter with a set of choices.
  member private this.addParameter(format, name, choices : Choices<'a>) =
    let i = this.size
    let a = this.state.[this.level]
    let semanticId = DnaParameter.getSemanticId(format, name, choices.maximum)
    let parameter = {
      format       = format
      name         = name
      maxValue     = choices.maximum
      level        = this.level
      address      = a.address
      number       = a.number
      parent       = if this.level > 0 then this.state.[this.level - 1].parameterIndex else Noneval
      semanticId   = semanticId
      structuralId = DnaParameter.getStructuralId(semanticId, this.level, a.address, a.number)
      subtreeId    = semanticId
      value        = 0u
      valueString  = ""
      choices      = Some (choices :> IChoices)
      generated    = None
      }
    this.parameterArray.add(parameter)
    parameter.value <- this.choose(choices)
    let generated = choices.value(parameter.value)
    parameter.valueString <- choices.name(parameter.value)
    parameter.generated <- Some(box generated)
    this.updateFingerprint(parameter)
    this.state.[this.level] <- DnaLevelState(a.address, Someval(i), a.children, a.number + 1)
    generated


  /// Calls a subgenerator in a child node in the parameter tree.
  /// Creation of child branches is always done through this method.
  member private this.descend(generator : Dna -> _) =
    let a = this.state.[this.level]
    let subtreeRoot = this.parameterArray.lastItem
    this.state.push(DnaLevelState(a.address.child(a.children), Noneval, 0, 0))
    let result = generator this
    subtreeRoot.parent.apply(fun i -> this.[i].updateSubtreeId(subtreeRoot))
    this.state.pop()
    this.state.[this.level] <- DnaLevelState(a.address, a.parameterIndex, a.children + 1, a.number)
    result


  // PUBLIC INTERFACE


  /// Generates an object using the source.
  member this.generate(source, generator : Dna -> _) =
    this.parameterArray.reset()
    this.source <- Someval(source)
    this.injectorArray.reset()
    source.start()
    this.state.reset()
    this.state.push(DnaLevelState(DnaAddress.Root, Noneval, 0, 0))
    this.fingerprint <- 0L
    let result = generator this
    this.source <- Noneval
    this.injectorArray.reset()
    this.state.reset()
    source.ready(this)
    result


  /// Copies the genotype here from another Dna.
  member this.copyFrom(dna : Dna) =
    if dna <>= this then
      this.parameterArray.resize(dna.size)
      for i = 0 to dna.last do 
        this.parameterArray.[i] <- dna.parameter(i)
      this.fingerprint <- dna.fingerprint


  /// Resets this Dna to an empty genotype.
  member this.reset() =
    this.parameterArray.reset()
    this.fingerprint <- 0L


  /// Prints the genotype to stdout.
  member this.print() =
    printfn "Dna fingerprint: %016x" this.fingerprint
    for i = 0 to this.last do
      let p = this.[i]
      printfn "%*s %s: %s" p.level "" p.name p.valueString


  // PUBLIC INTERFACE FOR PROCEDURAL GENERATORS


  /// Adds a label parameter. Labels are parameters where the value contains no information.
  /// They can be used to inject custom information into Dna in the parameter name. They are also used
  /// as dummy parameters whose purpose is to make sure that every branch of the Dna tree has a root node.
  member this.addLabel(name) =
    let i = this.size
    let a = this.state.[this.level]
    let semanticId = DnaParameter.getSemanticId(Categorical, name, 0u)
    let parent = if this.level > 0 then this.state.[this.level - 1].parameterIndex else Noneval
    let parameter = {
      format       = Categorical
      name         = name
      maxValue     = 0u
      level        = this.level
      address      = a.address
      number       = a.number
      parent       = parent
      semanticId   = semanticId
      structuralId = DnaParameter.getStructuralId(semanticId, this.level, a.address, a.number)
      subtreeId    = semanticId
      value        = 0u
      valueString  = ""
      choices      = None
      generated    = None
      }
    this.parameterArray.add(parameter)
    this.choose(ignore, ignore) |> ignore
    this.updateFingerprint(parameter)
    this.state.[this.level] <- DnaLevelState(a.address, Someval(i), a.children, a.number + 1)


  /// Adds an injector. More recently added injectors have higher precedence.
  member this.addInjector(injector : DnaInjector) =
    this.injectorArray.add(injector)


  /// Removes the most recently added injector.
  member this.popInjector() =
    this.injectorArray.pop()


  /// Calls a subgenerator in a child node in the parameter tree. Adds a dummy parameter
  /// as a label for the subtree.
  member this.descend(label : string, generator : Dna -> _) =
    this.addLabel(label)
    this.descend(generator)


  /// Queries a float parameter. Both transform and prior from the unit range with the given interval type
  /// (closed by default) must be monotonic if specified.
  member this.float(name, ?transform : float -> float, ?prior : float -> float, ?interval, ?suffix, ?value) =
    let interval       = interval >? Closed
    let transform      = transform >? id
    let valueTransform = float01u interval >> transform
    let prior          = prior >? id
    let suffix         = suffix >? ""
    let value          = value.map(fun value -> Fun.binarySearchClosest 0u (maxValue uint) valueTransform value)
    this.addParameter(
      Ordered,
      name,
      maxValue uint,
      valueTransform,
      (fun x -> Pretty.string x + suffix),
      float01u interval >> prior >> transform,
      ?value = value
      )


  /// Queries a float32 parameter. Both transform and prior from the unit range with the given interval type
  /// (closed by default) must be monotonic if specified.
  member this.float32(name, ?transform, ?prior, ?interval, ?suffix, ?value : float32) =
    this.float(name, float32 >> (transform >? id) >> float, float32 >> (prior >? id) >> float, ?interval = interval, ?suffix = suffix, ?value = value.map(float)) |> float32


  /// Queries a categorical parameter.
  member this.category(name, choices : Choices<_>) =
    this.addParameter(Categorical, name, choices)


  /// Queries a categorical parameter.
  member this.category(name, [<System.ParamArray>] choices : C<_> array) =
    this.category(name, Choices(choices))


  /// Queries a categorical parameter that triggers a subgenerator.
  /// The subgenerator is invoked in a child node.
  member this.branch(name, choices : Choices<Dna -> _>) =
    this.descend(this.category(name, choices))


  /// Queries a categorical parameter that triggers a subgenerator.
  /// The subgenerator is invoked in a child node.
  member this.branch(name, [<System.ParamArray>] choices : C<Dna -> _> array) =
    this.branch(name, Choices(choices))


  /// Queries an ordered parameter from a set of choices.
  member this.ordered(name, choices : Choices<_>) =
    this.addParameter(Ordered, name, choices)


  /// Queries an ordered parameter from a set of choices.
  member this.ordered(name, [<System.ParamArray>] choices : C<_> array) =
    this.ordered(name, Choices(choices))


  /// Queries an int parameter in [minimum, maximum], with an optional monotonic transformation.
  member this.int(name, minimum : int, maximum : int, ?transform : int -> int) =
    enforce (minimum <= maximum) "Dna.int: Empty range."
    let maximumValue = uint maximum - uint minimum
    let valueTransform = (+) (uint minimum) >> int >> (transform >? id)
    this.addParameter(Ordered, name, maximumValue, valueTransform, string, valueTransform)


  /// Queries an int parameter in [0, range[.
  member this.int(name, range) =
    this.int(name, 0, range - 1)


  /// Queries an int parameter in [0, range[, with a monotonic transformation.
  member this.int(name, range, transform : int -> int) =
    this.int(name, 0, range - 1, transform)


  /// Queries a full range int parameter.
  member this.int(name) =
    this.int(name, minValue int, maxValue int)


  /// Queries "raw" categorical data in [0, range[ (or, by default, the full 32-bit range).
  /// The result is returned as an int (due to popular demand).
  member this.data(name, ?range : int) =
    enforce (range.isNone || !range > 0) "Dna.data: Empty range."
    let maximumValue = match range with | Some(r) -> uint r - 1u | None -> maxValue uint
    this.addParameter(
      Categorical,
      name,
      maximumValue,
      int,
      fun x ->
        let nybbles = match range with | Some(r) -> (Bits.highestBit32 (uint r - 1u) + 3) / 4 | None -> 8
        sprintf "%0*x" nybbles x
      )


  /// Queries a 32-bit word, which is converted into an object.
  member this.data(name, transform : int -> _) =
    let valueTransform = int >> transform
    this.addParameter(Categorical, name, maxValue uint, valueTransform, box >> string)


  /// Returns true or false.
  member this.boolean(name, ?trueProbability) =
    let p = trueProbability >? 0.5
    this.addParameter(Categorical, name, Choices(C(1.0 - p, "no", false), C(p, "yes", true)))


  /// Calls a subgenerator in a child node and creates clones of the returned object.
  /// Returns a total of n objects in an array. The parameters are drawn only once.
  member this.repeat(n, generator : Dna -> _) =
    let i = this.size
    let original = generator this
    let dna : Dna = Dna.create()
    let dataSource = DnaData(Array.init (this.size - i) (fun j -> this.parameter(i + j).value))
    Array.init n (fun i -> if i = 0 then original else dna.generate(dataSource, generator))


  // STATIC METHODS


  /// Creates a new Dna.
  static member create() =
    {
      parameterArray = Darray.create(autoTrim = false)
      injectorArray = Darray.create(autoTrim = false)
      source = Noneval
      state = Darray.create()
      fingerprint = 0L
    }

  /// Creates a copy of (the parameters of) the Dna.
  static member createCopy(dna : Dna) =
    let copy = Dna.create()
    copy.copyFrom(dna)
    copy

  /// Generates a phenotype from the given seed. Returns just the phenotype.
  static member generate(seed : int, generator : Dna -> _) =
    let dna = Dna.create()
    dna.generate(RandomSource(seed), generator)

  /// Generates a phenotype using the given Rnd. Returns just the phenotype.
  static member generate(rnd : Rnd, generator : Dna -> _) =
    let dna = Dna.create()
    dna.generate(RandomSource(rnd), generator)

  /// Generates a phenotype using the given source. Returns just the phenotype.
  static member generate(source : DnaSource, generator : Dna -> _) =
    let dna = Dna.create()
    dna.generate(source, generator)

  /// Generates n unique phenotypes (based on parameter fingerprints) using seed data from the given Rnd.
  /// Returns an array of phenotypes.
  static member generateUnique(n, rnd : Rnd, generator : Dna -> _) =
    let set = HashSet<int64>.create(int)
    Array.init n (fun _ ->
      snd <| doFind (fun _ ->
                      let dna = Dna.create()
                      dna, dna.generate(RandomSource(rnd), generator)
                      )
                    (fun (dna, _) -> if set.exists(dna.fingerprint) then
                                       false
                                     else
                                       set.add(dna.fingerprint)
                                       true)
      )



/// Dna injectors impose additional constraints to genotype generation on the go.
/// Injectors are added by generators. They will be consulted for parameter values before the source.
and [<AbstractClass>] DnaInjector() =
  abstract choose : Dna * int -> uint Optionval
  abstract choose : Dna * int * Choices<_> -> uint Optionval

  /// Creates an injector for a parameter with a uniform prior.
  static member create(f : Dna -> int -> uint Optionval) =
    { new DnaInjector() with
      member this.choose(dna, i) = f dna i
      member this.choose(_, _, _) = Noneval
    }

  /// Creates an injector for a parameter with a set of choices. Note that the type must be the specific type
  /// of the parameter value that is to be provided.
  static member create<'a>(f : Dna -> int -> Choices<'a> -> uint Optionval) =
    { new DnaInjector() with
      member this.choose(dna, i) = Noneval
      member this.choose(dna, i, choices : Choices<_>) =
        match box choices with
        | :? Choices<'a> as c -> f dna i c
        | _ -> Noneval
    }



/// Data for a genotype comes from a DnaSource. It features a reinforcement mechanism for optimization.
and [<AbstractClass>] DnaSource() =

  /// Called when phenotype generation is about to start.
  /// Only one phenotype is generated at a time and all calls are synchronous.
  abstract start : unit -> unit

  /// Called when generation of the current phenotype is complete.
  abstract ready : Dna -> unit

  /// Provides feedback on the degree of success of the genotype, in unit range -
  /// 0 is utter failure, 1 is absolute success. It is implied that the source provided
  /// the data for the genotype.
  abstract feedback : Dna * float -> unit

  /// Informs the source of the existence of a genotype and its fitness value.
  /// The fitness scale is arbitrary.
  abstract observe : Dna * float -> unit

  /// Chooses a value for a parameter from a set of weighted choices.
  /// The chosen value must have a non-zero weight. Note that it is possible that
  /// there is only one value with a non-zero weight.
  abstract choose : Dna * int * Choices<_> -> uint

  /// Chooses a value for a parameter with an implied uniform prior distribution.
  /// The maximum value can be zero: this is called even for dummy label parameters.
  abstract choose : Dna * int -> uint

  /// Chooses an ordered integer value for a parameter. The transformations are monotonic.
  /// This is routed to DnaSource.choose by default.
  abstract chooseInt : Dna * int * (uint -> int) * (uint -> int) -> uint

  /// Chooses an ordered float value for a parameter. The transformations are monotonic.
  /// This is routed to DnaSource.choose by default.
  abstract chooseFloat : Dna * int * (uint -> float) * (uint -> float) -> uint

  default __.start() = ()
  default __.ready(_) = ()
  default __.feedback(_, _) = ()
  default __.observe(_, _) = ()

  default this.chooseInt(dna, i, valueTransform, priorTransform) = this.choose(dna, i)
  default this.chooseFloat(dna, i, valueTransform, priorTransform) = this.choose(dna, i)

  /// Generates an object using this source.
  member this.generate(generator : Dna -> _) =
    let dna = Dna.create()
    dna.generate(this, generator)



/// A Dna source that can replicate an existing genotype.
and DnaData(data : uint[]) =
  inherit DnaSource()

  /// Constructs the source to replicate the given Dna.
  new(dna : Dna) = DnaData(Array.init dna.size (fun i -> dna.[i].value))

  /// Constructs the data from a specially encoded byte string in the format produced from DnaData.sourceCode.
  new(data : byte[]) =
    let decodedData = seq {
      let mutable i = 0
      while i < data.size do
        let x = decodeBase64 data.[i]
        if x < 60 then
          yield uint x
          i <- i + 1
        else
          let mutable x = x - 60
          for j = 1 to 5 do x <- (x <<< 6) + decodeBase64 data.[i + j]
          yield uint x
          i <- i + 6
    }
    DnaData(Seq.toArray decodedData)

  /// Constructs the data from a specially encoded string in the format produced from DnaData.sourceCode.
  new(data : string) = DnaData(System.Text.Encoding.ASCII.GetBytes(data))

  override this.choose(dna, i, choices) =
    enforce (i < data.size && data.[i] <= dna.parameter(i).maxValue && choices.weight(data.[i]) > 0.0) "DnaData.choose: Data mismatch."
    data.[i]

  override this.choose(dna, i) =
    enforce (i < data.size && data.[i] <= dna.parameter(i).maxValue) "DnaData.choose: Data mismatch."
    data.[i]

  /// Returns an F# constructor for this source. Employs a special variable length byte string encoding.
  member this.sourceCode =
    tome {
      yield "DnaData(\""
      for i = 0 to data.last do
        let x = data.[i]
        if x < 60u then yield int x |> encodeBase64 |> char else
          yield int (x >>> 30) + 60 |> encodeBase64 |> char
          for j = 1 to 5 do yield int (x >>> 30 - 6 * j) &&& 63 |> encodeBase64 |> char
      yield "\"B)"
    } |> buildTome

  /// Description length of the (uncompressed) genotype in bytes.
  member this.descriptionLength = data.size * sizeof<uint>



/// A pseudo-random source of Dna data.
and RandomSource(sourceRnd : Rnd) =
  inherit DnaSource()
  let rnd0 = Rnd()
  let rnd = Rnd()
  do rnd0.mix(sourceRnd)
  new(seed : int) = RandomSource(Rnd(seed))

  member this.mix(rnd : Rnd) = rnd0.mix(rnd)

  override this.start() =
    rnd.copyFrom(rnd0)
  override this.choose(_, _, choices) =
    choices.pick(rnd.float32() |> float)
  override this.choose(dna, i) =
    rnd.uint(0u, dna.[i].maxValue)
  override this.chooseInt(dna, i, valueTransform, priorTransform) =
    rnd.uint(0u, dna.[i].maxValue) |> priorTransform |> Fun.binarySearchClosest 0u dna.[i].maxValue valueTransform
  override this.chooseFloat(dna, i, valueTransform, priorTransform) =
    rnd.uint(0u, dna.[i].maxValue) |> priorTransform |> Fun.binarySearchClosest 0u dna.[i].maxValue valueTransform
