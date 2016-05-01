// A generic parameter space for procedural generators.
namespace FsMap3

(*
Dna System Overview

Procedural generators draw trees of parameters to generate objects. Parameter values are converted
from raw 32-bit data values. Given parameter data, the output of a generator is deterministic.

Raw data is provided by DnaSource objects. These can implement pseudo-random data, replicated data,
or sophisticated sampling and optimization methods. DnaSource has a feedback mechanism for guiding evolution.

As parameters are drawn, their details are recorded in the genotype, including tree structure
and parameter grouping. The details can be printed or visualized easily (e.g., see the DnaView type).

One of the advantages of the approach is that implementation of generators is easy: only the generator function
needs to be written (and a visualizer, if GUI interaction is desired). Mutation, optimization, serialization
and so on operate on parameter data, for which generic facilities are available. (In genetic programming,
this approach is called "Grammar Evolution"). 

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


/// Tree address is encoded in a 64-bit integer. It contains a 1 "guard" bit followed by 4 bits per level of sibling number
/// information. Levels 0 to 15 are supported. If the level is too large (this should be rare in practice), then the address
/// is set to the special null address.
type ParameterAddress = struct
  val x : uint64

  new(x) = { x = x }

  /// Root address contains only the guard bit. There can only be one root node, so we do not have to
  /// encode its sibling number.
  static member Root = ParameterAddress(1UL)
  /// The null address represents an address that does not exist or cannot be represented.
  static member Null = ParameterAddress(0UL)
  /// Address guard bit for the given level (level in [0, 15]).
  static member guard(level) = 1UL <<< (level <<< 2)

  member this.value = this.x
  member this.isNull = this.x = 0UL
  member this.isRoot = this.x = 1UL

  /// Returns whether this address is at the maximum representable level. Any children below this level are set to the null address.
  member this.isMaxLevel = this.x &&& ParameterAddress.guard 15 <> 0UL
  /// Returns the address of a child node. Returns the null address if the child cannot be represented.
  member this.child(childNumber) = if this.isMaxLevel then ParameterAddress.Null else ParameterAddress((this.x <<< 4) ||| uint64 (childNumber &&& 0xf))
  /// Returns the sibling number of this address at the given relative level, where 0 is the current level, 1 is the parent level, and so on.
  member this.siblingNumber(relativeLevel) = (this.x >>> (relativeLevel <<< 2)) &&& 0xfUL
  /// Returns a local address identifier that includes at most the specified number of levels of context. Note that the return value is not an address!
  member this.localId(maximumLevels) = if this.isNull then this.x elif maximumLevels >= 16 then this.x else this.x &&& ((1UL <<< maximumLevels) - 1UL)
  /// Returns the address of the given ancestor (0 = this node, 1 = parent, and so on) or the null address if it does not exist or cannot be obtained.
  member this.ancestor(relativeLevels) = if this.isNull then this else ParameterAddress(this.x >>> (relativeLevels <<< 2))
  /// Returns the address of the parent or the null address if it does not exist or cannot be obtained.
  member this.parent = this.ancestor(1)
  /// Returns the level of this (non-null) address.
  member this.level = Bits.highestBit64 this.x >>> 2
  /// Returns whether we are an ancestor of address.
  member this.isAncestorOf(address : ParameterAddress) = this.x < address.x && address.ancestor(address.level - this.level).x = this.x
end



/// Parameter format describes how different parameter values are related.
type ParameterFormat = Categorical | Ordered



/// Parameter of a procedural generator.
[<NoEquality; NoComparison>]
type Parameter =
  {
    /// Format of the parameter.
    format : ParameterFormat
    /// Name of the parameter.
    name : string
    /// The range of raw values is [0, maxValue].
    maxValue : uint

    /// Parameter level - zero is root.
    level : int
    /// Local tree address.
    address : ParameterAddress
    /// Local, intra-node parameter number.
    number : int
    /// Parent parameter index, if any. Every parameter not in the root node has a parent on the previous level.
    parent : int Optionval

    /// Combined hash of format, name and number of categories.
    semanticId : int64
    /// Combined hash of format, name, number of categories, address, level and number.
    /// This is enough to identify a parameter uniquely within a genotype.
    structuralId : int64

    /// Raw value. This is typically transformed into another type and/or range. The transformed value
    /// itself is not stored here (but its description is - see below).
    mutable value : uint
    /// Description of the value of the parameter. For interactive display; not used by the Dna system itself.
    mutable valueString : string
    /// Value strings indexed by value. For interactive editing; not used by the Dna system itself.
    mutable valueChoices : string[]
  }

  static member getSemanticId(format : ParameterFormat, name, maxValue : uint) =
    mangle64 (int64 (mangleString name) + mangle64 (match format with | Categorical -> int64 maxValue | _ -> -1L))

  static member getStructuralId(semanticId, level, address : ParameterAddress, number) =
    mangle64 (semanticId + mangle64 (int64 address.x + mangle64 ((int64 number <<< 32) + int64 level)))

  /// Returns whether the parameter has a parent. Every parameter except the root is supposed to have a parent.
  member inline this.hasParent = this.parent.isSome

  /// Label parameters exist only to serve as subtree roots: every subtree in a Dna should have a root parameter.
  member inline this.isLabel = this.maxValue = 0u

  /// Returns whether the given parameter value is legal.
  member inline this.isLegal(x) = x <= this.maxValue

  /// Returns whether the parameter has the full integer range.
  member inline this.isFullRange = this.maxValue = maxValue uint

  /// The number of possible values of the parameter (as uint64 because it may not fit into an uint).
  member inline this.range = uint64 this.maxValue + 1UL

  /// The value as a float in [0, 1]. This is often used by Dna sources to manipulate ordered values.
  member inline this.value01 =
    if this.maxValue > 0u then (float this.value / float this.maxValue) else 0.5



/// A choice is a typed value, weighted and named, for a parameter.
type Choice<'a>(weight : float, name : string, value : 'a) =
  member this.weight = weight
  member this.name = name
  member this.value = value

  /// Constructs a choice with name retrieved via Object.GetString.
  new(weight : float, value : 'a) = Choice<'a>(weight, string (box value), value)

  /// Constructs a unit weight choice.
  new(name : string, value : 'a) = Choice<'a>(1.0, name, value)

  /// Constructs a unit weight choice with name retrieved via Object.GetString.
  new(value : 'a) = Choice<'a>(1.0, string (box value), value)



/// A shorthand for the Choice type.
type C<'a> = Choice<'a>



/// A type for specifying a set of choices. The raw choice values are numbered starting from zero.
type Choices<'a>([<System.ParamArray>] v : C<'a> array) =

  let wset = Wset<Pair<string, 'a>>()

  do
    for i = 0 to v.last do wset.add(v.[i].weight, Pair(v.[i].name, v.[i].value))

  /// The number of choices.
  member this.size = wset.size

  /// Number of the last choice.
  member this.last = wset.last

  /// Weight of choice number i.
  member this.weight(i) = wset.weight(i)

  /// Weight of choice number u.
  member this.weight(u : uint) = wset.weight(int u)

  /// Sets weight of choice number i.
  member this.setWeight(i, w) = wset.weight(i) <- w

  /// Sets weight of choice number u.
  member this.setWeight(u : uint, w) = wset.weight(int u) <- w

  /// Name of choice number i.
  member this.name(i) = wset.at(i).fst

  /// Name of choice number u.
  member this.name(u : uint) = wset.at(int u).fst

  /// Returns choice number i.
  member this.value(i) = wset.at(i).snd

  /// Returns choice number u.
  member this.value(u : uint) = wset.at(int u).snd

  /// Picks a choice proportionately using x in [0, 1].
  member this.pick(x) = uint (wset.pickIndex(x))

  /// Picks a choice proportionately using x in [0, 1]. The single choice excluded must not be the only valid choice.
  member this.pickExcluding(exclude, x) = uint (wset.pickIndexExcluding(exclude, x))

  /// Picks a choice proportionately using x in [0, 1]. The single choice excluded must not be the only valid choice.
  member this.pickExcluding(exclude : uint, x) = uint (wset.pickIndexExcluding(int exclude, x))

  /// Maximum choice number (raw value).
  member this.maximum = uint wset.last

  /// Total weight of choices.
  member this.total = wset.total

  /// Adds a choice.
  member this.add(weight, name, value) = wset.add(weight, Pair(name, value))

  /// If there is only one possible choice, returns its number, otherwise returns Noneval.
  member this.singular =
    let i = wset.pickIndex(0.0)
    if wset.weight(i) = wset.total then Someval(uint i) else Noneval

  /// Returns whether choice number u exists and has a non-zero weight.
  member this.isLegal(u : uint) = u < uint wset.size && wset.weight(int u) > 0.0

  /// Returns whether choice number i exists and has a non-zero weight.
  member this.isLegal(i : int) = this.isLegal(uint i)

  /// Returns the number of the (single) choice that fulfills the predicate. Throws an exception if no choices
  /// or more than one choice match.
  member this.numberOf(predicate : 'a -> bool) =
    let mutable number = Noneval
    for i = 0 to this.last do
      if predicate (this.value(i)) then
        enforce (number.isNone) "Choices.numberOf: More than one matching choice."
        number <- Someval(uint i)
    enforce (number.isSome) "Choices.numberOf: No matching choices."
    !number



/// Level state for a Dna. Internal type.
type LevelState = struct
  /// Current node address on this level.
  val address : ParameterAddress
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
    parameterArray : Parameter Darray

    /// Fingerprint of the genotype.
    mutable fingerprint : int64

    /// The data source for this generation. Used during generation only.
    mutable source : DnaSource Optionval

    /// Current set of injectors for this generation. Used during generation only.
    injectorArray : DnaInjector Darray

    /// Level states. The array always contains at least the root level during generation.
    /// Used during generation only.
    state : LevelState Darray
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

  /// Computes a local fingerprint for the subtree rooted at the node containing parameter i.
  member this.localFingerprint(i) =
    let address = this.[i].address
    let mutable localprint = 0L
    for j = 0 to this.last do
      if this.isInSubtree(j, i) then
        localprint <- mangle64 (localprint + this.[j].structuralId)
        localprint <- mangle64 (localprint + int64 this.[j].value)
    localprint
 

  // INTERNAL METHODS


  /// Chooses a value for a new parameter.
  member private this.choose(transform : uint -> 'a) =
    let rec tryFilter i =
      if i < 0 then Noneval else
        match this.injectorArray.[i].choose(this, this.last) with
        | Noneval -> tryFilter (i - 1)
        | x -> x
    let value =
      match tryFilter this.injectorArray.last with
      | Noneval ->
        match box transform with
        | :? (uint -> int) as f -> (!this.source).chooseInt(this, this.last, f)
        | :? (uint -> float) as f -> (!this.source).chooseFloat(this, this.last, f)
        | _ -> (!this.source).choose(this, this.last)
      | Someval(x) ->
        x
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


  /// Creates a new parameter.
  member private this.newParameter(format, name, maximumValue, valueTransform : uint -> 'a, stringConverter : 'a -> string) =
    let i = this.size
    let a = this.state.[this.level]
    let semanticId = Parameter.getSemanticId(format, name, maximumValue)
    let parameter = {
      Parameter.format = format
      name = name
      maxValue = maximumValue
      level = this.level
      address = a.address
      number = a.number
      parent = if this.level > 0 then this.state.[this.level - 1].parameterIndex else Noneval
      semanticId = semanticId
      structuralId = Parameter.getStructuralId(semanticId, this.level, a.address, a.number)
      value = 0u
      valueString = ""
      valueChoices = Array.createEmpty
      }
    this.parameterArray.add(parameter)
    parameter.value <- this.choose(valueTransform)
    let transformed = valueTransform parameter.value
    parameter.valueString <- stringConverter transformed
    parameter.valueChoices <- Array.createEmpty
    this.updateFingerprint(parameter)
    this.state.[this.level] <- LevelState(a.address, Someval(i), a.children, a.number + 1)
    transformed


  /// Creates a new parameter with a set of choices.
  member private this.newParameter(format, name, choices : Choices<'a>) =
    let i = this.size
    let a = this.state.[this.level]
    let semanticId = Parameter.getSemanticId(format, name, choices.maximum)
    let parameter = {
      Parameter.format = format
      name = name
      maxValue = choices.maximum
      level = this.level
      address = a.address
      number = a.number
      parent = if this.level > 0 then this.state.[this.level - 1].parameterIndex else Noneval
      semanticId = semanticId
      structuralId = Parameter.getStructuralId(semanticId, this.level, a.address, a.number)
      value = 0u
      valueString = ""
      valueChoices = Array.createEmpty
      }
    this.parameterArray.add(parameter)
    parameter.value <- this.choose(choices)
    parameter.valueString <- choices.name(parameter.value)
    // Copy values to the choice vector in the parameter.
    parameter.valueChoices <- Array.init choices.size (fun i -> if choices.weight(i) > 0.0 then choices.name(i) else "")
    this.updateFingerprint(parameter)
    this.state.[this.level] <- LevelState(a.address, Someval(i), a.children, a.number + 1)
    choices.value(parameter.value)


  /// Creates a label parameter. Labels are dummy parameters whose purpose is to make sure
  /// every branch of the Dna tree has a root node.
  member private this.createLabel(name) =
    let i = this.size
    let a = this.state.[this.level]
    let semanticId = Parameter.getSemanticId(Categorical, name, 0u)
    let parameter = {
      Parameter.format = Categorical
      name = name
      maxValue = 0u
      level = this.level
      address = a.address
      number = a.number
      parent = if this.level > 0 then this.state.[this.level - 1].parameterIndex else Noneval
      semanticId = semanticId
      structuralId = Parameter.getStructuralId(semanticId, this.level, a.address, a.number)
      value = 0u
      valueString = ""
      valueChoices = Array.createEmpty
      }
    this.parameterArray.add(parameter)
    this.choose(ignore) |> ignore
    this.updateFingerprint(parameter)
    this.state.[this.level] <- LevelState(a.address, Someval(i), a.children, a.number + 1)


  /// Creates a new full range parameter.
  member private this.newParameter(format, name, valueTransform, stringConverter) =
    this.newParameter(format, name, ~~~0u, valueTransform, stringConverter)


  /// Does bookkeeping after all parameter attributes have been set. Updates the Dna fingerprint.
  /// This is called exactly once per created parameter.
  member private this.updateFingerprint(parameter : Parameter) =
    this.fingerprint <- mangle64 (this.fingerprint + parameter.structuralId)
    this.fingerprint <- mangle64 (this.fingerprint + int64 parameter.value)
      

  /// Calls a subgenerator in a child node in the parameter tree.
  member private this.descend(generator : Dna -> _) =
    let a = this.state.[this.level]
    this.state.push(LevelState(a.address.child(a.children), Noneval, 0, 0))
    let result = generator this
    this.state.pop()
    this.state.[this.level] <- LevelState(a.address, a.parameterIndex, a.children + 1, a.number)
    result


  // PUBLIC INTERFACE


  /// Generates an object using the source.
  member this.generate(source, generator : Dna -> _) =
    this.parameterArray.reset()
    this.source <- Someval(source)
    this.injectorArray.reset()
    source.start()
    this.state.reset()
    this.state.push(LevelState(ParameterAddress.Root, Noneval, 0, 0))
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


  /// Adds an injector. More recently added injectors have higher precedence.
  member this.addInjector(injector : DnaInjector) =
    this.injectorArray.add(injector)


  /// Calls a subgenerator in a child node in the parameter tree. Adds a dummy parameter
  /// as a label for the subtree.
  member this.descend(label : string, generator : Dna -> _) =
    this.createLabel(label)
    this.descend(generator)


  /// Queries a float parameter. Transformation from the unit interval
  /// as well as unit interval type (closed by default) can be specified.
  member this.float(name, ?transformation : float -> float, ?interval, ?unit) =
    let interval = interval >? Closed
    let transformation = transformation >? id
    let unit = unit >? ""
    this.newParameter(
      Ordered,
      name,
      float01u interval >> transformation,
      fun x -> match unit with | "" -> Pretty.string x | unit -> Pretty.string x + " " + unit
      )


  /// Queries a float32 parameter. Transformation from the unit interval
  /// as well as unit interval type (closed by default) can be specified.
  member this.float32(name, ?transformation, ?interval, ?unit) =
    this.float(name, float32 >> (transformation >? id) >> float, interval >? Closed, unit >? "") |> float32


  /// Queries a categorical parameter.
  member this.category(name, choices : Choices<_>) =
    this.newParameter(Categorical, name, choices)


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
    this.newParameter(Ordered, name, choices)


  /// Queries an ordered parameter from a set of choices.
  member this.ordered(name, [<System.ParamArray>] choices : C<_> array) =
    this.ordered(name, Choices(choices))


  /// Queries an ordered full range parameter.
  member this.ordered(name, transformation : int -> _) =
    this.newParameter(Ordered, name, (+) 0x80000000u >> int >> transformation, box >> string)


  /// Queries an int parameter in [minimum, maximum], with an optional transformation.
  member this.int(name, minimum : int, maximum : int, ?transformation : int -> int, ?general) =
    enforce (minimum <= maximum) "Dna.int: Empty range."
    let maximumValue = uint maximum - uint minimum
    this.newParameter(Ordered, name, maximumValue, (+) (uint minimum) >> int >> (transformation >? id), string)


  /// Queries an int parameter in [0, range[.
  member this.int(name, range) =
    this.int(name, 0, range - 1)


  /// Queries an int parameter in [0, range[, with a transformation.
  member this.int(name, range, transformation : int -> int) =
    this.int(name, 0, range - 1, transformation)


  /// Queries a full range int parameter.
  member this.int(name) =
    this.int(name, minValue int, maxValue int)


  /// Queries "raw" categorical data in [0, range[ (or, by default, the full 32-bit range).
  /// The result is returned as an int (due to popular demand).
  member this.data(name, ?range : int) =
    enforce (range.isNone || range.value > 0) "Dna.data: Empty range."
    let maximumValue = match range with | Some(r) -> uint r - 1u | None -> maxValue uint
    this.newParameter(
      Categorical,
      name,
      maximumValue,
      id,
      fun x ->
        let nybbles = match range with | Some(r) -> max 1 ((Bits.highestBit32 (uint r - 1u) + 3) / 4) | None -> 8
        sprintf "%0*x" nybbles x
      ) |> int


  /// Queries a 32-bit word, which is converted into an object.
  member this.data(name, transformation : int -> _) =
    this.newParameter(Categorical, name, int >> transformation, box >> string)


  /// Returns true or false.
  member this.boolean(name, ?trueProbability) =
    let p = trueProbability >? 0.5
    this.newParameter(Categorical, name, Choices(C(1.0 - p, "no", false), C(p, "yes", true)))


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

  /// Chooses an ordered integer value for a parameter. The transformation is monotonic.
  /// This is routed to DnaSource.choose by default.
  abstract chooseInt : Dna * int * (uint -> int) -> uint

  /// Chooses an ordered float value for a parameter. The transformation is monotonic.
  /// This is routed to DnaSource.choose by default.
  abstract chooseFloat : Dna * int * (uint -> float) -> uint

  default __.start() = ()
  default __.ready(_) = ()
  default __.feedback(_, _) = ()
  default __.observe(_, _) = ()

  default this.chooseInt(dna, i, transformation : uint -> int) = this.choose(dna, i)
  default this.chooseFloat(dna, i, transformation : uint -> float) = this.choose(dna, i)

  /// Generates an object using this source.
  member this.generate(generator : Dna -> _) =
    let dna = Dna.create()
    dna.generate(this, generator)



/// A Dna source that can replicate an existing genotype.
and DnaData(data : uint array) =
  inherit DnaSource()

  /// Constructs the source to replicate the given Dna.
  new(dna : Dna) = DnaData(Array.init dna.size (fun i -> dna.parameter(i).value))

  override this.choose(dna, i, choices) =
    enforce (i < data.size && data.[i] <= dna.parameter(i).maxValue && choices.weight(data.[i]) > 0.0) "DnaData.choose: Data mismatch."
    data.[i]

  override this.choose(dna, i) =
    enforce (i < data.size && data.[i] <= dna.parameter(i).maxValue) "DnaData.choose: Data mismatch."
    data.[i]

  /// Returns an F# constructor for this source.
  member this.sourceCode =
    tome {
      yield "DnaData([|"
      for i = 0 to data.Length - 1 do
        if i % 8 = 0 then yield "\n  " else yield "; "
        if data.[i] < 100u then yield sprintf "%du" data.[i] else yield sprintf "0x%xu" data.[i]
      yield " |])"
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

  override this.start() = rnd.copyFrom(rnd0)
  override this.choose(_, _, choices) = choices.pick(rnd.float32() |> float)
  override this.choose(dna, i) = rnd.uint(0u, dna.parameter(i).maxValue)



/// Specimen is an individual generated from Dna, with an attached fitness value.
[<NoEquality; NoComparison>]
type Specimen<'a> =
  {
    dna : Dna
    phenotype : 'a
    fitness : float
  }

  /// Creates a specimen.
  static member create(dna, phenotype : 'a) = { Specimen.dna = dna; phenotype = phenotype; fitness = 0.0 }

  /// Creates a specimen with fitness.
  static member create(dna, phenotype : 'a, fitness) =
    enforce (isNaN fitness = false) "Specimen.create: NaN fitness values are prohibited."
    { Specimen.dna = dna; phenotype = phenotype; fitness = fitness }

  /// Generates a specimen from the source.
  static member generate(source : DnaSource, generatef : Dna -> 'a, ?fitnessf : 'a -> float) =
    let dna = Dna.create()
    let pheno = dna.generate(source, generatef)
    Specimen.create(dna, pheno, fitnessf.map((|>) pheno) >? 0.0)

  /// Generates a random specimen using the given Rnd.
  static member generate(rnd : Rnd, generatef : Dna -> 'a, ?fitnessf : 'a -> float) =
    let dna = Dna.create()
    let pheno = dna.generate(RandomSource(rnd), generatef)
    Specimen.create(dna, pheno, fitnessf.map((|>) pheno) >? 0.0)

