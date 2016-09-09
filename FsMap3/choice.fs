namespace Fuse

open Common


/// This generic interface is used to record Choices in Dna parameters.
type IChoices =
  /// Number of choices.
  abstract choiceCount : int
  /// Weight of choice (weight >= 0).
  abstract choiceWeight : int -> float
  /// Name of choice.
  abstract choiceName : int -> string
  /// Boxed value of choice.
  abstract choiceValue : int -> obj



/// Choice is a typed value, weighted and named, for a parameter.
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



/// A type for specifying a set of choices in Dna parameter value selection.
/// The choices are numbered starting from zero.
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

  /// Returns a default choice, which is the first choice with the greatest weight.
  member this.pickDefault() = Fun.argMax 0 this.last this.weight |> uint

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


  interface IChoices with
    member this.choiceCount = this.size
    member this.choiceWeight(i) = this.weight(i)
    member this.choiceName(i) = this.name(i)
    member this.choiceValue(i) = this.value(i) |> box

