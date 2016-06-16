// Option as a value type.
[<AutoOpen>]
module FsMap3.Optionval

open Common


/// Option as a value type. Interface here should be kept as compatible as possible with the standard option type.
/// This type is useful in avoiding allocations when the parameter type is a value type as well.
type Optionval<'T> = struct
  val isSome : bool
  val value : 'T

  new(isSome, value) = { isSome = isSome; value = value }

  member inline this.Value = this.value
  member inline this.isNone = not this.isSome
  member inline this.IsSome = this.isSome

  /// Applies a function to the value of the option, if it exists.
  member inline this.apply(f : 'T -> unit) = if this.isSome then f this.value

  /// Transforms the option with the given value map.
  member inline this.map(f : 'T -> 'U) = if this.isSome then Optionval(true, f this.value) else Optionval(false, Unchecked.defaultof<'U>)

  /// Filters option values with a predicate, transforming to Noneval those that fail it.
  member inline this.filter(predicate : 'T -> bool) = if this.isSome && predicate !this then this else Optionval(false, Unchecked.defaultof<'T>)

  /// Returns true iff the option has a value that matches the predicate.
  member inline this.isSomeAnd(predicate : 'T -> bool) = this.isSome && predicate !this

  /// Returns true iff the option has no value or if the value matches the predicate.
  member inline this.isNoneOr(predicate : 'T -> bool) = this.isNone || predicate !this

  override this.ToString() = if this.isSome then sprintf "Someval(%A)" this.value else "Noneval"
end


/// Constructs an Optionval structure with no value.
let inline Noneval<'T> = Optionval(false, Unchecked.defaultof<'T>)


/// Construct an Optionval structure with a value.
let inline Someval(value) = Optionval(true, value)


/// Constructs an Optionval structure that is logically Noneval but has a fallback value that can be accessed.
/// For low level optimization.
let inline Fallbackval(value) = Optionval(false, value)


/// Active pattern for matching option structures.
let (|Someval|Noneval|) (x : _ Optionval) = if x.isSome then Someval(x.value) else Noneval

