namespace FsMap3

open Common
open Mangle


/// Parameter format describes how different parameter values are related.
type DnaParameterFormat = Categorical | Ordered



/// Parameter of a procedural generator.
[<NoEquality; NoComparison>]
type DnaParameter =
  {
    /// Format of the parameter.
    format : DnaParameterFormat
    /// Name of the parameter.
    name : string
    /// The range of raw values is [0, maxValue].
    maxValue : uint

    /// Parameter level - zero is root.
    level : int
    /// Local tree address.
    address : DnaAddress
    /// Local, intra-node parameter number.
    number : int
    /// Parent parameter index, if any. Every parameter not in the root node has a parent on the previous level.
    parent : int Optionval

    /// Combined hash of format, name and number of categories.
    semanticId : int64
    /// Combined hash of format, name, number of categories, address, level and number.
    /// This is enough to identify a parameter uniquely within the set of genotypes produced by a generator.
    structuralId : int64
    /// Combined hash of semantic IDs in this parameter and its descendants.
    mutable subtreeId : int64

    /// Raw value. This is transformed into the generated object.
    mutable value : uint

    /// Description of the value of the parameter (optional). For interactive display; not used by the Dna system itself.
    mutable valueString : string
    /// Set of choices, if applicable (optional).
    mutable choices : IChoices option
    /// Generated object (optional).
    mutable generated : obj option
  }

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

  member this.updateSubtreeId(child : DnaParameter) =
    this.subtreeId <- mangle64 (this.subtreeId + child.subtreeId)

  static member getSemanticId(format : DnaParameterFormat, name, maxValue : uint) =
    mangle64 (int64 (mangleString name) + mangle64 (match format with | Categorical -> int64 maxValue | _ -> -1L))

  static member getStructuralId(semanticId, level, address : DnaAddress, number) =
    mangle64 (semanticId + mangle64 (int64 address.x + mangle64 ((int64 number <<< 32) + int64 level)))

