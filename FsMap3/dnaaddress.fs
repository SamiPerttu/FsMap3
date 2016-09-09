namespace Fuse

open Common
open Tome
open Convert
open Mangle


/// Tree address is encoded in a 64-bit integer. It contains a 1 "guard" bit followed by 4 bits per level of sibling number
/// information. Levels 0 to 15 are supported. If the level is too large (this should be rare in practice), then the address
/// is set to the special null address.
[<NoComparison>]
type DnaAddress = struct
  val x : uint64

  new(x) = { x = x }

  /// Root address contains only the guard bit. There is exactly one root node -
  /// we do not have to encode its sibling number.
  static member Root = DnaAddress(1UL)

  /// The null address represents an address that does not exist or cannot be represented.
  static member Null = DnaAddress(0UL)

  /// Address guard bit for the given level (level in [0, 15]).
  static member guard(level) = 1UL <<< (level <<< 2)

  member this.value = this.x
  member this.isNull = this.x = 0UL
  member this.isRoot = this.x = 1UL

  /// Returns whether this address is at the maximum representable level. Any children below this level are set to the null address.
  member this.isMaxLevel = this.x &&& DnaAddress.guard 15 <> 0UL

  /// The address of a child node. Returns the null address if the child cannot be represented.
  member this.child(childNumber) = if this.isMaxLevel then DnaAddress.Null else DnaAddress((this.x <<< 4) ||| uint64 (childNumber &&& 0xf))

  /// The sibling number of this address at the given relative level where 0 is the current level,
  /// 1 is the parent level, and so on.
  member this.siblingNumber(relativeLevel) = (this.x >>> (relativeLevel <<< 2)) &&& 0xfUL

  /// Calculates a local address identifier that includes at most the specified number of levels of context.
  /// Note that the return value is not an address!
  member this.localId(maximumLevels) = if this.isNull then this.x elif maximumLevels >= 16 then this.x else this.x &&& ((1UL <<< maximumLevels) - 1UL)

  /// The address of the given ancestor (0 = this node, 1 = parent, and so on)
  /// or the null address if it does not exist or cannot be obtained.
  member this.ancestor(relativeLevel) = DnaAddress(this.x >>> (relativeLevel <<< 2))

  /// The address of the parent or the null address if it does not exist or cannot be obtained.
  member this.parent = this.ancestor(1)

  /// The level of this (non-null) address.
  member this.level = Bits.highestBit64 this.x >>> 2

  /// Returns whether we are an ancestor of the given address.
  member this.isAncestorOf(address : DnaAddress) = this.x < address.x && address.ancestor(address.level - this.level).x = this.x

end

