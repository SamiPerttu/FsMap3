/// Shared, lock-free variables and atomic operations.
namespace FsMap3.Atom

open System.Threading

open FsMap3
open Common


/// Shared object that supports atomic operations.
type Shared<'a>(initialValue : 'a) =

  // In .NET, marking a field volatile ensures acquire semantics on reads and release semantics on writes.
  // It also prevents the compiler from keeping private, cached versions of the field in, e.g., CPU registers.
  // Acquire semantics means that subsequent operations cannot be reordered to occur before it.
  // Release ("publish") semantics means that preceding operations cannot be reordered to occur after it.
  // In general, reordering semantics are needed whenever data is to be shared across threads. For example,
  // both locks and operations in System.Threading.Interlocked impose full memory barriers
  // (both acquire and release semantics).

  [<VolatileField>]
  let mutable cell = box initialValue

  let rec modifyLoop (f : 'a -> 'a) =
    let currentCell = cell
    let currentValue = unbox<'a>(currentCell)
    let targetValue = f currentValue
    let sourceCell = Interlocked.CompareExchange(&cell, box targetValue, currentCell)
    if sourceCell === currentCell then
      Pair(currentValue, targetValue)
    else
      // Another thread modified the value. Try again.
      modifyLoop f

  /// Value accessor.
  member this.value
    with get() = unbox<'a>(cell)
    and set(v : 'a) = cell <- box v

  /// Dereferencing accessor.
  member this.Value = unbox<'a>(cell)

  /// Sets the object.
  member this.set(v) = cell <- box v

  /// Modifies the object atomically with a function. Retries in case of a collision with another thread.
  /// Does not return a value.
  member this.modify(f) = modifyLoop f |> ignore

  /// Modifies the object atomically with a function. Retries in case of a collision with another thread.
  /// Returns the original value ("pre-op").
  member this.pre(f) = modifyLoop(f).x

  /// Modifies the object atomically with a function. Retries in case of a collision with another thread.
  /// Returns the modified value ("post-op").
  member this.post(f) = modifyLoop(f).y

  override this.ToString() = sprintf "Shared(%A)" !this



/// Integer that supports atomic operations.
type Int(?initialValue) =

  [<VolatileField>]
  let mutable v = initialValue >? 0

  let rec modifyLoop (f : int -> int) =
    let currentValue = v
    let targetValue = f currentValue
    let sourceValue = Interlocked.CompareExchange(&v, targetValue, currentValue)
    if sourceValue = currentValue then
      Pair(currentValue, targetValue)
    else
      modifyLoop f

  /// Value accessor.    
  member this.value
    with get() = v
    and set(v') = v <- v'

  /// Dereferencing accessor.
  member this.Value = v

  /// Sets the value of the atom.
  member this.set(v') = v <- v'

  /// Modifies the value atomically with a function. Does not return a value.
  member this.modify(f) = modifyLoop f |> ignore

  /// Modifies the value atomically with a function. Returns the original value ("pre-op").
  member this.pre(f) = modifyLoop(f).x

  /// Modifies the value atomically with function. Returns the modified value ("post-op").
  member this.post(f) = modifyLoop(f).y;

  /// Attempts to update the value atomically from v0 to v1. Returns success or failure.
  member this.update(v0, v1) = Interlocked.CompareExchange(&v, v1, v0) = v0

  override this.ToString() = sprintf "Atom.Int(%A)" !this



/// Float that supports atomic operations.
type Float(?initialValue) =

  // Note 1. .NET does not guarantee that floats are accessed atomically. Therefore, we need to use
  // atomic operations even when accessing the value unconditionally. Marking the variable
  // volatile would be superfluous in this case.

  // Note 2. In ECMA-335 12.6.2 it is stated that "It is strongly recommended that float64 be aligned
  // on an 8-byte boundary, even when the size of native int is 32 bits." In other words, in practice
  // we should not have to worry about these objects straddling a cache line boundary (which would
  // prevent atomic operations).

  let mutable v = initialValue >? 0.0

  let rec modifyLoop (f : float -> float) =
    let currentValue = Interlocked.CompareExchange(&v, 0.0, 0.0)
    let targetValue = f currentValue
    let sourceValue = Interlocked.CompareExchange(&v, targetValue, currentValue)
    if sourceValue = currentValue then
      Pair(currentValue, targetValue)
    else
      modifyLoop f

  /// Modifies the value with a function. Does not return a value.
  member this.modify(f) = modifyLoop f |> ignore

  /// Value accessor.    
  member this.value
    with get() = Interlocked.CompareExchange(&v, 0.0, 0.0)
    and set(v') = ignore <| Interlocked.Exchange(&v, v')

  /// Dereferencing accessor.
  member this.Value = Interlocked.CompareExchange(&v, 0.0, 0.0)

  /// Sets the value of the atom.
  member this.set(v') = ignore <| Interlocked.Exchange(&v, v')

  /// Modifies the value atomically with a function. Returns the original value ("pre-op").
  member this.pre(f) = modifyLoop(f).x

  /// Modifies the value atomically with function. Returns the modified value ("post-op").
  member this.post(f) = modifyLoop(f).y;

  /// Attempts to update the value atomically from v0 to v1. Returns success or failure.
  member this.update(v0, v1) = Interlocked.CompareExchange(&v, v1, v0) = v0

  override this.ToString() = sprintf "Atom.Float(%A)" !this



/// This lock-free counter iterates on a stored value atomically.
[<ReferenceEquality>]
type SharedCounter<'a> =
  {
    value : Shared<'a>
    f : 'a -> 'a
  }
  // Enable the ! operator, for which we mirror the semantics of Common.Counter.
  member inline this.Value = this.value.value

  /// Returns the current value while iterating to the next value.
  member inline this.tick = this.value.pre(this.f)

  /// Creates a lock-free incrementing counter.
  static member createCounter(value0) = { Counter.value = value0; f = (+) 1G }

  /// Creates a lock-free counter with an arbitrary (reentrant) function.
  static member createIterate(value0, f) = { Counter.value = value0; f = f }

  /// Creates a lock-free MINSTD linear congruential generator (seed > 0).
  static member createMinstd(seed) =
    enforce (seed > 0) "Atom.createSharedLcg: Seed must be greater than zero."
    // Do a little "burn-in" for the initial value.
    { Counter.value = minstd (minstd seed); f = minstd }



/// Lock-free lazy value. The price we pay for locklessness is that the function computing the value may be called
/// more than once. The value established and used comes from the first thread that manages to post it.
[<NoEquality; NoComparison>]
type LazyAtom<'a> =
  {
    f : unit -> 'a
    v : Shared<'a option>
  }

  member this.value =
    match this.v.value with
    | Some(v) -> v
    | None    -> !this.v.post(function | None -> Some(this.f()) | x -> x)

  member inline this.Value = this.value

  static member create(f) : LazyAtom<'a> =
    {
      LazyAtom.f = f;
      v = Shared(None)
    }


