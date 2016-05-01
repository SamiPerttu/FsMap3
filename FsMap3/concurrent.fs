/// Some concurrent and thread-safe data structures.
module FsMap3.Concurrent

open Common

// Atomic variables and immutable data structures make many lock-free data structures simple to implement,
// although the resulting implementations may not be as efficient as optimized versions based on
// fine grained mutability.


/// A lock-free stack.
[<NoEquality; NoComparison>]
type Stack<'a>() =
  let a = Atom.Shared<'a list>([])

  /// Returns whether the stack is empty.
  member this.isEmpty = a.value.IsEmpty

  /// Resets the stack to an empty state.
  member this.reset() = a.set([])

  /// Pushes an item on the stack.
  member this.push(v) = a.modify(fun l -> v :: l)

  /// Attempts to remove an item from the stack.
  member this.pull() =
    let result : 'a option ref = ref None
    // Note that in the modify function we need to set the result each time, as the function
    // may be called multiple times until it succeeds.
    a.modify(function head :: tail -> result := Some(head); tail | empty -> result := None; empty)
    !result



/// A lock-free, first-in-first-out queue.
[<NoEquality; NoComparison>]
type Queue<'a>() =
  // To achieve constant time operations, we store the queue as a pair of lists,
  // one for enqueueing and the other for dequeueing. The dequeue list is in reverse order.
  let a = Atom.Shared<Pair<'a list, 'a list> >(Pair([], []))

  /// Returns whether there are items in the queue.
  member this.isEmpty = let q = !a in q.a.IsEmpty && q.b.IsEmpty   

  /// Resets the queue to an empty state.
  member this.reset() = a.set(Pair([], []))

  /// Adds an item to the queue.
  member this.enqueue(v) = a.modify(fun f -> Pair(v :: f.a, f.b))

  /// Removes all items from the queue, calling the supplied function with each item as argument.
  /// Returns the number of items removed.
  member this.dequeueAll(f : 'a -> unit) =
    let mutable n = 0
    while not this.isEmpty do
      match this.dequeue() with
      | Some(v) -> f v
                   n <- n + 1
      | _ -> ()
    n

  /// Attempts to remove an item from the queue. Returns None if the queue is empty.
  member this.dequeue() =
    let result : 'a option ref = ref None
    a.modify(
      fun f ->
        if f.b.IsEmpty then
          // Dequeue list is empty. We reverse the enqueue list or return if it is empty, too.
          // Reversing a list is O(n), but as it contains n elements, it amortizes out to constant time.
          if f.a.IsEmpty then
            result := None
            f
          else
            let fb' = List.rev f.a
            result := Some(fb'.Head)
            Pair([], fb'.Tail)
        else
          result := Some(f.b.Head)
          Pair(f.a, f.b.Tail) )
    !result



/// A synchronized object that can be shared between threads. Ensures that only a single thread can
/// read or write the object concurrently.
[<NoEquality; NoComparison>]
type Synced<'a> =
  private { mutable currentValue : 'a }

  /// The current value.
  member this.value : 'a = lock this (fun _ -> this.currentValue)

  /// The current value.
  member inline this.Value = this.value

  /// Sets the value.
  member this.set(value') = lock this (fun _ -> this.currentValue <- value')

  /// Modifies the value.
  member this.modify(f : 'a -> 'a) = lock this (fun _ -> this.currentValue <- f this.currentValue)

  /// Creates a synchronized object.
  static member create(initialValue : 'a) = { Synced.currentValue = initialValue }



/// A synchronized object that any number of threads can modify and wait on.
/// Modifications are always broadcast to all waiting threads.
[<NoEquality; NoComparison>]
type Whiteboard<'a>(initialValue) =
  let mutable currentValue = initialValue

  /// The current value.
  member this.value =
    try
      System.Threading.Monitor.Enter(this)
      currentValue
    finally
      System.Threading.Monitor.Exit(this)

  /// The current value.
  member this.Value = this.value

  /// Modifies the item and signals all waiting threads that it has been modified.
  member this.modify(f : 'a -> 'a) =
    try
      System.Threading.Monitor.Enter(this)
      currentValue <- f currentValue
      System.Threading.Monitor.PulseAll(this)
    finally
      System.Threading.Monitor.Exit(this)

  /// Sets the item and signals all waiting threads that it has been modified.
  member this.set(value' : 'a) =
    try
      System.Threading.Monitor.Enter(this)
      currentValue <- value'
      System.Threading.Monitor.PulseAll(this)
    finally
      System.Threading.Monitor.Exit(this)

  /// Waits until the item fulfills the predicate. Returns the resulting item.
  member this.wait(f : 'a -> bool) =
    try
      System.Threading.Monitor.Enter(this)
      let rec loop() =
        if f currentValue then
          currentValue
        else
          System.Threading.Monitor.Wait(this) |> ignore
          loop()
      loop()
    finally
      System.Threading.Monitor.Exit(this)
    


/// A synchronized, first-in-first-out message board. If waiting is not necessary, there is
/// also a lock-free alternative: Concurrent.Queue.
[<NoEquality; NoComparison>]
type MessageBoard<'a>() =
  let queue = System.Collections.Generic.Queue<'a>()

  /// Maximum number of messages, if any. If this size limit is exceeded, oldest messages are
  /// silently discarded.
  member val maximumSize = none<int> with get, set

  /// Posts the message and signals a waiting thread that the board has been refreshed.
  member this.post(message : 'a) =
    try
      System.Threading.Monitor.Enter(this)
      this.maximumSize.apply(fun n -> while queue.Count >= n do ignore <| queue.Dequeue())
      queue.Enqueue(message)
      System.Threading.Monitor.Pulse(this)
    finally
      System.Threading.Monitor.Exit(this)

  /// Waits for a message to arrive on the board.
  member this.wait() =
    try
      System.Threading.Monitor.Enter(this)
      let rec loop() =
        if queue.Count > 0 then
          queue.Dequeue()
        else
          System.Threading.Monitor.Wait(this) |> ignore
          loop()
      loop()
    finally
      System.Threading.Monitor.Exit(this)

  /// Checks if a message is available. If there is one, removes it from the board and returns it.
  /// Otherwise returns None.
  member this.check() =
    try
      System.Threading.Monitor.Enter(this)
      if queue.Count > 0 then Some(queue.Dequeue()) else None
    finally
      System.Threading.Monitor.Exit(this)



/// Lock-free, single consumer, single producer, fixed capacity circular queue.
[<NoEquality; NoComparison>]
type RingBuffer<'a> =
  {
    /// Index where next item is consumed.
    consumerShared : Atom.Int
    mutable consumer : int
    /// Power-of-two item buffer.
    buffer : 'a array
    mask : int
    mutable producer : int
    /// Index where next item is produced.
    producerShared : Atom.Int
  }

  member inline this.Item
    with get i = this.buffer.[i]
    and set i x = this.buffer.[i] <- x

  // PUBLIC INTERFACE

  /// The capacity is size minus one - the buffer can never be completely full because we cannot distinguish
  /// between empty and full with a pair of wrapped indices.
  member inline this.capacity = this.mask

  /// Creates a circular queue with the given power-of-two size.
  static member create(size) : RingBuffer<'a> =
    enforce (Bits.isPowerOf2(size)) "RingBuffer.create: Size must be a power of two."
    { consumerShared = Atom.Int(0); consumer = 0; buffer = Array.zeroCreate size; mask = size - 1; producer = 0; producerShared = Atom.Int(0) }

  // CONSUMER INTERFACE

  /// How many items are ready for consumption.
  member inline this.items = (!this.producerShared - !this.consumerShared) &&& this.mask

  /// Gets an item. This is valid only if there are ready items.
  /// This must be followed (eventually) by a call to RingBuffer.publishGet.
  member inline this.get =
    let i = this.consumer
    let x = this.[i]
    this.consumer <- (i + 1) &&& this.mask
    x

  /// Publishes current consumption status for the producer to see. This allows the producer to reuse consumed
  /// item slots.
  member inline this.publishGet() = this.consumerShared.set(this.consumer)

  /// Feeds all items to the supplied function and publishes consumption status.
  member this.getAll(f : 'a -> unit) =
    let mutable n = this.items
    if n > 0 then
      for i = 1 to n do f this.get
      this.publishGet()

  // PRODUCER INTERFACE

  /// How many items can be produced at the moment.
  member inline this.space = this.capacity - this.items

  /// Produces an item. This is valid only if there is available space.
  /// This must be followed (eventually) by a call to RingBuffer.publishPut.
  member inline this.put(x) =
    let i = this.producer
    this.[i] <- x
    this.producer <- (i + 1) &&& this.mask

  /// Publishes current production status for the consumer to see. This allows the consumer
  /// to retrieve recently produced items.
  member inline this.publishPut() = this.producerShared.set(this.producer)



/// Wraps a procedure to prevent it from being called more than once.
/// Any calls after the first one result in no action taken.
let once (f : _ -> unit) =
  // Alternatively, we could use the inbuilt F# lazy computation facilities
  // (the lazy keyword and the Lazy<'T> type), which are thread-safe,
  // but our solution is slightly better, as it is also lock-free.
  let flag = Atom.Int(0)
  fun _ -> if !flag = 0 && flag.update(0, 1) then f()


