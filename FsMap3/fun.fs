/// Generic algorithms parametrized with functions.
[<RequireQualifiedAccess>]
module FsMap3.Fun

open Common


/// Binary searches the argument range [i0, i1] for the value.
/// Returns the final argument from the range. If the exact value is found,
/// then the result will point to the first matching value.
let inline binarySearch i0 i1 f value =
  enforce (i0 <= i1) "Fun.binarySearch: Empty range."
  let mutable i0 = i0
  let mutable i1 = i1
  while i0 < i1 do
    // The following formulation of the midpoint allows negative indices, avoids overflows,
    // and works for all integral types.
    let i = (i0 >>> 1) + (i1 >>> 1)
    if f i < value then i0 <- i + 1G else i1 <- i
  i0


/// Binary searches the argument range [i0, i1] for an insertion slot for the value.
/// Returns the final index, which is the index to which the value can be inserted
/// to maintain correct ordering, once existing values starting from the index are
/// shifted out of the way. If there are existing values equal to the values,
/// the new value will be inserted before the others. The result is in [i0, i1 + 1].
let inline binarySearchSlot i0 i1 f value =
  let i = binarySearch i0 i1 f value
  if value > f i then i + 1G else i


/// Binary searches the argument range [i0, i1]. Returns the argument where the function
/// comes closest to the value in absolute difference.
let inline binarySearchClosest i0 i1 f value =
  let i = binarySearch i0 i1 f value
  let mutable closestI = i
  let mutable closestD = abs(f i - value)
  if i > i0 then
    let d = abs(f (i - 1G) - value)
    if d < closestD then
      closestI <- i - 1G
      closestD <- d
  if i < i1 then
    let d = abs(f (i + 1G) - value)
    if d < closestD then
      closestI <- i + 1G
      closestD <- d
  closestI


/// Reduces a projected range with the binary operator.
let inline reduce i0 i1 (f : int -> 'a) (binop: 'a -> 'a -> 'a) =
  enforce (i0 <= i1) "Fun.reduce: Empty range."
  let mutable v = f i0
  for i = i0 + 1 to i1 do
    v <- binop v (f i)
  v


/// Calculates the sum of the function in [i0, i1].
let inline sum i0 i1 f = reduce i0 i1 f (+)


/// Calculates the product of the function in [i0, i1].
let inline product i0 i1 f = reduce i0 i1 f (*)


/// Returns the minimum of the function in [i0, i1].
let inline min i0 i1 f = reduce i0 i1 f min


/// Returns the maximum of the function in [i0, i1].
let inline max i0 i1 f = reduce i0 i1 f max


/// Returns the function value with the minimum projection in [i0, i1].
let inline minBy i0 i1 f projection =
  enforce (i0 <= i1) "Fun.minBy: Empty range."
  let mutable minV = f i0
  let mutable minP = projection minV
  for i = i0 + 1 to i1 do
    let v = f i
    let p = projection v
    if p < minP then
      minV <- v
      minP <- p
  minV


/// Returns the function value with the maximum projection in [i0, i1].
let inline maxBy i0 i1 f projection =
  enforce (i0 <= i1) "Fun.maxBy: Empty range."
  let mutable maxV = f i0
  let mutable maxP = projection maxV
  for i = i0 + 1 to i1 do
    let v = f i
    let p = projection v
    if p > maxP then
      maxV <- v
      maxP <- p
  maxV
  

/// Returns the argument of the minimum of f in [i0, i1].
let inline argMin i0 i1 (f : int -> _) =
  enforce (i0 <= i1) "Fun.argMin: Empty range."
  let mutable imin = i0
  let mutable vmin = f i0
  for i = i0 + 1 to i1 do
    let v = f i
    if v < vmin then imin <- i; vmin <- v
  imin


/// Returns the argument of the maximum of f in [i0, i1].
let inline argMax i0 i1 (f : int -> _) =
  enforce (i0 <= i1) "Fun.argMax: Empty range."
  let mutable imax = i0
  let mutable vmax = f i0
  for i = i0 + 1 to i1 do
    let v = f i
    if v > vmax then imax <- i; vmax <- v
  imax


/// Searches for an integer satisfying the predicate in [i0, i1]. Checks integers
/// in ascending order. If one is found, invokes the success continuation.
/// If none are found, or the range is empty, invokes the failure continuation.
let inline find i0 i1 (predicate : _ -> bool) (success : _ -> _) (failure : unit -> _) =
  let rec loop i =
    if i <= i1 then
      if predicate i then success i else loop (i + 1G)
    else failure()
  loop i0


/// Searches for an integer satisfying the predicate in [i0, i1]. Checks integers
/// in descending order. If one is found, invokes the success continuation.
/// If none are found, or the range is empty, invokes the failure continuation.
let inline findBack i0 i1 (predicate : _ -> bool) (success : _ -> _) (failure : unit -> _) =
  let rec loop i =
    if i >= i0 then
      if predicate i then success i else loop (i - 1G)
    else failure()
  loop i1


/// Searches for an integer satisfying the predicate in [i0, i1].
/// Returns the smallest integer found. If none are found, returns Noneval.
let inline findArg i0 i1 (predicate : _ -> bool) =
  let mutable i = i0
  let mutable result = Noneval
  let mutable loop = i <= i1
  while loop do
    if predicate i then
      result <- Someval(i)
      loop <- false
    elif i < i1 then
      i <- i + 1G
    else loop <- false
  result


/// Searches for an integer satisfying the predicate in [i0, i1].
/// Returns the largest integer found. If none are found, returns Noneval.
let inline findArgBack i0 i1 (predicate : _ -> bool) =
  let mutable i = i1
  let mutable result = Noneval
  let mutable loop = i >= i0
  while loop do
    if predicate i then
      result <- Someval(i)
      loop <- false
    elif i > i0 then
      i <- i - 1G
    else loop <- false
  result


/// Returns the entropy (in bits) of a function of frequencies (which need not sum to 1)
/// from the integer range [i0, i1].
let inline entropy i0 i1 (f : int -> float) =
  enforce (i0 <= i1) "Fun.entropy: Empty range."
  if i0 = i1 then 0.0 else
    let M = reduce i0 i1 f (+)
    enforce (M > 0.0) "Fun.entropy: Range sum is zero."
    let Z = 1.0 / M
    let mutable E = 0.0
    for i = i0 to i1 do
      let p = Z * f i
      if p > 0.0 then E <- E - p * log2(p)
    E


/// Detects cycles of the given function of length up to 2^P (P < 63). If found, returns the length of the cycle.
/// Algorithm devised by Richard P. Brent.
let detectCycle (x0 : 'a) (f : 'a -> 'a) P =
  enforce (P < 63) "Fun.detectCycle: Period too large."
  let mutable x = x0
  let mutable p = 1
  let mutable L = Noneval
  while p < P && L.isNone do
    // We make 2^p checks in this loop.
    let n = 1L <<< p
    let mutable y = f x
    let mutable delta = 1L // number of iterations between x and y
    while delta <= n && x <> y do
      y <- f y
      delta <- delta + 1L
    if x = y then L <- Someval(delta)
    x <- y
    p <- p + 1
  L


/// Returns true iff the predicate is true for all integers in [i0, i1].
/// Returns true if the range is empty.
let inline forall i0 i1 (predicate : _ -> bool) =
  // Make sure we can represent i1 + 1.
  enforce (i1 < i1 + 1G) "Fun.forall: Arithmetic overflow."
  let mutable result = true
  let mutable i = i0
  while i <= i1 do
    if predicate i then
      i <- i + 1G
    else
      i <- i1 + 1G
      result <- false
  result


/// Returns true iff the predicate is true for some argument in [i0, i1].
/// Returns false if the range is empty.
let inline exists i0 i1 (predicate : _ -> bool) =
  // Make sure we can represent i1 + 1.
  enforce (i1 < i1 + 1G) "Fun.exists: Arithmetic overflow."
  let mutable result = false
  let mutable i = i0
  while i <= i1 do
    if predicate i then
      i <- i1 + 1G
      result <- true
    else
      i <- i + 1G
  result
  

/// Returns the number of times the predicate is true in [i0, i1].
let inline count i0 i1 (predicate : int -> bool) =
  let mutable count = 0
  for i = i0 to i1 do
    if predicate i then count <- count + 1
  count


/// Returns the nth (n > 0) integer in [i0, i1] that satisfies the predicate, or Noneval
/// if the predicate is not satisfied that many times. The range is iterated in ascending order.
let inline nth i0 i1 n (predicate : _ -> bool) =
  enforce (i1 < i1 + 1G) "Fun.nth: Arithmetic overflow."
  let mutable count = 0
  let mutable result = Noneval
  let mutable i = i0
  while i <= i1 do
    if predicate i then
      count <- count + 1
      if count = n then
        result <- Someval(i)
        i <- i1
    i <- i + 1G
  result


/// Calls the function with arguments in [i0, i1] in ascending order.
let inline iter i0 i1 (f : int -> _) =
  for i = i0 to i1 do f i


/// Reverses the range [i0, i1].
let inline reverse i0 i1 (get : int -> 'a) (set : int -> 'a -> unit) =
  let rec loop i0 i1 =
    if i0 < i1 then
      let tmp = get i0
      set i0 (get i1)
      set i1 tmp
      loop (i0 + 1) (i1 - 1)
  loop i0 i1


/// Folds in [i0, i1] using the function get to retrieve items and the function fold
/// to fold them into the state.
let inline fold i0 i1 (state0 : 'a) (get : int -> 'b) (fold : 'a -> 'b -> 'a) =
  let mutable state = state0
  for i = i0 to i1 do
    state <- fold state (get i)
  state


/// Pinpoints, via interval halving, argument x in [x0, x1] (x0 <= x1) for which f(x) = value.
/// If the value is not reached exactly, returns the supremum of arguments for which f(x) < value.
/// The function f must be monotonic. Returns the midpoint of the range if f(x0) = f(x1).
let inline pinpointArg (x0 : 'a) (x1 : 'a) (value : 'a) (f : 'a -> 'a) : 'a =
  let mutable x0 = x0
  let mutable x1 = x1
  let mutable x = average x0 x1
  let mutable fx0 = f x0
  let mutable fx1 = f x1
  let slope = signum (fx1 - fx0)
  if slope = 0G then
    x
  else
    while x0 < x && x < x1 do
      let fx = f x
      if fx * slope < value * slope then
        x0 <- x
        fx0 <- fx
      else
        x1 <- x
        fx1 <- fx
      x <- average x0 x1
    x


/// Selects from the argument range [i0, i1] an item that is greater than or equal to exactly k items.
/// Runs in linear time using a divide-and-choose algorithm. Rearranges items via the swap function.
/// Returns the final index of the requested item.
let inline quickselect i0 i1 (projection : int -> _) (swap : int -> int -> unit) k =
  let inline reorder i0 i1 =
    if projection i0 > projection i1 then
      swap i0 i1
      true
    else false
  enforce (i0 <= i1) "Fun.quickselect: Empty range."
  enforce (k >= 0 && k <= i1 - i0) "Fun.quickselect: Order statistic out of range."
  let rec recurse i0 i1 k =
    if i0 + 3 > i1 then
      // We have at most 3 elements.
      if i0 = i1 then i0 else
        let im = i0 + 1
        reorder i0 im |> ignore
        if im = i1 then i0 + k else
          if reorder im i1 then reorder i0 im |> ignore
          i0 + k
    else
      // Pick as the pivot the median of first, middle and last elements of the range.
      let ip = i0 + ((i1 - i0) >>> 1)
      reorder i0 ip |> ignore
      if reorder ip i1 then reorder i0 ip |> ignore
      let pivot = projection ip
      // Partition the range.
      let mutable p0 = i0 + 1
      let mutable p1 = i1 - 1
      while p0 < p1 do
        while p0 < p1 && projection p0 <= pivot do p0 <- p0 + 1
        while p0 < p1 && pivot <= projection p1 do p1 <- p1 - 1
        if p0 < p1 then
          swap p0 p1
          p0 <- p0 + 1
          p1 <- p1 - 1
      if projection p0 > pivot then p0 <- p0 - 1
      // Now we have partitions [i0, p0] and [p0 + 1, i1].
      let ik = i0 + k
      if ik <= p0 then
        recurse i0 p0 k
      else
        let n0 = p0 - i0 + 1
        recurse (p0 + 1) i1 (k - n0)
  recurse i0 i1 k


/// Insertion sorts a range. This is O(n^2) and thus efficient for small ranges only.
let inline insertionSort i0 i1 (projection : int -> _) (swap : int -> int -> unit) =
  for i = i0 + 1 to i1 do
    let p = projection i
    let mutable j = i
    while j > i0 && projection (j - 1) > p do
      swap j (j - 1)
      j <- j - 1


/// Quicksorts in [i0, i1], comparing arguments with the given projection.
/// Quicksort is fast and in-place but not stable.
let inline quicksort i0 i1 (projection : int -> _) (swap : int -> int -> unit) =
  let inline reorder i0 i1 =
    if projection i0 > projection i1 then swap i0 i1; true else false
  let rec recurse i0 i1 =
    if i0 + 6 > i1 then
      // Use insertion sort for small ranges.
      insertionSort i0 i1 projection swap
    else
      // Pick as the pivot the median of first, middle and last elements of the range.
      let ip = i0 + ((i1 - i0) >>> 1)
      reorder i0 ip |> ignore
      if reorder ip i1 then reorder i0 ip |> ignore
      let pivot = projection ip
      // Partition the range.
      let mutable p0 = i0 + 1
      let mutable p1 = i1 - 1
      while p0 < p1 do
        while p0 < p1 && projection p0 <= pivot do p0 <- p0 + 1
        while p0 < p1 && pivot <= projection p1 do p1 <- p1 - 1
        if p0 < p1 then
          swap p0 p1
          p0 <- p0 + 1
          p1 <- p1 - 1
      if projection p0 > pivot then p0 <- p0 - 1
      let n0 = p0 - i0 + 1
      let n1 = i1 - p0
      // Recurse to the smaller partition first to keep stack growth in check.
      if n0 >= n1 then
        // Move the pivot from the first partition to between the two partitions and exclude it from recursion.
        if ip <> p0 then swap ip p0
        recurse (p0 + 1) i1
        let mutable I0 = p0 - 1
        // If there is a large imbalance, attempt to fix it by compacting elements equal to the pivot
        // and excluding them from recursion. This prevents masses of duplicate elements
        // from blowing up our running time.
        if (n0 >>> 3) > n1 then
          p0 <- i0
          while p0 < I0 do
            if projection p0 = pivot then swap p0 I0 ; I0 <- I0 - 1
            p0 <- p0 + 1
        recurse i0 I0
      else
        // Move the pivot from the second partition to between the two partitions and exclude it from recursion.
        if ip <> (p0 + 1) then swap ip (p0 + 1)
        recurse i0 p0
        let mutable I1 = (p0 + 2)
        // If there is a large imbalance, attempt to fix it by compacting elements equal to the pivot.
        if (n1 >>> 3) > n0 then
          p1 <- i1
          while I1 < p1 do
            if projection p1 = pivot then swap I1 p1; I1 <- I1 + 1
            p1 <- p1 - 1
        recurse I1 i1

  recurse i0 i1


/// Traces the integer coordinates of a line from (x0, y0) to (x1, y1) by calling the supplied function.
/// Does not make diagonal jumps.
let inline traceLine2 (x0 : 'a) (y0 : 'a) (x1 : 'a) (y1 : 'a) (f : int -> int -> unit) =
  let mutable ix = int x0
  let mutable iy = int y0
  let ix1 = int x1
  let iy1 = int y1
  let deltax = x1 - x0
  let deltay = y1 - y0
  let incx = sign deltax
  let incy = sign deltay
  let n = abs (ix1 - ix) + abs (iy1 - iy)
  if ix = ix1 then
    for i = 0 to n do
      f ix iy
      iy <- iy + incy
  elif y0 = y1 then
    for i = 0 to n do
      f ix iy
      ix <- ix + incx
  else
    let deltanorm = sqrt (squared deltax + squared deltay)
    let directionx = deltax / deltanorm
    let directiony = deltay / deltanorm
    let invx = G incx / directionx
    let invy = G incy / directiony
    let mutable dx = abs (G (ix + ((incx + 1) >>> 1)) - x0) * invx
    let mutable dy = abs (G (iy + ((incy + 1) >>> 1)) - y0) * invy
    for i = 0 to n do
      f ix iy
      if dx < dy then
        ix <- ix + incx
        dy <- dy - dx
        dx <- invx
      else
        iy <- iy + incy
        dx <- dx - dy
        dy <- invy


/// Computes edit distance from source to target. matchCost x y returns the cost of replacing x with y.
/// If x and y are equal, the cost is typically zero. Note that replace cost values higher than
/// deleteCost + insertCost have no effect because it becomes cheaper to delete x and insert y.
let inline editDistance sourceSize (getSource : int -> 'a) targetSize (getTarget : int -> 'a) deleteCost insertCost (matchCost : 'a -> 'a -> float) =

  // The initializer sets i = 0 (source index) and j = 0 (target index) base cases once and for all.
  let d = Array2D.init (sourceSize + 1) (targetSize + 1) (fun i j -> float i * deleteCost + float j * insertCost)

  for i = 1 to sourceSize do
    let i0 = i - 1
    for j = 1 to targetSize do
      let j0 = j - 1
      d.[i, j] <- min3 (d.[i0, j] + deleteCost)
                       (d.[i, j0] + insertCost)
                       (d.[i0, j0] + matchCost (getSource i0) (getTarget j0))

  d.[sourceSize, targetSize]


