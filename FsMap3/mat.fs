/// Special math functions and utilities.
module Fuse.Mat

open Common


/// Sum of an arithmetic series with n terms. That is, sum over i in [0, n[ of a0 + delta * i.
let inline arithmeticSum n (a0 : 'a) (delta : 'a) : 'a =
  G n * (2G * a0 + delta * (G (n - 1))) / 2G

/// Sum of a geometric series with n terms. That is, sum over i in [0, n[ of a0 * ratio ** i.
let inline geometricSum n (a0 : 'a) (ratio : 'a) : 'a =
  if ratio <> 1G then a0 * (1G - pow ratio n) / (1G - ratio) else a0 * G n

/// Inverse hyperbolic sine.
let inline asinh (x : 'a) : 'a = log (x + sqrt (1G + squared x))

/// Inverse hyperbolic cosine.
let inline acosh (x : 'a) : 'a = log (x + (x + 1G) * sqrt ((x - 1G) / (x + 1G)))

/// Inverse hyperbolic tangent.
let inline atanh (x : 'a) : 'a = Q 1 2 * log ((1G + x) / (1G - x))

/// This exp-like, quadratic response function has a second order discontinuity at the origin but
/// has the property f(x) = 1.0 / f(-x). f(x) > 0 for all x. Like the exponential function, f(0) = f'(0) = 1.
let inline qesp (x : 'a) : 'a = if x < 0G then 1G / (1G - x + squared x) else 1G + x + squared x

/// This exp-like, smooth response function is second order continuous. It has asymmetrical magnitude curves:
/// linear when x < 0 and quadratic when x > 0. f(x) > 0 for all x. Like the exponential function, f(0) = f'(0) = 1.
let inline esp (x : 'a) : 'a = if x < 0G then 1G / (1G - x) else 1G + x + squared x

/// This exp-like, smooth response function is second order continuous. It has asymmetrical magnitude curves:
/// linear when x < 0 and quartic when x > 0. f(x) > 0 for all x. Like the exponential function, f(0) = f'(0) = 1.
let inline eqp (x : 'a) : 'a = if x < 0G then 1G / (1G - x) else 1G + x + squared x * (1G + Q 1 8 * squared x)

/// An accurate logarithm of the gamma function (n! ~ exp(lgamma(n + 1))).
let lgamma x =
  let i = 1.0 / x
  let i3 = cubed i
  (x - 0.5) * log x - x + 0.918938533204673 + 0.0833333333333333333 * i - (0.002777777777777777 - 0.000793650793650794 * i * i + 0.000595238095238095 * i3 * i) * i3

/// Logarithm of the complete beta function.
let lbeta x y = lgamma x + lgamma y - lgamma (x + y)

/// Logarithm of the binomial coefficient (choose n k).
let lchoose n k = lgamma (float (n + 1)) - lgamma (float (k + 1)) - lgamma (float (n - k + 1))

/// Probability mass function of the binomial distribution B(n, p), with value i in [0, n].
let binomialPmf n p i =
  if p <= 0.0 then kronecker i 0
  elif p >= 1.0 then kronecker i n
  else lchoose n i + float i * log p + float (n - i) * log (1.0 - p) |> exp

/// Pearson's chi-squared statistic for comparing two binary outcomes (a, b) and (c, d).
/// With one degree of freedom, the chi-squared statistics corresponding to some p-values are:
/// 3.841 (p = 0.05), 5.412 (p = 0.02), 6.635 (p = 0.01), 10.827 (p = 0.001).
let inline pearson a b c d =
  let a, b, c, d = float a, float b, float c, float d
  squared(a * d - b * c) * (a + b + c + d) / ((a + b) * (c + d) * (b + d) * (a + c))

/// Gaussian CDF approximation by W Bryc (2002). Returns probability that the standard normal distribution has value <= x.
/// The largest absolute error is 0.000019. The largest relative error, 0.5%, occurs in the tail, when abs x > 11.8.
let gaussianCdf x =
  let inline cdf z = (z * z + 5.575192695 * z + 12.77436324) / (2.506628274631 * z * z * z + 14.38718147 * z * z + 31.53531977 * z + 25.54872648) * exp(-0.5 * z * z)
  if x > 0.0 then 1.0 - cdf x else cdf -x

/// Binary entropy: the amount of information in a binary random variable with the given p.
/// The result is returned in bits and ranges from 0 to 1.
let entropy (p : float) =
  if p > 0.0 && p < 1.0 then (p * log p + (1.0 - p) * log(1.0 - p)) / -ln2 else 0.0



/// Numerical differentiation. Estimates the derivative of f at x with a simple two-point method.
let derivative (f : float -> float) x =
  // A delta of one millionth consumes about 20 bits of precision.
  // This is no problem for derivatives of ordinary functions.
  let delta = 1.0e-6
  let h = max 1.0e-9 (abs x * delta)
  (f(x + h) - f(x - h)) / (2.0 * h)



/// Ascends to a local maximum of function f, starting at x. Returns the maximum argument.
let ascendArg (f : float -> float) x0 =
  let mutable iteration = 0
  let maxIterations = 1000000

  let mutable d = 1.0e-6
  let mutable x = x0
  let mutable y = f x

  while abs(d) > 1.0e-30 && iteration < maxIterations do
    let x' = x + d
    let y' = f x'
    if y' > y then
      x <- x'
      y <- y'
      d <- d * 1.2
    else
      d <- d * -0.5
    iteration <- iteration + 1

  x

/// Descends to a local minimum of function f, starting at x. Returns the minimum argument.
let descendArg (f : float -> float) x = ascendArg (fun y -> -f y) x

/// Ascends to a local maximum of function f, starting at x. Returns the maximum value.
let ascend f x = f (ascendArg f x)

/// Descends to a local minimum of function f, starting at x. Returns the minimum value.
let descend f x = f (descendArg f x)



/// Greatest common divisor. a, b >= 0.
let gcd a b =
  // This is the binary GCD algorithm.
  let rec gcdr a b =
    if a = b then
      a
    elif a = 0 then
      b
    elif b = 0 then
      a
    elif a &&& 1 = 0 then
      if b &&& 1 <> 0 then
        gcdr (a >>> 1) b
      else
        2 * gcdr (a >>> 1) (b >>> 1)
    elif b &&& 1 = 0 then
      gcdr a (b >>> 1)
    elif a > b then
      gcdr ((a - b) >>> 1) b
    else
      gcdr ((b - a) >>> 1) a
  enforce (a >= 0 && b >= 0) "Mat.gcd: Arguments must be non-negative."
  gcdr a b

/// Binomial coefficient: (choose n k), where n >= k, is the number of k-element subsets in an n-element set.
let inline choose (n : 'a) (k : 'a) : 'a =
  let k = if k < n / 2G then k else n - k
  let mutable result = 1G
  let mutable i      = 1G
  while i <= k do
    // The division is always without remainder here.
    result <- result * (n - k + i) / i
    i      <- i + 1G
  result

/// The (normalized) sinc function.
let inline sinc (x : 'a) : 'a = if x = 0G then 1G else let px = G(pi) * x in sin(px) / px

// Note. The window functions below are defined in a form convenient for sinc sampling.
// They peak at origin and their support is ]-a, a[.

/// The Lanczos window. Window order is a. Jim Blinn advocates the Lanczos kernel with a = 3 for image resampling:
/// "it keeps low frequencies and rejects high frequencies better than any (achievable) filter we've seen so far."
let lanczos a x = if abs x < a then sinc (x / a) else 0.0

/// The Hann window (also known as the Hanning window). Window order is a.
/// The Hann window has good spectral resolution and the side-lobes drop off at 18 dB/octave.
let hann a x = if abs x < a then 0.5 + 0.5 * cos(pi * x / a) else 0.0

/// The Hamming window. Window order is a. The Hamming window, relative to the Hann window, minimizes
/// the height of the nearest side-lobes (to -44 dB) at the expense of drop-off, which becomes negligible.
let hamming a x = if abs x < a then 0.54 + 0.46 * cos(pi * x / a) else 0.0

/// The Nuttall window. Window order is a. The Nuttall window trades spectral resolution for
/// significantly reduced leakage: the first side-lobe is only -93 dB.
let nuttall a x =
  if abs x < a then
    let z = pi * x / a
    0.355768 - 0.487396 * cos(z) + 0.144232 * cos (2.0 * z) - 0.012604 * cos (3.0 * z)
  else 0.0

/// The Blackman-Nuttall window. Window order is a. The Blackman-Nuttall window lowers leakage
/// even further compared to the Nuttall window: the first side-lobe is only -98 dB.
/// This is done at the expense of drop-off, which becomes negligible.
let blackmanNuttall a x =
  if abs x < a then
    let z = pi * x / a
    0.3635819 - 0.4891775 * cos(z) + 0.1365995 * cos (2.0 * z) - 0.0106411 * cos (3.0 * z)
  else 0.0

/// A flat-top window. Flat-top windows are designed for estimating amplitudes of pure frequency
/// components. This is achieved by spreading the energy of a pure frequency over multiple bins
/// in the spectrum, ensuring that its unattenuated amplitude can be found on at least
/// one of the neighboring bins. The frequency resolution is poor as a consequence.
let flatTop a x =
  if abs x < a then
    let z = pi * x / a
    1.0 - 1.93 * cos(z) + 1.29 * cos (2.0 * z) - 0.388 * cos (3.0 * z) + 0.028 * cos (4.0 * z)
  else 0.0



/// Internal helper for Hilbert curve functions.
let inline internal rotateHilbert n x y rx ry =
  if ry = 0 then
    if rx = 1 then
      Pair(n - 1 - y, n - 1 - x)
    else Pair(y, x)
  else Pair(x, y)


/// Returns (x, y) location of point i of iteration n of the Hilbert curve.
/// Iteration number n > 0. The range of i is [0, 4^n[.
let hilbert n i =
  let n = 1 <<< n
  let mutable t = i
  let mutable x = 0
  let mutable y = 0
  let mutable s = 1
  while s < n do
    let rx = 1 &&& (t >>> 1)
    let ry = 1 &&& (t ^^^ rx)
    let (Pair(x', y')) = rotateHilbert s x y rx ry
    x <- x' + s * rx
    y <- y' + s * ry
    t <- t >>> 2
    s <- s <<< 1
  Pair(x, y)


/// Returns point i of the location (x, y) of iteration n of the Hilbert curve.
/// Iteration number n > 0. The range of both coordinates is [0, 2^n[.
let hilberti n x y =
  let n = 1 <<< n
  let mutable i = 0
  let mutable x = x
  let mutable y = y
  let mutable s = n >>> 1
  while s > 0 do
    let rx = if (x &&& s > 0) then 1 else 0
    let ry = if (y &&& s > 0) then 1 else 0
    i <- i + s * s * ((3 * rx) ^^^ ry)
    let (Pair(x', y')) = rotateHilbert s x y rx ry
    x <- x'
    y <- y'
    s <- s >>> 1
  i



/// Keeps accurate running estimates of the mean, deviation and variance of a set of real-valued samples.
/// Samples can be weighted. Existing samples can be discounted.
type MomentEstimator() =

  /// Number of samples.
  let mutable n = 0L
  /// Total sample weight.
  let mutable W = 0.0
  /// Sample mean.
  let mutable m = 0.0
  /// Variance tracking variable.
  let mutable m2 = 0.0

  /// The current number of samples.
  member this.samples = n

  /// Weighted mean of the samples.
  member this.mean = m

  /// Total weight of the samples. Takes discounting into account.
  member this.weight = W

  /// An unbiased estimate of the weighted variance of the samples.
  member this.variance = if n > 1L then m2 / W * float n / float (n - 1L) else 0.0

  /// An unbiased estimate of the weighted standard deviation of the samples.
  member this.deviation = sqrt this.variance

  /// Adds a sample x.
  member this.add(x) = this.add(1.0, x)

  /// Adds a sample x with weight w.
  member this.add(w, x) =
    let wsign = sign w
    n <- n + int64 wsign
    if wsign > 0 || n > 0L then
      // This update retains as much precision as possible, particularly in the variance estimate.
      let delta = x - m
      let R = delta * w / (W + w)
      m <- m + R
      m2 <- m2 + W * delta * R
      W <- W + w
    else
      W <- 0.0
      m <- 0.0
      m2 <- 0.0

  /// Removes a sample x. This can be used to implement a windowed estimator.
  member this.remove(x) =
    enforce (n > 0L) "MomentEstimator.remove: No sample to remove."
    this.add(-1.0, x)

  /// Removes a sample x with weight w. This can be used to implement a windowed estimator.
  /// The weight should match the weight the sample was added with.
  member this.remove(w, x) =
    enforce (n > 0L) "MomentEstimator.remove: No sample to remove."
    this.add(-w, x)

  /// Discounts the weights of existing samples by multiplying them with the given factor.
  /// Note, samples should not be removed if you use discounting (and vice versa)!
  member this.discount(factor) =
    W <- W * factor
    m2 <- m2 * factor

  /// Resets the estimator.
  member this.reset() =
    n <- 0L
    W <- 0.0
    m <- 0.0
    m2 <- 0.0

  /// Uses the current estimates to softmax normalize the value x. Softmax normalization reduces the significance
  /// of outliers while maintaining linearity of samples within sigma standard deviations of the mean
  /// (cf. the "three sigma rule"). Typical values for sigma are in [1, 3]. The return value is in unit range,
  /// with sample mean mapped to 1/2.
  member this.softmaxNormalize(sigma, x) =
    let deviation = this.deviation
    if deviation > 0.0 then
      logistic ((x - this.mean) / (sigma * deviation))
    else
      logistic (x - this.mean)

