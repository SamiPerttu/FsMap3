/// Special math functions and utilities.
module FsMap3.Mat

open Common


/// Sum of a geometric series with n terms. Terms are defined by first term a0 and the ratio between successive terms;
/// term i is a0 * ratio ** (i - 1), 1 <= i <= n.
let inline geometricSum n (a0 : 'a) (ratio : 'a) : 'a =
  if ratio <> 1G then a0 * (1G - pow ratio n) / (1G - ratio) else a0 * G n

/// Sum of an arithmetic series with n terms. Terms are defined by first term a0 and the difference between successive
/// terms; term i is a0 + delta * (i - 1), 1 <= i <= n.
let inline arithmeticSum n (a0 : 'a) (delta : 'a) : 'a =
  G n * (2G * a0 + delta * (G (n - 1))) / 2G

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

/// Gaussian CDF approximation by W Bryc (2002). Returns probability that the standard normal distribution has value <= x.
/// The largest absolute error is 0.000019. The largest relative error, 0.5%, occurs in the tail, when abs x > 11.8.
let gaussianCdf x =
  let inline cdf z = (z * z + 5.575192695 * z + 12.77436324) / (2.506628274631 * z * z * z + 14.38718147 * z * z + 31.53531977 * z + 25.54872648) * exp(-0.5 * z * z)
  if x > 0.0 then 1.0 - cdf x else cdf -x

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

