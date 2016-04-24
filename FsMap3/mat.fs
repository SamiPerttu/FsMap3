/// Special mathematics functions and utilities.
module FsMap3.Mat

open Common


/// Keeps accurate running estimates of the mean, deviation and variance of a set of real-valued samples.
/// Samples can be weighted. Existing samples can be discounted.
type MomentEstimator() =

  let mutable n = 0L // number of samples
  let mutable W = 0.0 // total weight
  let mutable m = 0.0 // mean
  let mutable m2 = 0.0 // variance variable

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
  /// with sample mean being mapped to 1/2.
  member this.softmaxNormalize(sigma, x) =
    let deviation = this.deviation
    if deviation > 0.0 then
      logistic ((x - this.mean) / (sigma * deviation))
    else
      logistic (x - this.mean)

