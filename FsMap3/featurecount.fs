/// Texture cell feature count functions.
module Fuse.FeatureCount

open Common


/// Some cell based maps support a variable number of features per cell. A feature count function translates
/// the hash value of a cell into its feature count. The cell hash is "almost" clean and needs at most a slight retouch.
type FeatureCount = int -> int


/// One feature per cell.
let unityCount (_ : int) = 1

/// Number of features picked from the Poisson distribution with the given mean.
let poissonCount mean =
  let f = Convert.poisson mean
  fun (h : int) -> f (h * 0x714f69)

/// Number of features picked from the unity Poisson distribution Pois(1).
let unityPoisson = poissonCount 1.0

/// Constant number of features per cell.
let exactCount (n : int) (_ : int) = n

/// Rounds number of features up or down probabilistically.
let roundCount (n : float) =
  let i = int n
  let n = n - float i
  fun (h : int) ->
    if Convert.float01 LeftClosed (h * 0x1d39bb4f) < n then i + 1 else i

/// Number of features picked from the geometric distribution with the given mean.
let geometricCount mean h = Convert.geometric (1.0 / (1.0 + mean)) (h * 0x46eb1f5)

