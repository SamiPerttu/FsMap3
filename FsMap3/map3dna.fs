/// Procedural generation of Map3 and related functions.
module FsMap3.Map3Dna

open Common
open Basis3
open Map3
open Atlas3
open Potential
open Perlin
open Cubex
open Radial
open Leopard
open Peacock
open Worley
open Capflow
open Impflow
open Julia
open Orbit
open CellColor
open FeatureCount
open FadeDna


let layoutChoices =
  Choices(
    C(1.0e9, "No Tiling", Layout.Hifi),
    C("Tile All", Layout.Tile),
    C("Tile X Only", Layout.TileX),
    C("Tile Y Only", Layout.TileY),
    C("Tile Z Only", Layout.TileZ)
    )


/// Generates a layout.
let genLayout (dna : Dna) =
  dna.category("Layout", layoutChoices)


/// Generates a layout that does not rotate.
let genFixedLayout (dna : Dna) =
  match genLayout dna with | Layout.Hifi -> Layout.Offset | x -> x


/// Generates a unit cube offset.
let genOffset (dna : Dna) =
  let x = dna.float32("X offset")
  let y = dna.float32("Y offset")
  let z = dna.float32("Z offset")
  translate (Vec3f(x, y, z))


/// Generates a cell coloring function.
let genCellColor (dna : Dna) =
  dna.branch("Cell color",
    C("unit", fun _ -> unitColor),
    C("big", fun _ -> bigColor),
    C(2.0, "any", fun _ -> anyColor),
    C(0.8, "random choice", fun _ -> anyColors (dna.int("Choices", 1, 8)) (dna.data("Seed"))),
    C(0.6, "high choice", fun _ -> highColors (dna.int("Choices", 1, 8)) (dna.data("Seed")) 0.5f),
    C(0.6, "unit choice", fun _ -> unitColors (dna.int("Choices", 1, 8)) (dna.data("Seed")))
    )


/// Generates a potential function.
let genPotential (dna : Dna) =
  dna.branch("Potential function",
    C("rounded cube", fun _ -> roundedCube),
    C("teardrop", fun _ -> teardrop),
    C("rounded cylinder", fun _ ->
      roundedCylinder (dna.float32("Cylinder radius", Fade.hump >> lerp 0.05f 1.0f))
      ),
    C("rounded cone", fun _ ->
      roundedCone (dna.float32("Cone radius", Fade.hump >> lerp 0.05f 1.0f))
      ),
    C(0.5, "torus", fun _ ->
      torus (dna.float32("Major axis", lerp 0.5f 0.95f))
      ),
    C(2.0, "superellipsoid", fun _ ->
      superellipsoid (dna.float32("Radial power", xerp 0.5f 8.0f)) (dna.float32("Transverse power", xerp 0.5f 8.0f))
      ),
    C(1.5, "supertorus", fun _ ->
      supertorus (dna.float32("Ring power", xerp 1.0f 8.0f)) (dna.float32("Major axis", lerp 0.5f 0.95f))
      ),
    C("supercone", fun _ ->
      supercone (dna.float32("Radial power", xerp 1.0f 8.0f)) (dna.float32("Cone radius", Fade.hump >> lerp 0.05f 1.0f))
      ),
    C("Roman", fun _ -> roman)
    )


/// Generates a potential function and feature radius. Constrains radius in an attempt
/// to get the potential to occupy at least 1% of the volume of the unit sphere.
let genPotentialAndRadius (dna : Dna) =
  let potential = genPotential dna
  // TODO. Improve sampling efficiency.
  let volume = sampleVolume 500 potential |> max 0.001f
  let minVolume = 0.01f
  let volumeFactor = volume / minVolume
  let minRadius = clamp 0.1f 0.9f (1.0f / cbrt volumeFactor)
  let radius = dna.float32("Potential radius", sqrt >> lerp minRadius 1.0f)
  (potential, radius)


/// Generates a feature count function.
let genFeatureCount (dna : Dna) =
  dna.branch("Features per cell",
    C(1.0, "one", fun _ -> unityCount),
    C(1.0, "round", fun _ -> flipCount (dna.float("Average", xerp 0.2 4.0))),
    C(2.5, "Poisson", fun _ -> poissonCount (dna.float("Mean", xerp 0.2 4.0))),
    C(1.0, "geometric", fun _ -> geometricCount (dna.float("Mean", xerp 0.2 4.0)))
    )


/// Generates a mixing operator for octave or map mixing.
let genMixOp (dna : Dna) =
  dna.branch("Mix operator",
    C(4.0, "sum", fun _ -> Mix.sum),
    C(1.5, "over", fun _ -> Mix.over),
    C(1.5, "norm", fun _ -> Mix.norm (dna.float32("Hardness"))),
    C(2.0, "soft", fun _ -> Mix.soft (dna.float32("Bias", lerp -5.0f 5.0f))),
    C(3.0, "layer", fun _ -> Mix.layer (dna.float32("Layer width", lerp 0.1f 1.0f)) (genLayerFade dna) (dna.float32("Layer persist", lerp 0.1f 0.9f)))
    )


/// Generates a mixing operator for intra-basis (feature) mixing.
let genBasisMixOp (dna : Dna) =
  dna.branch("Mix operator",
    C(4.0, "sum", fun _ -> Mix.sum),
    C(1.5, "over", fun _ -> Mix.over),
    C(0.75, "min", fun _ -> Mix.min),
    C(0.75, "max", fun _ -> Mix.max),
    C(1.0, "norm", fun _ -> Mix.norm (dna.float32("Hardness"))),
    C(1.0, "soft", fun _ -> Mix.soft (dna.float32("Bias", lerp -5.0f 5.0f)))
    )


/// Generates a cell distance function.
let genCellDistance (dna : Dna) =
  dna.branch("Cell distance function",
    C(1.0, "1-norm", fun _ -> cellNorm1),
    C(0.5, "low norm", fun _ -> cellNorm (dna.float32("Exponent", lerp 1.1f 1.9f))),
    C(2.5, "2-norm", fun _ -> cellNorm2),
    C(0.5, "3-norm", fun _ -> cellNorm3),
    C(1.0, "4-norm", fun _ -> cellNorm4),
    C(0.5, "6-norm", fun _ -> cellNorm6),
    C(0.5, "8-norm", fun _ -> cellNorm8),
    C(0.5, "max-norm", fun _ -> cellNormMax)
    )


/// Generates a scattering map.
let genScatter (dna : Dna) =
  let dx = dna.float32("X offset", lerp -0.5f 0.5f)
  let dy = dna.float32("Y offset", lerp -0.5f 0.5f)
  let dz = dna.float32("Z offset", lerp -0.5f 0.5f)
  let wave = dna.category("Scatter wave", C("triangle", tri), C("smooth triangle", Fade.sinefy Fade.smoothLine), C("sine", sin))
  scatter wave (Vec3f(dx, dy, dz))


/// Generates a bleed map.
let genBleed (dna : Dna) =
  let b1 = dna.float32("Bleed 1", lerp -1.0f 1.0f)
  let b2 = dna.float32("Bleed 2", lerp -1.0f 1.0f)
  fun (v : Vec3f) -> Vec3f(v.x + b1 * v.z + b2 * v.y, v.y + b1 * v.x + b2 * v.z, v.z + b1 * v.y + b2 * v.x)


/// Generates a posterizer map.
let genPosterize (dna : Dna) =
  let levels = dna.float32("Posterize levels", xerp 1.0f 20.0f)
  crush3 (genSigmoidFade "Posterize hardness" dna) levels


/// Generates a vector reflect map.
let genVectorReflect (dna : Dna) =
  let fade, factor = dna.category("Reflect fade", C(0.5, "hump", (Fade.hump, 1.0f)),
                                                  C("line", (Fade.line, 1.0f)),
                                                  C("smooth line", (Fade.smoothLine, 1.0f)),
                                                  C("smooth", (Fade.smooth, 1.0f)),
                                                  C("super smooth", (Fade.super, 1.0f)),
                                                  C("wave", (Fade.worm 2, 0.6f)),
                                                  C(0.5, "spike", (Fade.spike, 0.8f)))
  let amount = dna.float32("Reflect amount", lerp 2.0f 8.0f) * factor
  reflectNormf fade amount


/// Generates a component reflect map.
let genReflect (dna : Dna) =
  let wave = dna.category("Reflect fade", C(1.5, "line", tri),
                                          C("smooth line", Fade.sinefy Fade.smoothLine),
                                          C("sine", sinFast),
                                          C("smooth", Fade.sinefy Fade.smooth),
                                          C("spike", Fade.sinefy Fade.spike))
  let amount = dna.float32("Reflect amount", lerp 2.0f 8.0f)
  let offset = Vec3f.fromSeed(dna.data("Reflect offset"), -0.125f, 0.125f)
  translate offset >> reflect wave amount


/// Generates a saturation map.
let genSaturate (dna : Dna) =
  saturate (dna.float32("Saturation amount", xerp 2.0f 20.0f))


/// Generates a wave packet map.
let genWavePacket (dna : Dna) =
  packet (dna.float32("Packet seed", lerp 1.0f 2.0f, interval = Interval.LeftClosed))


/// Generates a shaping function (or nothing).
let genShape (dna : Dna) =
  dna.branch("Shape",
    C(6.0, "none", always identity),
    C(1.0, "scatter", genScatter),
    C(1.0, "bleed", genBleed),
    C(1.0, "posterize", genPosterize),
    C(0.5, "vector reflect", genVectorReflect),
    C(0.5, "reflect", genReflect),
    C(1.0, "saturate", genSaturate),
    C(1.0, "wave packet", genWavePacket)
    )


/// Generates a unary operation.
let genUnary (subGen : Dna -> Map3) (dna : Dna) =

  let unaryShape (shapeGen : Dna -> Map3) (dna : Dna) =
    let shape = shapeGen dna
    subGen dna >> shape

  dna.branch("Unary op",
    C(1.0, "scatter", unaryShape genScatter),
    C(1.0, "bleed", unaryShape genBleed),
    C(1.0, "posterize", unaryShape genPosterize),
    C(0.5, "vector reflect", unaryShape genVectorReflect),
    C(0.5, "reflect", unaryShape genReflect),
    C(1.0, "saturate", unaryShape genSaturate),
    C(1.0, "wave packet", unaryShape genWavePacket),
    C(1.0, "curl", Map3Info.normalizeWithId 0xc081 (subGen >> curl))
    )


/// Generates a displacement response.
let genDisplacement minAmount maxAmount (dna : Dna) =
  dna.branch("Displace response",
    C("shaped", fun _ ->
      let fade = genDisplaceFade dna
      let amount = dna.float32("Displace amount", squared >> lerp minAmount maxAmount) / Fade.area fade
      Walk.shape3f fade >> scale amount
      ),
    C("scaled", fun _ ->
      let amount = dna.float32("Displace amount", squared >> lerp minAmount maxAmount)
      scale amount
      ),
    C("power", fun _ ->
      let power = dna.float32("Displace power", xerp 0.4f 2.5f)
      let amount = dna.float32("Displace amount", squared >> lerp minAmount maxAmount) * power
      shape3 (fun x -> amount * apow x power)
      )
    )


/// Generates a component softmix shaper.
let genSoftmix (dna : Dna) =
  softmix (dna.float32("Mix", lerp -6.0f 6.0f))


/// Generates a vector softmix shaper.
let genSoftmix3 (dna : Dna) =
  softmix3 (dna.float32("Mix", lerp -6.0f 6.0f))


/// Generates an atlas.
let genAtlas (genPattern : Dna -> Map3) (dna : Dna) =
  dna.branch("Pattern",
    C(1.0, "Solid color", fun _ ->
      colorAtlas (genCellColor dna)
      ),
    C(1.0, "Offset pattern", fun _ ->
      let scale = dna.float32("Locale scale", lerp 0.2f 5.0f)
      offsetAtlas (genPattern dna) scale
      )
    )


/// Generates a basis function.
let rec genBasis maxDepth (dna : Dna) =
  let dualWeight = if maxDepth > 1 then 1.0 else 0.0
  let maxDepth' = maxDepth - 1

  let genFactor() = dna.float32("Frequency factor", squared >> xerp 0.25f 4.0f)

  let genShapedBasis (dna : Dna) =
    let basis = genBasis maxDepth' dna
    shapeBasis (genShape dna) basis

  let genPattern (dna : Dna) =
    let frequency = dna.float32("Pattern frequency", xerp 1.0f 4.0f, interval = LeftClosed)
    let basis = genBasis maxDepth' dna
    let shape = genShape dna
    basis frequency >> shape

  dna.branch("Basis",
    C(1.0, "Perlin noise", fun _ ->
      let fade = dna.category("Perlin fade", C("cubic", Fade.cubic), C("smooth", Fade.smooth), C("super smooth", Fade.super))
      perlin (genLayout dna |> layoutFunction) fade
      ),
    C(1.0, "cubex noise", fun _ ->
      let color = genCellColor dna
      let fade = dna.category("Cubex fade", C("sine", Fade.sine), C("smooth", Fade.smooth), C("upward arc", Fade.uparc))
      let mix = genBasisMixOp dna
      cubex (genLayout dna |> layoutFunction) mix color fade
      ),
    C(0.5, "weave", fun _ ->
      let period = dna.int("Weave period", 2, 4)
      let fade = dna.category("Weave", C("threaded", Fade.wave period), 
                                       C("quilted", Fade.wave period >> Fade.smooth),
                                       C("wired", Fade.wave period >> Fade.shelf),
                                       C("tiled", Fade.clone period Fade.spike))
      // The wave fade increases the range so scale down a little.
      shapeBasis (scale 0.4f) (perlin (genFixedLayout dna |> layoutFunction) fade)
      ),
    C(1.0, "radial value noise", fun _ ->
      let color = genCellColor dna
      let fade = dna.category("Radial fade", C(0.4, "cubic", Fade.cubic), C(0.3, "smooth", Fade.smooth), C(0.2, "super smooth", Fade.super), C(0.2, "spike", Fade.spike))
      radial (genLayout dna |> layoutFunction) color fade
      ),
    C(1.0, "leopard", fun _ ->
      let count = genFeatureCount dna
      let mix = genBasisMixOp dna
      let color = genCellColor dna
      let fade = genPotentialFade dna
      leopard (genLayout dna |> layoutFunction) count mix color fade (dna.float32("Leopard shading")) (dna.float32("Leopard radius", xerp 0.25f 1.0f))
      ),
    C(1.0, "Worley", fun _ ->
      let count = genFeatureCount dna
      let distance = genCellDistance dna
      let P = worleyPattern.size
      let p = dna.data("Worley pattern", cubed P)
      let fade = genWorleyFade dna
      worley (genLayout dna |> layoutFunction) count (p % P) (p / P % P) (p / P / P % P) distance fade),
    C(1.0, "camo", fun _ ->
      let count = genFeatureCount dna
      let distance = genCellDistance dna
      let P = camoPattern.size
      let p = dna.data("Camo pattern", cubed P)
      let fade = genWorleyFade dna
      let line = dna.float32("Cell fade distance", xerp 0.025f 0.25f)
      let color = genCellColor dna
      camo (genLayout dna |> layoutFunction) count (p % P) (p / P % P) (p / P / P % P) distance color fade line
      ),
    C(1.0, "peacock", fun _ ->
      let count = genFeatureCount dna
      let potential, radius = genPotentialAndRadius dna
      let mix = genBasisMixOp dna
      let color = genCellColor dna
      let fade = genPotentialFade dna
      peacock (genLayout dna |> layoutFunction) count potential mix color fade (dna.float32("Peacock shading", squared)) radius
      ),
    C(1.0, "Julia", fun _ ->
      let iterations = dna.int("Iterations", 2, 10)
      let formula = dna.category("Formula", C(Julia), C(AexionJulia), C(Makin), C(White), C(Mandelcorn))
      let count = genFeatureCount dna
      let radius = dna.float32("Feature radius", lerp 0.5f 1.25f)
      let roughness = dna.float32("Roughness", xerp 0.5f 2.0f)
      let fade = genJuliaFade dna
      let lerpc = lerp -1.0f 1.0f
      let lerpq = lerp -1.0f 1.0f
      let cx = dna.float32("C-origin X", lerpc)
      let cy = dna.float32("C-origin Y", lerpc)
      let cz = if formula.is2D then 0.0f else dna.float32("C-origin Z", lerpc)
      let qx = dna.float32("Q-origin X", lerpq)
      let qy = dna.float32("Q-origin Y", lerpq)
      let qz = if formula.is2D then 0.0f else dna.float32("Q-origin Z", lerpq)
      let cRange = dna.float32("C-range", squared >> lerp 0.5f 2.0f)
      let qRange = dna.float32("Q-range", squared >> lerp 0.5f 2.0f)
      let cScale = dna.float32("C-scale", lerp 0.0f 2.0f)
      let qScale = dna.float32("Q-scale", lerp 0.0f 2.0f)
      julia (genLayout dna |> layoutFunction) count formula iterations roughness radius fade (Vec3f(cx, cy, cz)) (Vec3f(qx, qy, qz)) cRange qRange cScale qScale
      ),
    C(dualWeight, "Julia orbit trap", fun _ ->
      let iterations = dna.int("Iterations", 2, 8)
      let formula = dna.category("Formula", C(Julia), C(AexionJulia), C(Makin), C(White), C(Mandelcorn))
      let count = genFeatureCount dna
      let radius = dna.float32("Feature radius", sqrt >> lerp 0.5f 1.25f)
      let lerpc = lerp -1.0f 1.0f
      let lerpq = lerp -1.0f 1.0f
      let cx = dna.float32("C-origin X", lerpc)
      let cy = dna.float32("C-origin Y", lerpc)
      let cz = if formula.is2D then 0.0f else dna.float32("C-origin Z", lerpc)
      let qx = dna.float32("Q-origin X", lerpq)
      let qy = dna.float32("Q-origin Y", lerpq)
      let qz = if formula.is2D then 0.0f else dna.float32("Q-origin Z", lerpq)
      let cRange = dna.float32("C-range", squared >> lerp 0.5f 2.0f)
      let qRange = dna.float32("Q-range", squared >> lerp 0.5f 2.0f)
      let ffade = genJuliaFade dna
      let cScale = dna.float32("C-scale", lerp 0.0f 2.0f)
      let qScale = dna.float32("Q-scale", lerp 0.0f 2.0f)
      let tx = dna.float32("Trap X", lerpc)
      let ty = dna.float32("Trap Y", lerpc)
      let tz = if formula.is2D then 0.0f else dna.float32("Trap Z", lerpc)
      let tr = dna.float32("Trap radius", lerp 1.0f 2.0f)
      let tfade = genTrapFade dna
      let mix = genBasisMixOp dna
      let atlas = genAtlas genPattern dna
      orbit (genLayout dna |> layoutFunction) count formula iterations (Vec3f(cx, cy, cz)) (Vec3f(qx, qy, qz)) cRange qRange cScale qScale ffade (Vec3f(tx, ty, tz)) tr tfade atlas mix radius
      ),
    C(dualWeight, "geopard", fun _ ->
      let count = genFeatureCount dna
      let mix = genBasisMixOp dna
      let curvature = dna.float32("Curvature")
      geopard (genLayout dna |> layoutFunction) count mix (genPotentialFade dna) (dna.float32("Geopard radius", xerp 0.25f 1.0f)) curvature (genAtlas genPattern dna)
      ),
    C(dualWeight, "displace", fun _ ->
      let factor = genFactor()
      let basis1 = dna.descend("Basis 1", Map3Info.normalizeBasis genShapedBasis)
      let basis2 = dna.descend("Basis 2", genBasis maxDepth')
      displaceBasis (genDisplacement 0.2f 1.5f dna) factor basis1 basis2
      ),
    C(dualWeight, "layer", fun _ ->
      let factor = genFactor()
      let width = dna.float32("Layer width", lerp 2.0f 5.0f)
      let fade = genLayerFade dna
      let basis1 = dna.descend("Basis 1", genBasis maxDepth')
      let basis2 = dna.descend("Basis 2", genBasis maxDepth')
      binaryBasis (layer width fade) factor basis1 basis2
      ),
    C(dualWeight, "softmix", fun _ ->
      let factor = genFactor()
      let mix = genSoftmix dna
      let basis1 = dna.descend("Basis 1", genBasis maxDepth')
      let basis2 = dna.descend("Basis 2", genBasis maxDepth')
      binaryBasis mix factor basis1 basis2
      ),
    C(dualWeight, "norm softmix", fun _ ->
      let factor = genFactor()
      let mix = genSoftmix3 dna
      let basis1 = dna.descend("Basis 1", genBasis maxDepth')
      let basis2 = dna.descend("Basis 2", genBasis maxDepth')
      binaryBasis mix factor basis1 basis2
      ),
    C(dualWeight, "layered rotate", fun _ ->
      let factor = genFactor()
      let rotateAmount = dna.float32("Rotate amount", lerp 2.0f 6.0f)
      let rotateWidth = dna.float32("Rotate width", lerp 1.0f 3.0f)
      let fade = genLayerFade dna
      let basis1 = dna.descend("Basis 1", genShapedBasis)
      let basis2 = dna.descend("Basis 2", genBasis maxDepth')
      binaryBasis (rotatef rotateWidth rotateAmount fade) factor basis1 basis2
      ),
    C(dualWeight, "displace and rotate", fun _ ->
      let factor = genFactor()
      let rotation = dna.float32("Rotate amount", lerp 2.0f 6.0f)
      let displace = genDisplacement 0.2f 1.5f dna
      let basis1 = dna.descend("Basis 1", Map3Info.normalizeBasis genShapedBasis)
      let basis2 = dna.descend("Basis 2", genBasis maxDepth')
      binaryBasisd (rotate rotation) displace factor basis1 basis2
      ),
    C(dualWeight, "displace and softmix", fun _ ->
      let factor = genFactor()
      let mix = genSoftmix3 dna
      let displace = genDisplacement 0.2f 1.5f dna
      let basis1 = dna.descend("Basis 1", Map3Info.normalizeBasis genShapedBasis)
      let basis2 = dna.descend("Basis 2", genBasis maxDepth')
      binaryBasisd mix displace factor basis1 basis2
      ),
    C(dualWeight, "shape", fun _ ->
      let factor = genFactor()
      let saturation = dna.float32("Saturation")
      let monoization = dna.float32("Monoization")
      let scattering = dna.float32("Scattering")
      let basis1 = dna.descend("Basis 1", genShapedBasis)
      let basis2 = dna.descend("Basis 2", genBasis maxDepth')
      binaryBasis variableShape factor (shapeBasis (Vec3f(saturation, monoization, scattering) |> scale) basis1) basis2
      ),
    C(dualWeight, "capsule flow", fun _ ->
      let flowFrequency = dna.float32("Flow frequency", squared >> lerp 0.02f 2.0f, unit = "x")
      let radius = dna.float32("Radius", lerp 0.1f 1.0f)
      let length = dna.float32("Length", squared >> lerp 0.5f 4.0f)
      let fade = genPotentialFade dna
      let shading = dna.float32("Shading", squared)
      let cellColor = genCellColor dna
      let mixOp = genBasisMixOp dna
      let featureCount = genFeatureCount dna
      let basis1 = dna.descend("Flow basis", Map3Info.normalizeBasis genShapedBasis)
      capflow (layoutFunction <| genLayout dna) featureCount mixOp cellColor fade shading length radius basis1 flowFrequency
      ),
    C(dualWeight, "implicit flow", fun _ ->
      let flowFrequency = dna.float32("Flow frequency", squared >> lerp 0.02f 2.0f, unit = "x")
      let potential, radius = genPotentialAndRadius dna
      let fade = genPotentialFade dna
      let shading = dna.float32("Shading", squared)
      let cellColor = genCellColor dna
      let mixOp = genBasisMixOp dna
      let featureCount = genFeatureCount dna
      let basis1 = dna.descend("Flow basis", Map3Info.normalizeBasis genShapedBasis)
      impflow (layoutFunction <| genLayout dna) featureCount potential mixOp cellColor fade shading radius basis1 flowFrequency
      ),
    C(dualWeight, "pattern flow", fun _ ->
      let flowFrequency = dna.float32("Flow frequency", squared >> lerp 0.02f 2.0f, unit = "x")
      let potential, radius = genPotentialAndRadius dna
      let fade = genPotentialFade dna
      let mixOp = genBasisMixOp dna
      let featureCount = genFeatureCount dna
      let curvature = dna.float32("Curvature")
      let pattern = genAtlas genPattern dna
      let basis1 = dna.descend("Flow basis", Map3Info.normalizeBasis genShapedBasis)
      patflow (layoutFunction <| genLayout dna) featureCount potential mixOp fade radius curvature pattern basis1 flowFrequency
      )
    )


/// Generates a shaped basis function.
let genShapedBasis maxDepth (dna : Dna) =
  let basis = genBasis maxDepth dna
  shapeBasis (genShape dna) basis


/// Generates a walk operator.
let genWalk minAmount maxAmount (dna : Dna) =
  let amount = dna.float32("Displace amount", xerp minAmount maxAmount)
  let fade = genDisplaceFade dna
  let amount = amount / Fade.area fade
  dna.category("Walk operator",
    C("march", Walk.march amount fade),
    C("shuffle", Walk.shuffle amount fade),
    C("lurch", Walk.lurch amount fade),
    C("bounce", Walk.bounce amount fade),
    C("restep", Walk.restep amount fade)
    )


/// Generates a palette for a generator. The generator is normalized here as well.
let genColor (generator : Dna -> Map3) (dna : Dna) =
  let permutation = dna.data("Color permutation", 48)
  let palette = Color.genPalette 32 dna
  let map = normalize (generator dna >> permute permutation)
  fun (v : Vec3f) -> palette (map v)


/// Generates a fractal map.
let genFractal (subGen : Dna -> Map3) (dna : Dna) =
  let offset = genOffset dna
  let octaves = dna.int("Octaves", 2, 10)
  let basis = genBasis 2 dna
  let basef = dna.float32("Frequency", xerp 2.0f 16.0f)
  let roughness = dna.float32("Roughness", xerp 0.4f 0.9f)
  let minLacunarity = 0.5f / G sqrt2
  let maxLacunarity = 0.5f * G sqrt2
  let fractalizer =
    dna.branch("Fractalizer",
      C(1.0, "mix and displace", fun (dna : Dna) ->
        let lacunarity = dna.float32("Lacunarity", xerp minLacunarity maxLacunarity)
        let highpass = dna.float32("Highpass filter", lerp 0.0f 0.99f)
        fractald roughness lacunarity highpass (genMixOp dna) (genWalk 0.1f 2.0f dna) basef octaves basis
        ),
      C(1.0, "inverted mix and displace", fun (dna : Dna) ->
        let lacunarity = dna.float32("Lacunarity", xerp minLacunarity maxLacunarity)
        let lowpass = dna.float32("Lowpass filter", lerp 0.0f 0.99f)
        fractaldi roughness lacunarity lowpass (genMixOp dna) (genWalk 0.05f 0.5f dna) basef octaves basis
        ),
      C(1.0, "walk", fun _ ->
        let displace = dna.float32("Displace amount", xerp 1.0f 4.0f)
        let twist = dna.float32("Twist amount", lerp 0.0f 3.0f)
        walk roughness (displace / basef) twist octaves (basis basef)
        ),
      C(1.0, "variable lowpass and displace", fun (dna : Dna) ->
        let lacunarity = dna.float32("Lacunarity", xerp minLacunarity maxLacunarity)
        let displace = dna.float32("Displace amount", squared >> lerp 0.0f 2.0f)
        let displaceV = dna.float32("Displace variability", xerp 1.0f 8.0f)
        let twist = dna.float32("Twist amount", squared >> lerp 0.0f 5.0f)
        let twistV = dna.float32("Twist variability", xerp 1.0f 8.0f)
        let mix = genMixOp dna
        let multiMap = dna.descend("Variable map", Map3Info.normalize subGen)
        multid (roughness + 0.05f) lacunarity (displace / displaceV) displace (twist / twistV) twist basef 1.0f (float32 octaves + 0.999f) mix multiMap basis
        )
      )
  offset >> fractalizer >> genShape dna


/// Generates a binary operator.
let genBinop (subGen1 : Dna -> Map3) (subGen2 : Dna -> Map3) (dna : Dna) =
  let offset = genOffset dna
  let binop =
    dna.branch("Operator",
      C(1.0, "displace and rotate", fun _ ->
        bimapd (genDisplacement 0.01f 0.3f dna) <| rotatef (dna.float32("Rotate width", lerp 1.0f 4.0f)) (dna.float32("Rotate amount", lerp 2.0f 6.0f)) (genLayerFade dna)
        ),
      C(1.0, "displace and softmix", fun _ ->
        bimapd (genDisplacement 0.01f 0.3f dna) <| genSoftmix3 dna
        ),
      C(2.0, "displace and layer", fun _ ->
        bimapd (genDisplacement 0.01f 0.3f dna) <| layer (dna.float32("Layer width", lerp 1.5f 5.0f)) (genLayerFade dna)
        ),
      C(1.0, "layered rotate", fun _ ->
        bimap <| rotatef (dna.float32("Rotate width", lerp 1.0f 4.0f)) (dna.float32("Rotate amount", lerp 2.0f 6.0f)) (genLayerFade dna)
        ),
      C(1.0, "rotate", fun _ ->
        bimap <| rotate (dna.float32("Rotate amount", lerp 2.0f 6.0f))
        ),
      C(1.0, "softmix", fun _ ->
        bimap <| genSoftmix dna
        ),
      C(1.0, "norm softmix", fun _ ->
        bimap <| genSoftmix3 dna
        ),
      C(2.0, "layer", fun _ ->
        bimap <| layer (dna.float32("Layer width", lerp 1.5f 5.0f)) (genLayerFade dna)
        )
      )
  let b0 = dna.descend("Map 1", subGen1)
  let b1 = dna.descend("Map 2", subGen2)
  binop b0 b1 >> genShape dna


/// Generates a node tree recursively. E is the "energy" left in this node.
/// It limits tree complexity probabilistically.
let rec genNode (E : float) (dna : Dna) =

  let nodeWeight complexity = exp2(E - complexity) |> clamp 1.0e-6 1.0

  dna.branch("Node",
    C(2.0 * nodeWeight 1.0, "Basis", fun _ ->
      genOffset dna >> genBasis 3 dna (dna.float32("Frequency", xerp 2.0f 64.0f)) >> genShape dna
      ),
    C(nodeWeight 5.0, "Fractal", fun _ ->
      genFractal (genNode (0.7 * E)) dna
      ),
    C(0.5 * nodeWeight 2.0, "Unary", fun _ ->
      genUnary (genNode (0.75 * E)) dna
      ),
    C(nodeWeight 5.0, "Binary", fun _ ->
      genBinop (genNode (0.5 * E)) (genNode (0.5 * E)) dna
      )
    )


/// Map generator for the Explorer GUI.
let generateExplorerMap (dna : Dna) =
  let layout = genLayout dna
  dna.addInjector(DnaInjector.create(fun dna i (choices : Choices<Layout>) -> if dna.[i].name = "Layout" then Someval(choices.numberOf((=) layout)) else Noneval))
  genNode 5.0 dna

