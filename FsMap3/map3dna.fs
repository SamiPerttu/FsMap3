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


let internal layoutChoices =
  Choices(
    C(1.0e9, "Non-Tiling", Layout.Hifi),
    C("Tiling", Layout.Tile),
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


/// Generates a layering fade function.
let genLayerFade (dna : Dna) = 
  dna.category("Layer response",
    C(0.30, "smooth", Fade.smooth),
    C(0.30, "rational", Fade.rational),
    C(0.20, "sigmoid 1/4", Fade.sigmoid 0.25f),
    C(0.15, "sigmoid 1/3", Fade.sigmoid 0.333f),
    C(0.10, "sigmoid 3/8", Fade.sigmoid 0.375f),
    C(0.10, "sigmoid 2/5", Fade.sigmoid 0.4f)
    )


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
    C("rounded cylinder", fun _ -> roundedCylinder (dna.float32("Cylinder radius", Fade.hump >> lerp 0.05f 1.0f))),
    C("rounded cone", fun _ -> roundedCone (dna.float32("Cone radius", Fade.hump >> lerp 0.05f 1.0f))),
    C(0.5, "torus", fun _ -> torus (dna.float32("Major axis", lerp 0.5f 0.95f))),
    C(2.0, "superellipsoid", fun _ -> superellipsoid (dna.float32("Radial power", xerp 0.5f 8.0f)) (dna.float32("Transverse power", xerp 0.5f 8.0f))),
    C(1.5, "supertorus", fun _ -> supertorus (dna.float32("Ring power", xerp 1.0f 8.0f)) (dna.float32("Major axis", lerp 0.5f 0.95f))),
    C("supercone", fun _ -> supercone (dna.float32("Radial power", xerp 1.0f 8.0f)) (dna.float32("Cone radius", Fade.hump >> lerp 0.05f 1.0f))),
    C("Roman", fun _ -> roman)
    )


/// Generates a potential function and feature radius. Constrains radius in an attempt
/// to get the potential to occupy at least 1% of the volume of the unit sphere.
let genPotentialAndRadius (dna : Dna) =
  let potential = genPotential dna
  let volume = sampleVolume 1000 potential
  let minVolume = 0.01f
  let volumeFactor = volume / minVolume
  let minRadius = clamp 0.1f 0.9f (1.0f / (volumeFactor ** (1.0f / 3.0f)))
  let radius = dna.float32("Potential radius", sqrt >> lerp minRadius 1.0f)
  (potential, radius)


/// Generates a feature count function.
let genFeatureCount (dna : Dna) =
  dna.branch("Features per cell",
    C(1.0, "one", fun _ -> unityCount),
    C(1.0, "flip", fun _ -> flipCount (dna.float("Average", xerp 0.2 2.0))),
    C(2.5, "Poisson", fun _ -> poissonCount (dna.float("Mean", xerp 0.4 3.0))),
    C(1.0, "geometric", fun _ -> geometricCount (dna.float("Mean", xerp 0.4 3.0)))
    )


/// Generates a mixing operator for octave or map mixing.
let genMixOp (dna : Dna) =
  dna.branch("Mix operator",
    C(2.0, "sum", fun _ -> Mix.sum),
    C(1.5, "over", fun _ -> Mix.over),
    C(1.5, "norm", fun _ -> Mix.norm (dna.float32("Hardness"))),
    C(2.0, "soft", fun _ -> Mix.soft (dna.float32("Bias", lerp -5.0f 5.0f))),
    C(1.0, "layer", fun _ -> Mix.layer (dna.float32("Layer width", lerp 0.1f 1.0f)) (genLayerFade dna) (dna.float32("Layer persist", lerp 0.1f 0.9f)))
    )


/// Generates a mixing operator for intra-basis (feature) mixing.
let genBasisMixOp (dna : Dna) =
  dna.branch("Mix operator",
    C(2.0, "sum", fun _ -> Mix.sum),
    C(1.5, "over", fun _ -> Mix.over),
    C(0.75, "min", fun _ -> Mix.min),
    C(0.75, "max", fun _ -> Mix.max),
    C(1.5, "norm", fun _ -> Mix.norm (dna.float32("Hardness"))),
    C(1.5, "soft", fun _ -> Mix.soft (dna.float32("Bias", lerp -5.0f 5.0f)))
    )


/// Generates a fade for potential functions.
let genPotentialFade (dna : Dna) =
  dna.category("Potential fade",
    C("downarc", Fade.downarc),
    C("hump", Fade.hump),
    C("linear", Fade.linear),
    C("sine", Fade.sine),
    C("smooth", Fade.smooth),
    C("saturated smooth", Fade.skew -1.0f >> Fade.smooth),
    C("squared", Fade.power2)
    )


/// Generates a fade for displacements.
let genDisplaceFade (dna : Dna) =
  dna.category("Displace response",
    C("linear", Fade.linear),
    C("2nd power", Fade.power2),
    C("3rd power", Fade.power3),
    C("4th power", Fade.power4),
    C("sine", Fade.sine),
    C("smooth", Fade.smooth),
    C("hump", Fade.hump),
    C("shelf", Fade.shelf)
    )


/// Generates a cell distance function.
let genCellDistance (dna : Dna) =
  dna.branch("Cell distance function",
    C(1.0, "1-norm", fun _ -> cellNorm1),
    C(0.5, "low norm", fun _ -> cellNorm (dna.float32("Power", lerp 1.1f 1.9f))),
    C(2.5, "2-norm", fun _ -> cellNorm2),
    C(0.5, "3-norm", fun _ -> cellNorm3),
    C(1.0, "4-norm", fun _ -> cellNorm4),
    C(0.5, "6-norm", fun _ -> cellNorm6),
    C(0.5, "8-norm", fun _ -> cellNorm8),
    C(0.5, "max-norm", fun _ -> cellNormMax)
    )


/// Generates a fade function for the Worley cellular basis.
let genWorleyFade (dna : Dna) =
  dna.category("Worley fade",
    C("linear", Fade.linear),
    C("squared", Fade.power2),
    C("uparc", Fade.uparc),
    C("sine", Fade.sine),
    C(0.75, "smooth", Fade.smooth),
    C(0.75, "super", Fade.super),
    C("downarc", Fade.downarc),
    C("hump", Fade.hump)
    )


/// Generates a shaping function (or nothing).
let genShape (dna : Dna) =
  dna.branch("Shape",
    C(6.0, "none", fun _ -> identity),
    C(1.0, "scatter", fun _ ->
      let dx = dna.float32("X offset", lerp -0.5f 0.5f)
      let dy = dna.float32("Y offset", lerp -0.5f 0.5f)
      let dz = dna.float32("Z offset", lerp -0.5f 0.5f)
      let wave = dna.category("Scatter wave", C("triangle", triangle), C("sine", sin))
      scatter wave (Vec3f(dx, dy, dz))
      ),
    C(1.0, "bleed", fun _ ->
      let b1 = dna.float32("Bleed 1", lerp -1.0f 1.0f)
      let b2 = dna.float32("Bleed 2", lerp -1.0f 1.0f)
      fun (v : Vec3f) -> Vec3f(v.x + b1 * v.z + b2 * v.y, v.y + b1 * v.x + b2 * v.z, v.z + b1 * v.y + b2 * v.x)
      ),
    C(1.0, "posterize", fun _ ->
      let levels = dna.float32("Posterize levels", xerp 1.0f 20.0f)
      crush3 (dna.category("Posterize fade", C("smooth", Fade.smooth), C("rational", Fade.rational), C("sigmoid 1/4", Fade.sigmoid 0.25f), C("sigmoid 2/5", Fade.sigmoid 0.4f), C(0.5, "sigmoid 1/2", Fade.sigmoid 0.5f))) levels
      ),
    C(0.5, "vector reflect", fun _ ->
      let fade, factor = dna.category("Reflect fade", C(1.5, "linear", (Fade.linear, 1.0f)), C("smooth", (Fade.smooth, 1.0f)), C("super", (Fade.super, 1.0f)), C("worm", (Fade.worm 2, 0.6f)), C(0.5, "spike", (Fade.spike, 0.8f)), C(0.5, "hump", (Fade.hump, 1.0f)))
      let amount = dna.float32("Reflect amount", lerp 2G 8G) * factor
      reflectNormf fade amount
      ),
    C(0.5, "reflect", fun _ ->
      let wave = dna.category("Reflect wave", C("sine", sin), C(1.5, "triangle", triangle), C("spike", Fade.sinefy Fade.spike), C("smooth", Fade.sinefy Fade.smooth))
      let amount = dna.float32("Reflect amount", lerp 2.0f 8.0f)
      let offset = Vec3f.fromSeed(dna.data("Reflect offset"), -0.125f, 0.125f)
      translate offset >> reflect wave amount
      ),
    C(1.0, "saturate", fun _ ->
      saturate (dna.float32("Saturate amount", xerp 2.0f 20.0f))
      ),
    C(1.0, "wave packet", fun _ ->
      packet (dna.float32("Wave frequency", lerp 1.0f 2.0f, interval = Interval.LeftClosed))
      )
    )


/// Generates a displacement response.
let genDisplacement minAmount maxAmount (dna : Dna) =
  dna.branch("Displace response",
    C("shaped", fun _ ->
      let fade = genDisplaceFade dna
      Walk.shape3f fade >> scale (dna.float32("Displace amount", squared >> lerp minAmount maxAmount) / Fade.area fade)),
    C("scaled", fun _ -> scale (dna.float32("Displace amount", squared >> lerp minAmount maxAmount))),
    C("power", fun _ ->
      let power = dna.float32("Displace power", xerp 0.4f 2.5f)
      let amount = dna.float32("Displace amount", squared >> lerp minAmount maxAmount) * power
      shape3 (fun x -> amount * apow x power))
      )


/// Generates a component softmix shaper.
let genSoftmix (dna : Dna) =
  softmix (dna.float32("Mix", lerp -6.0f 6.0f))


/// Generates a vector softmix shaper.
let genSoftmix3 (dna : Dna) =
  softmix3 (dna.float32("Mix", lerp -6.0f 6.0f))


let genAtlas (genPattern : Dna -> Map3) (dna : Dna) : Atlas3 =
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
    C(1.0, "Perlin", fun _ ->
      let fade = dna.category("Perlin fade", C("smooth", Fade.smooth), C("cubic", Fade.cubic), C("super", Fade.super))
      perlin (genLayout dna |> layoutFunction) fade
      ),
    C(1.0, "cubex", fun _ ->
      let color = genCellColor dna
      let fade = dna.category("Cubex fade", C("sine", Fade.sine), C("smooth", Fade.smooth), C("uparc", Fade.uparc))
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
    C(1.0, "radial", fun _ ->
      let color = genCellColor dna
      let fade = dna.category("Radial fade", C(0.4, "cubic", Fade.cubic), C(0.3, "smooth", Fade.smooth), C(0.2, "super", Fade.super), C(0.2, "spike", Fade.spike))
      radial (genLayout dna |> layoutFunction) color fade
      ),
    C(1.0, "leopard", fun _ ->
      let count = genFeatureCount dna
      let mix = genBasisMixOp dna
      let color = genCellColor dna
      leopard (genLayout dna |> layoutFunction) count mix color (genPotentialFade dna) (dna.float32("Leopard shading")) (dna.float32("Leopard radius", xerp 0.25f 1.0f))
      ),
    C(1.0, "Worley", fun _ ->
      let count = genFeatureCount dna
      let distance = genCellDistance dna
      let P = worleyPattern.size
      let p = dna.data("Worley pattern", cubed P)
      let fade = genWorleyFade dna
      worley (genLayout dna |> layoutFunction) count (p % P) ((p / P) % P) ((p / P / P) % P) distance fade),
    C(1.0, "camo", fun _ ->
      let count = genFeatureCount dna
      let distance = genCellDistance dna
      let P = camoPattern.size
      let p = dna.data("Camo pattern", cubed P)
      let fade = genWorleyFade dna
      let line = dna.float32("Cell fade distance", xerp 0.025f 0.25f)
      let color = genCellColor dna
      camo (genLayout dna |> layoutFunction) count (p % P) ((p / P) % P) ((p / P / P) % P) distance color fade line
      ),
    C(1.0, "peacock", fun _ ->
      let count = genFeatureCount dna
      let potential, radius = genPotentialAndRadius dna
      let mix = genBasisMixOp dna
      let color = genCellColor dna
      peacock (genLayout dna |> layoutFunction) count potential mix color (genPotentialFade dna) (dna.float32("Peacock shading", squared)) radius
      ),
    C(1.0, "Julia", fun _ ->
      let iterations = dna.int("Iterations", 2, 8)
      let formula = dna.category("Formula", C(Julia), C(AexionJulia), C(Makin), C(White), C(Mandelcorn))
      let count = genFeatureCount dna
      let radius = dna.float32("Feature radius", lerp 0.5f 1.25f)
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
      let fade = dna.category("Feature fade", C("saturated sine", Fade.skew -1.0f >> Fade.sine),
                                              C("sine", Fade.sine),
                                              C("smooth", Fade.smooth),
                                              C("squared", Fade.power2),
                                              C("uparc", Fade.uparc))
      julia (genLayout dna |> layoutFunction) count formula iterations (Vec3f(cx, cy, cz)) (Vec3f(qx, qy, qz)) cRange qRange cScale qScale fade radius
      ),
    C(dualWeight, "orbit", fun _ ->
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
      let ffade = dna.category("Feature fade", C("sine", Fade.sine), C("smooth", Fade.smooth), C("squared", Fade.power2), C("uparc", Fade.uparc))
      let cScale = dna.float32("C-scale", lerp 0.0f 2.0f)
      let qScale = dna.float32("Q-scale", lerp 0.0f 2.0f)
      let tx = dna.float32("Trap X", lerpc)
      let ty = dna.float32("Trap Y", lerpc)
      let tz = if formula.is2D then 0.0f else dna.float32("Trap Z", lerpc)
      let tr = dna.float32("Trap radius", lerp 1.0f 2.0f)
      let tfade = dna.category("Trap fade", C("downarc", Fade.downarc),
                                            C("hump", Fade.hump),
                                            C("linear", Fade.linear),
                                            C("sine", Fade.sine),
                                            C("smooth", Fade.smooth),
                                            C("saturated smooth", Fade.saturate 0.3f >> Fade.smooth),
                                            C("squared", Fade.power2),
                                            C("uparc", Fade.uparc))
      let mix = genBasisMixOp dna
      orbit (genLayout dna |> layoutFunction) count formula iterations (Vec3f(cx, cy, cz)) (Vec3f(qx, qy, qz)) cRange qRange cScale qScale ffade (Vec3f(tx, ty, tz)) tr tfade (genAtlas genPattern dna) mix radius
      ),
    C(dualWeight, "geopard", fun _ ->
      let count = genFeatureCount dna
      let mix = genBasisMixOp dna
      let curvature = dna.float32("Curvature")
      geopard (genLayout dna |> layoutFunction) count mix (genPotentialFade dna) (dna.float32("Geopard radius", xerp 0.25f 1.0f)) curvature (genAtlas genPattern dna)
      ),
    C(dualWeight, "displace", fun _ ->
      let factor = genFactor()
      let basis1 = dna.descend("Basis 1", genShapedBasis) |> normalizeBasis
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
      let basis1 = dna.descend("Basis 1", genShapedBasis) |> normalizeBasis
      let basis2 = dna.descend("Basis 2", genBasis maxDepth')
      binaryBasisd (rotate rotation) displace factor basis1 basis2
      ),
    C(dualWeight, "displace and softmix", fun _ ->
      let factor = genFactor()
      let mix = genSoftmix3 dna
      let displace = genDisplacement 0.2f 1.5f dna
      let basis1 = dna.descend("Basis 1", genShapedBasis) |> normalizeBasis
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
      let length = dna.float32("Length", lerp 0.5f 2.0f)
      let fade = genPotentialFade dna
      let shading = dna.float32("Shading", squared)
      let cellColor = genCellColor dna
      let mixOp = genBasisMixOp dna
      let featureCount = genFeatureCount dna
      let basis1 = dna.descend("Flow basis", genShapedBasis) |> normalizeBasis
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
      let basis1 = dna.descend("Flow basis", genShapedBasis) |> normalizeBasis
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
      let basis1 = dna.descend("Flow basis", genShapedBasis) |> normalizeBasis
      patflow (layoutFunction <| genLayout dna) featureCount potential mixOp fade radius curvature pattern basis1 flowFrequency
      )
    )


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


/// Generates a palette for the generator. The generator is normalized here as well.
let genColor (generator : Dna -> Map3) (dna : Dna) =
  let permutation = dna.data("Color permutation", 48)
  let palette = Color.genPalette 32 dna
  let map = normalize (generator dna >> permute permutation)
  fun (v : Vec3f) -> palette (map v)


/// Generates a single octave of a basis.
let genOctave maxDepth (dna : Dna) =
  let offset = genOffset dna
  let basef = dna.float32("Frequency", xerp 2.0f 16.0f)
  let basis = genBasis maxDepth dna
  offset >> basis basef >> genShape dna


/// Generates a fractal map.
let genFractal (dna : Dna) =
  let offset = genOffset dna
  let octaves = dna.int("Octaves", 2, 8)
  let basis = genBasis 2 dna
  let basef = dna.float32("Frequency", xerp 2.0f 16.0f)
  let roughness = dna.float32("Roughness", xerp 0.4f 0.9f)
  let minLacunarity = 0.5f / G sqrt2
  let maxLacunarity = 0.5f * G sqrt2
  let fractalizer =
    dna.branch("Fractalizer",
      C(1.0, "fractald", fun (dna : Dna) ->
        let lacunarity = dna.float32("Lacunarity", xerp minLacunarity maxLacunarity)
        let highpass = dna.float32("Highpass filter", lerp 0.0f 0.99f)
        fractald roughness lacunarity highpass (genMixOp dna) (genWalk 0.1f 2.0f dna) basef octaves basis
        ),
      C(1.0, "fractaldi", fun (dna : Dna) ->
        let lacunarity = dna.float32("Lacunarity", xerp minLacunarity maxLacunarity)
        let lowpass = dna.float32("Lowpass filter", lerp 0.0f 0.99f)
        fractaldi roughness lacunarity lowpass (genMixOp dna) (genWalk 0.05f 0.5f dna) basef octaves basis
        ),
      C(1.0, "walk", fun _ ->
        let displace = dna.float32("Displace amount", xerp 1.0f 4.0f)
        let twist = dna.float32("Twist amount", lerp 0.0f 3.0f)
        walk roughness (displace / basef) twist octaves (basis basef)
        ),
      C(2.0, "swiss", fun (dna : Dna) ->
        let lacunarity = dna.float32("Lacunarity", xerp minLacunarity maxLacunarity)
        swiss roughness lacunarity (genWalk 0.1f 2.0f dna) (genLayerFade dna) (dna.float32("Overlay width", lerp 1.5f 5.0f)) (dna.float32("Overlay persist")) basef octaves (dna.int("1st Octave", octaves)) basis
        ),
      C(2.0, "multid", fun (dna : Dna) ->
        let lacunarity = dna.float32("Lacunarity", xerp minLacunarity maxLacunarity)
        let displace = dna.float32("Displace amount", xerp 0.5f 3.0f)
        let displaceV = dna.float32("Displace variability", xerp 1.5f 8.0f)
        let twist = dna.float32("Twist amount", xerp 0.2f 5.0f)
        let twistV = dna.float32("Twist variability", xerp 1.5f 8.0f)
        let multiMap = dna.descend("Multi map", fun dna -> normalize (genOffset dna >> genBasis 2 dna (dna.float32("Frequency", xerp 2.0f 12.0f)) >> genShape dna))
        multid (roughness + 0.05f) lacunarity (displace / displaceV) displace (twist / twistV) twist basef 1G (float32 octaves + 2.0f) multiMap basis
        )
      )
  offset >> fractalizer >> genShape dna


/// Generates a binary operator.
let genBinop (subgen1 : Dna -> Map3) (subgen2 : Dna -> Map3) (dna : Dna) =
  let offset = genOffset dna
  let op =
    dna.branch("Operator",
      C(1.0, "displace and rotate", fun _ -> bimapd (genDisplacement 0.01f 0.3f dna) <| rotatef (dna.float32("Rotate width", lerp 1.0f 4.0f)) (dna.float32("Rotate amount", lerp 2.0f 6.0f)) (genLayerFade dna)),
      C(1.0, "displace and softmix", fun _ -> bimapd (genDisplacement 0.01f 0.3f dna) <| genSoftmix3 dna),
      C(2.0, "displace and layer", fun _ -> bimapd (genDisplacement 0.01f 0.3f dna) <| layer (dna.float32("Layer width", lerp 1.5f 5.0f)) (genLayerFade dna)),
      C(1.0, "layered rotate", fun _ -> bimap <| rotatef (dna.float32("Rotate width", lerp 1.0f 4.0f)) (dna.float32("Rotate amount", lerp 2.0f 6.0f)) (genLayerFade dna)),
      C(1.0, "rotate", fun _ -> bimap <| rotate (dna.float32("Rotate amount", lerp 2.0f 6.0f))),
      C(1.0, "softmix", fun _ -> bimap <| genSoftmix dna),
      C(1.0, "norm softmix", fun _ -> bimap <| genSoftmix3 dna),
      C(2.0, "layer", fun _ -> bimap <| layer (dna.float32("Layer width", lerp 1.5f 5.0f)) (genLayerFade dna))
      )
  let b0 = dna.descend("Map 1", subgen1)
  let b1 = dna.descend("Map 2", subgen2)
  op b0 b1 >> genShape dna


/// A recursive Map3 generator.
let rec genNode3 (E : float) (dna : Dna) =
  let nodeWeight complexity = max 1.0e-6 (exp2(-abs(E - complexity)))
  dna.branch("Node",
    C(nodeWeight 1.0, "Basis", fun _ ->
      genOffset dna >> genBasis 2 dna (dna.float32("Frequency", xerp 2.0f 16.0f)) >> genShape dna
      ),
    C(nodeWeight 4.0, "Fractal", fun _ ->
      genFractal dna
      ),
    C(nodeWeight 5.0, "Binary", fun _ ->
      genBinop (genNode3 (0.5 * E)) (genNode3 (0.5 * E)) dna
      )
    )


