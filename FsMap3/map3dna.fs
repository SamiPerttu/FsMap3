/// Procedural generation of Map3 and related functions.
module FsMap3.Map3Dna

open Common
open Mangle
open Potential
open Basis3
open Map3
open Atlas3
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


/// Generates a smallish offset.
let genOffset (dna : Dna) =
  let lerpC = lerp -5.0f 5.0f
  let x = dna.float32("X offset", lerpC)
  let y = dna.float32("Y offset", lerpC)
  let z = dna.float32("Z offset", lerpC)
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
    C(1.5, "rounded cube", fun _ -> roundedCube),
    C("teardrop", fun _ -> teardrop),
    C(1.5, "rounded cylinder", fun _ ->
      roundedCylinder (dna.float32("Cylinder radius", Fade.reverse Fade.power3 >> lerp 0.05f 1.0f))
      ),
    C(1.5, "rounded cone", fun _ ->
      roundedCone (dna.float32("Cone radius", Fade.reverse Fade.power3 >> lerp 0.05f 1.0f))
      ),
    C(1.0, "torus", fun _ ->
      torus (dna.float32("Major axis", lerp 0.5f 0.95f))
      ),
    C(2.0, "superellipsoid", fun _ ->
      superellipsoid (dna.float32("Radial power", xerp 0.5f 8.0f)) (dna.float32("Transverse power", xerp 0.5f 8.0f))
      ),
    C(1.0, "supertorus", fun _ ->
      supertorus (dna.float32("Ring power", xerp 1.0f 8.0f)) (dna.float32("Major axis", lerp 0.5f 0.95f))
      ),
    C("supercone", fun _ ->
      supercone (dna.float32("Radial power", xerp 1.0f 8.0f)) (dna.float32("Cone radius", Fade.reverse Fade.power3 >> lerp 0.05f 1.0f))
      ),
    C("Roman", fun _ -> roman)
    )


/// Generates a potential function and feature radius. Constrains radius in an attempt
/// to get the potential to occupy at least 1% of the volume of the unit sphere.
let genPotentialAndRadius (dna : Dna) =
  let potential = genPotential dna
  let volume = sampleVolume 8 potential |> max 0.001f
  let minVolume = 0.01f
  let volumeFactor = volume / minVolume
  let minRadius = clamp 0.1f 0.9f (1.0f / cbrt volumeFactor)
  let radius = dna.float32("Potential radius", sqrt >> lerp minRadius 1.0f)
  (potential, radius)


/// Generates a feature count function.
let genFeatureCount (dna : Dna) =
  dna.branch("Features per cell",
    C(1.0, "one", fun _ -> unityCount),
    C(1.0, "rounded average", fun _ -> flipCount (dna.float("Average", xerp 0.2 4.0))),
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
    C(3.0, "layer", fun _ -> Mix.layer (dna.float32("Layer width", lerp 0.1f 1.0f)) (genLayerFade dna) (dna.float32("Layer persist", lerp 0.0f 1.0f)))
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
  dna.branch("Cell distance norm",
    C(0.5, "1-norm", fun _ -> cellNorm1),
    C(0.5, "low norm", fun _ -> cellNorm (dna.float32("Exponent", lerp 1.1f 1.9f))),
    C(2.5, "2-norm", fun _ -> cellNorm2),
    C(0.5, "3-norm", fun _ -> cellNorm3),
    C(1.0, "4-norm", fun _ -> cellNorm4),
    C(0.5, "6-norm", fun _ -> cellNorm6),
    C(0.5, "8-norm", fun _ -> cellNorm8),
    C(1.0, "max-norm", fun _ -> cellNormMax)
    )


/// Generates a component shifting map.
let genShift (dna : Dna) =
  let dx = dna.float32("X shift", lerp -0.5f 0.5f)
  let dy = dna.float32("Y shift", lerp -0.5f 0.5f)
  let dz = dna.float32("Z shift", lerp -0.5f 0.5f)
  let wave = dna.category("Shift wave", C("triangle", trir),
                                        C("smooth triangle", fun x -> Fade.cosifyr Fade.smoothLine (x - Q 1 4)),
                                        C("sine", sinrFast)
                                        )
  scatter wave (Vec3f(dx, dy, dz))


/// Generates a scattering map.
let genScatter (dna : Dna) =
  let seed = dna.data("Scatter seed")
  let wave = dna.category("Scatter wave", C("triangle", trir),
                                          C("smooth triangle", fun x -> Fade.cosifyr Fade.smoothLine (x - Q 1 4)),
                                          C("sine", sinrFast)
                                          )
  let seed' = mangle32 seed
  let t1 = dna.float32("Scatter finetuning 1")
  let t2 = dna.float32("Scatter finetuning 2")
  let t3 = dna.float32("Scatter finetuning 3")
  let phase = Vec3f.fromSeed(seed, 0.0f, 1.0f)
  let fx = Vec3f.fromSeed(mangle32c seed, -0.5f, 0.5f)
  let fy = Vec3f.fromSeed(mangle32c seed', -0.5f, 0.5f)
  let fz = Vec3f.fromSeed(mangle32d seed, -0.5f, 0.5f)
  let vx = Vec3f.fromSeed(seed', -0.5f, 0.5f) + t1 * fx
  let vy = Vec3f.fromSeed(mangle32b seed, -0.5f, 0.5f) + t2 * fy
  let vz = Vec3f.fromSeed(mangle32b seed', -0.5f, 0.5f) + t3 * fz
  fun (v : Vec3f) ->
    Vec3f(wave (vx *. v + phase.x), wave (vy *. v + phase.y), wave (vz *. v + phase.z))


/// Generates a bleed map.
let genBleed (dna : Dna) =
  let b1 = dna.float32("Bleed 1", lerp -1.0f 1.0f)
  let b2 = dna.float32("Bleed 2", lerp -1.0f 1.0f)
  fun (v : Vec3f) -> Vec3f(v.x + b1 * v.z + b2 * v.y, v.y + b1 * v.x + b2 * v.z, v.z + b1 * v.y + b2 * v.x)


/// Generates a vector posterizer map.
let genVectorPosterize (dna : Dna) =
  let levels = dna.float32("Posterize levels", xerp 1.0f 20.0f)
  crush3 (genSigmoidFade "Posterize hardness" dna) levels


/// Generates a component posterizer map.
let genComponentPosterize (dna : Dna) =
  let levels = dna.float32("Posterize levels", xerp 1.0f 20.0f)
  crush (genSigmoidFade "Posterize hardness" dna) levels


/// Generates a vector reflection map.
let genVectorReflect (dna : Dna) =
  let fade = dna.category("Reflect fade", C(0.5, "reverse power-2", Fade.reverse Fade.power2),
                                          C("line", Fade.line),
                                          C("smooth line", Fade.smoothLine),
                                          C("smooth-2", Fade.smooth2),
                                          C("smooth-3", Fade.smooth3),
                                          C("worm", Fade.worm 2),
                                          C(0.5, "power-2", Fade.power2)
                                          )
  let amount = dna.float32("Reflect amount", lerp 2.0f 8.0f)
  reflect3f fade amount


/// Generates a component reflection map.
let genComponentReflect (dna : Dna) =
  let wave = dna.category("Reflect wave", C(1.5, "line", trir),
                                          C("smooth line", fun x -> Fade.cosifyr Fade.smoothLine (x - Q 1 4)),
                                          C("sine", sinrFast),
                                          C("smooth-2", fun x -> Fade.cosifyr Fade.smooth2 (x - Q 1 4)),
                                          C("power-2", Fade.sinefyr Fade.power2)
                                          )
  let amount = dna.float32("Reflect amount", lerp 2.0f 8.0f)
  let offset = Vec3f.fromSeed(dna.data("Reflect offset"), -0.125f, 0.125f)
  translate offset >> reflect wave amount


/// Generates a component overdrive map.
let genOverdrive (dna : Dna) =
  overdrive (dna.float32("Overdrive amount", xerp 2.0f 10.0f))


/// Generates a vector overdrive map.
let genVectorOverdrive (dna : Dna) =
  let amount = dna.float32("Overdrive amount", xerp 2.0f 10.0f)
  overdrive3 amount


/// Generates a wave packet map.
let genWavePacket (dna : Dna) =
  let seed = dna.data("Packet seed")
  packet (1.0f + float32 (Convert.float01 Interval.LeftClosed seed))


/// Generates a shaping function (or nothing).
let genShape (dna : Dna) =
  dna.branch("Shape",
    C(8.0, "none", always identity),
    C(1.0, "shift", genShift),
    C(1.0, "bleed", genBleed),
    C(1.0, "scatter", genScatter),
    C(1.0, "wave packet", genWavePacket),
    C(0.8, "component overdrive", genOverdrive),
    C(0.8, "vector overdrive", genVectorOverdrive),
    C(0.5, "component posterize", genComponentPosterize),
    C(1.0, "vector posterize", genVectorPosterize),
    C(0.25, "component reflect", genComponentReflect),
    C(0.25, "vector reflect", genVectorReflect)
    )


/// Generates a unary operation.
let genUnary (subGen : Dna -> Map3) (dna : Dna) =

  let unaryShape (shapeGen : Dna -> Map3) (dna : Dna) =
    let shape = shapeGen dna
    subGen dna >> shape

  dna.branch("Unary op",
    C(1.0, "shift", unaryShape genShift),
    C(1.0, "bleed", unaryShape genBleed),
    C(1.0, "scatter", unaryShape genScatter),
    C(1.0, "wave packet", unaryShape genWavePacket),
    C(1.0e-3, "curl", Map3Info.normalizeWithId 0xc081 (subGen >> curl)),
    C(0.8, "component overdrive", unaryShape genOverdrive),
    C(0.8, "vector overdrive", unaryShape genVectorOverdrive),
    C(0.5, "component posterize", unaryShape genComponentPosterize),
    C(1.0, "vector posterize", unaryShape genVectorPosterize),
    C(0.25, "component reflect", unaryShape genComponentReflect),
    C(0.25, "vector reflect", unaryShape genVectorReflect)
    )


/// Generates a displacement response.
let genDisplacement minAmount maxAmount (dna : Dna) =
  dna.branch("Displace response",
    C("linear", fun _ ->
      let amount = dna.float32("Displace amount", squared >> lerp minAmount maxAmount)
      scale amount
      ),
    C("power", fun _ ->
      let power = dna.float32("Displace power", xerp 0.333f 3.0f)
      let amount = dna.float32("Displace amount", squared >> lerp minAmount maxAmount) * power
      shape3 (fun x -> amount * apow x power)
      ),
    C("shaped", fun _ ->
      let fade = genDisplaceFade dna
      let amount = dna.float32("Displace amount", squared >> lerp minAmount maxAmount) / Fade.area fade
      Walk.shape3f fade >> scale amount
      )
    )


/// Generates a component softmix shaper.
let genSoftmix (dna : Dna) =
  softmix (dna.float32("Mix bias", lerp -6.0f 6.0f))


/// Generates a vector softmix shaper.
let genSoftmix3 (dna : Dna) =
  softmix3 (dna.float32("Mix bias", lerp -6.0f 6.0f))


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
  let maxDepth'  = maxDepth - 1

  let genFactor() = dna.float32("Frequency factor", squared >> xerp 0.25f 4.0f)

  let genShapedBasis (dna : Dna) =
    let basis = genBasis maxDepth' dna
    shapeBasis (genShape dna) basis

  let genPattern (dna : Dna) =
    let frequency = dna.float32("Pattern frequency", xerp 1.0f 4.0f, interval = LeftClosed)
    let basis     = genBasis maxDepth' dna
    let shape     = genShape dna
    basis frequency >> shape

  dna.branch("Basis",
    C(1.0, "Perlin noise", fun _ ->
      let fade = dna.category("Perlin fade", C("smooth-1", Fade.smooth1), C("smooth-2", Fade.smooth2), C("smooth-3", Fade.smooth3))
      perlin (genLayout dna |> layoutFunction) fade
      ),
    C(1.0, "cubex noise", fun _ ->
      let color = genCellColor dna
      let fade  = dna.category("Cubex fade", C("sine", Fade.sine), C("smooth-2", Fade.smooth2), C("upward arc", Fade.uparc))
      let mix   = genBasisMixOp dna
      cubex (genLayout dna |> layoutFunction) mix color fade
      ),
    C(0.5, "weave", fun _ ->
      let period = dna.int("Weave period", 2, 4)
      // TODO. When the period changes, the fade changes as well: we need to update the visualization in DnaGui somehow.
      // This will happen in the future to other visualizations, too, so we need a general solution.
      let fade = dna.category("Weave", C("threaded", Fade.wave period), 
                                       C("quilted", Fade.wave period >> Fade.smooth2),
                                       C("wired", Fade.wave period >> Fade.shelf),
                                       C("tiled", Fade.clone period Fade.power4))
      // The wave fade increases the range so scale down a little.
      shapeBasis (scale 0.4f) (perlin (genFixedLayout dna |> layoutFunction) fade)
      ),
    C(1.0, "radial value noise", fun _ ->
      let color = genCellColor dna
      let fade  = dna.category("Radial fade", C(0.4, "smooth-1", Fade.smooth1), C(0.3, "smooth-2", Fade.smooth2), C(0.2, "smooth-3", Fade.smooth3), C(0.2, "power-2", Fade.power2))
      radial (genLayout dna |> layoutFunction) color fade
      ),
    C(1.0, "leopard", fun _ ->
      let radius  = dna.float32("Leopard radius", xerp 0.25f 1.0f)
      let count   = genFeatureCount dna
      let fade    = genPotentialFade dna
      let mix     = genBasisMixOp dna
      let shading = dna.float32("Leopard shading")
      let color   = genCellColor dna
      leopard (genLayout dna |> layoutFunction) count mix color fade shading radius
      ),
    C(1.0, "Worley", fun _ ->
      let count    = genFeatureCount dna
      let distance = genCellDistance dna
      let P        = worleyPattern.size
      let p        = dna.data("Worley pattern", cubed P)
      let fade     = genWorleyFade dna
      worley (genLayout dna |> layoutFunction) count (p % P) (p / P % P) (p / P / P % P) distance fade),
    C(1.0, "colored Worley", fun _ ->
      let count    = genFeatureCount dna
      let distance = genCellDistance dna
      let P        = worleyPattern.size
      let p        = dna.data("Worley pattern", cubed P)
      let fade     = genWorleyFade dna
      let line     = dna.float32("Cell fade distance", xerp 0.025f 0.25f)
      let color    = genCellColor dna
      camo (genLayout dna |> layoutFunction) count (p % P) (p / P % P) (p / P / P % P) distance color fade line
      ),
    C(1.0, "peacock", fun _ ->
      let count             = genFeatureCount dna
      let potential, radius = genPotentialAndRadius dna
      let mix               = genBasisMixOp dna
      let color             = genCellColor dna
      let fade              = genPotentialFade dna
      peacock (genLayout dna |> layoutFunction) count potential mix color fade (dna.float32("Peacock shading", squared)) radius
      ),
    C(1.0, "Julia", fun _ ->
      let lerpc = lerp -1.0f 1.0f
      let lerpq = lerp -1.0f 1.0f
      let iterations = dna.int("Iterations", 2, 10)
      let formula    = dna.category("Formula", C(Julia), C(AexionJulia), C(Makin), C(White), C(Mandelcorn))
      let count      = genFeatureCount dna
      let radius     = dna.float32("Feature radius", lerp 0.5f 1.25f)
      let roughness  = dna.float32("Roughness", xerp 0.5f 2.0f)
      let fade       = genJuliaFade dna
      let cOrigin    = Vec3f(dna.float32("C-origin X", lerpc),
                             dna.float32("C-origin Y", lerpc),
                             if formula.is2D then 0.0f else dna.float32("C-origin Z", lerpc))
      let qOrigin    = Vec3f(dna.float32("Q-origin X", lerpq),
                             dna.float32("Q-origin Y", lerpq),
                             if formula.is2D then 0.0f else dna.float32("Q-origin Z", lerpq))
      let cRange     = dna.float32("C-range", squared >> lerp 0.5f 2.0f)
      let qRange     = dna.float32("Q-range", squared >> lerp 0.5f 2.0f)
      let cScale     = dna.float32("C-scale", lerp 0.0f 2.0f)
      let qScale     = dna.float32("Q-scale", lerp 0.0f 2.0f)
      julia (genLayout dna |> layoutFunction) count formula iterations roughness radius fade cOrigin qOrigin cRange qRange cScale qScale
      ),
    C(dualWeight, "Julia orbit trap", fun _ ->
      let lerpc = lerp -1.0f 1.0f
      let lerpq = lerp -1.0f 1.0f
      let iterations = dna.int("Iterations", 2, 8)
      let formula    = dna.category("Formula", C(Julia), C(AexionJulia), C(Makin), C(White), C(Mandelcorn))
      let count      = genFeatureCount dna
      let radius     = dna.float32("Feature radius", sqrt >> lerp 0.5f 1.25f)
      let cOrigin    = Vec3f(dna.float32("C-origin X", lerpc),
                             dna.float32("C-origin Y", lerpc),
                             if formula.is2D then 0.0f else dna.float32("C-origin Z", lerpc))
      let qOrigin    = Vec3f(dna.float32("Q-origin X", lerpq),
                             dna.float32("Q-origin Y", lerpq),
                             if formula.is2D then 0.0f else dna.float32("Q-origin Z", lerpq))
      let cRange     = dna.float32("C-range", squared >> lerp 0.5f 2.0f)
      let qRange     = dna.float32("Q-range", squared >> lerp 0.5f 2.0f)
      let fade       = genJuliaFade dna
      let cScale     = dna.float32("C-scale", lerp 0.0f 2.0f)
      let qScale     = dna.float32("Q-scale", lerp 0.0f 2.0f)
      let trap       = Vec3f(dna.float32("Trap X", lerpc),
                             dna.float32("Trap Y", lerpc),
                             if formula.is2D then 0.0f else dna.float32("Trap Z", lerpc))
      let trapR      = dna.float32("Trap radius", lerp 1.0f 2.0f)
      let trapFade   = genTrapFade dna
      let mix        = genBasisMixOp dna
      let atlas      = genAtlas genPattern dna
      orbit (genLayout dna |> layoutFunction) count formula iterations cOrigin qOrigin cRange qRange cScale qScale fade trap trapR trapFade atlas mix radius
      ),
    C(dualWeight, "displace", fun _ ->
      let factor   = genFactor()
      let displace = genDisplacement 0.2f 1.5f dna
      let basis1   = dna.descend("Displacer", Map3Info.normalizeBasis genShapedBasis)
      let basis2   = dna.descend("Base", genBasis maxDepth')
      displaceBasis displace factor basis1 basis2
      ),
    C(dualWeight, "layer", fun _ ->
      let factor = genFactor()
      let width  = dna.float32("Layer width", lerp 2.0f 5.0f)
      let fade   = genLayerFade dna
      let basis1 = dna.descend("Layer", genBasis maxDepth')
      let basis2 = dna.descend("Base", genBasis maxDepth')
      binaryBasis (layer width fade) factor basis1 basis2
      ),
    C(dualWeight, "component softmix", fun _ ->
      let factor = genFactor()
      let mix    = genSoftmix dna
      let basis1 = dna.descend("Basis 1", genBasis maxDepth')
      let basis2 = dna.descend("Basis 2", genBasis maxDepth')
      binaryBasis mix factor basis1 basis2
      ),
    C(dualWeight, "norm softmix", fun _ ->
      let factor = genFactor()
      let mix    = genSoftmix3 dna
      let basis1 = dna.descend("Basis 1", genBasis maxDepth')
      let basis2 = dna.descend("Basis 2", genBasis maxDepth')
      binaryBasis mix factor basis1 basis2
      ),
    C(dualWeight, "layered rotate", fun _ ->
      let factor       = genFactor()
      let rotateAmount = dna.float32("Rotate amount", lerp 2.0f 6.0f)
      let rotateWidth  = dna.float32("Rotate width", lerp 1.0f 3.0f)
      let fade         = genLayerFade dna
      let basis1       = dna.descend("Rotator", genShapedBasis)
      let basis2       = dna.descend("Base", genBasis maxDepth')
      binaryBasis (rotatef rotateWidth rotateAmount fade) factor basis1 basis2
      ),
    C(dualWeight, "displace and rotate", fun _ ->
      let factor   = genFactor()
      let rotation = dna.float32("Rotate amount", lerp 2.0f 6.0f)
      let displace = genDisplacement 0.2f 1.5f dna
      let basis1   = dna.descend("Modifier", Map3Info.normalizeBasis genShapedBasis)
      let basis2   = dna.descend("Base", genBasis maxDepth')
      binaryBasisd (rotate rotation) displace factor basis1 basis2
      ),
    C(dualWeight, "displace and softmix", fun _ ->
      let factor   = genFactor()
      let mix      = genSoftmix3 dna
      let displace = genDisplacement 0.2f 1.5f dna
      let basis1   = dna.descend("Modifier", Map3Info.normalizeBasis genShapedBasis)
      let basis2   = dna.descend("Base", genBasis maxDepth')
      binaryBasisd mix displace factor basis1 basis2
      ),
    C(dualWeight, "shape", fun _ ->
      let factor      = genFactor()
      let overdrive   = dna.float32("Overdrive")
      let monoization = dna.float32("Monoization")
      let scattering  = dna.float32("Scattering")
      let basis1      = dna.descend("Shaper", genShapedBasis)
      let basis2      = dna.descend("Base", genBasis maxDepth')
      binaryBasis variableShape factor (shapeBasis (Vec3f(overdrive, monoization, scattering) |> scale) basis1) basis2
      ),
    C(dualWeight, "capsule flow", fun _ ->
      let radius        = dna.float32("Radius", lerp 0.1f 1.0f)
      let length        = dna.float32("Length", squared >> lerp 0.5f 4.0f)
      let fade          = genPotentialFade dna
      let shading       = dna.float32("Shading", squared)
      let cellColor     = genCellColor dna
      let mixOp         = genBasisMixOp dna
      let featureCount  = genFeatureCount dna
      let flowFrequency = dna.float32("Flow frequency", squared >> lerp 0.02f 2.0f, suffix = "x")
      let basis1        = dna.descend("Flow", Map3Info.normalizeBasis genShapedBasis)
      capflow (layoutFunction <| genLayout dna) featureCount mixOp cellColor fade shading length radius basis1 flowFrequency
      ),
    C(dualWeight, "potential flow", fun _ ->
      let potential, radius = genPotentialAndRadius dna
      let fade              = genPotentialFade dna
      let shading           = dna.float32("Shading", squared)
      let cellColor         = genCellColor dna
      let mixOp             = genBasisMixOp dna
      let featureCount      = genFeatureCount dna
      let flowFrequency     = dna.float32("Flow frequency", squared >> lerp 0.02f 2.0f, suffix = "x")
      let basis1            = dna.descend("Flow", Map3Info.normalizeBasis genShapedBasis)
      impflow (layoutFunction <| genLayout dna) featureCount potential mixOp cellColor fade shading radius basis1 flowFrequency
      ),
    C(dualWeight, "pattern flow", fun _ ->
      let potential, radius = genPotentialAndRadius dna
      let fade              = genPotentialFade dna
      let mixOp             = genBasisMixOp dna
      let featureCount      = genFeatureCount dna
      let curvature         = dna.float32("Curvature")
      let pattern           = genAtlas genPattern dna
      let flowFrequency     = dna.float32("Flow frequency", squared >> lerp 0.02f 2.0f, suffix = "x")
      let basis1            = dna.descend("Flow", Map3Info.normalizeBasis genShapedBasis)
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


/// Generates a fractalizer map.
let genFractalizer (subGen : Dna -> Map3) (dna : Dna) =
  let offset          = genOffset dna
  let octaves         = dna.int("Octaves", 2, 10)
  let basef           = dna.float32("Frequency", xerp 2.0f 16.0f)
  let roughness       = dna.float32("Roughness", xerp 0.4f 0.9f)
  let minLacunarity   = 0.5f / G sqrt2
  let maxLacunarity   = 0.5f * G sqrt2
  let genLacunarity() = dna.float32("Lacunarity", xerp minLacunarity maxLacunarity)
  let fractalizer =
    dna.branch("Fractalizer",
      C(1.0, "mix and displace", fun (dna : Dna) ->
        let lacunarity = genLacunarity()
        let highpass   = dna.float32("Highpass filter", lerp 0.0f 0.99f)
        let mix        = genMixOp dna
        let walk       = genWalk 0.1f 2.0f dna
        let basis      = genBasis 2 dna
        fractald roughness lacunarity highpass mix walk basef octaves basis
        ),
      C(1.0, "inverted mix and displace", fun (dna : Dna) ->
        let lacunarity = genLacunarity()
        let lowpass    = dna.float32("Lowpass filter", lerp 0.0f 0.99f)
        let mix        = genMixOp dna
        let walk       = genWalk 0.05f 0.5f dna
        let basis      = genBasis 2 dna
        fractaldi roughness lacunarity lowpass mix walk basef octaves basis
        ),
      C(1.0, "walk", fun _ ->
        let displace = dna.float32("Displace amount", xerp 0.2f 3.0f)
        let twist    = dna.float32("Twist amount", lerp 0.0f 3.0f)
        let basis    = genBasis 2 dna
        walk roughness (displace / basef) twist octaves (basis basef)
        ),
      C(1.0, "variable lowpass and displace", fun (dna : Dna) ->
        let lacunarity = genLacunarity()
        let displace   = dna.float32("Displace amount", squared >> lerp 0.0f 2.0f)
        let displaceV  = dna.float32("Displace variability", xerp 1.0f 8.0f)
        let twist      = dna.float32("Twist amount", squared >> lerp 0.0f 5.0f)
        let twistV     = dna.float32("Twist variability", xerp 1.0f 8.0f)
        let mix        = genMixOp dna
        let multiMap   = dna.descend("Variable map", Map3Info.normalize subGen)
        let basis      = genBasis 2 dna
        multid (roughness + 0.05f) lacunarity (displace / displaceV) displace (twist / twistV) twist basef 1.0f (float32 octaves + 0.999f) mix multiMap basis
        )
      )
  offset >> fractalizer >> genShape dna


/// Generates a binary operator.
let genBinop (subGen1 : Dna -> Map3) (subGen2 : Dna -> Map3) (dna : Dna) =
  let offset         = genOffset dna
  let displace()     = genDisplacement 0.01f 0.3f dna
  let rotateWidth()  = dna.float32("Rotate width", lerp 1.0f 4.0f)
  let rotateAmount() = dna.float32("Rotate amount", lerp 2.0f 6.0f)
  let layer()        = layer (dna.float32("Layer width", lerp 1.5f 5.0f)) (genLayerFade dna)
  let subName1, subName2, binop =
    dna.branch("Operator",
      C(1.0, "displace and rotate", fun _ ->
        let displace = displace()
        let rotateWidth = rotateWidth()
        let rotateAmount = rotateAmount()
        "Modifier", "Base", bimapd displace <| rotatef rotateWidth rotateAmount (genLayerFade dna)
        ),
      C(1.0, "displace and softmix", fun _ ->
        let displace = displace()
        "Modifier", "Base", bimapd displace <| genSoftmix3 dna
        ),
      C(2.0, "displace and layer", fun _ ->
        let displace = displace()
        "Modifier", "Base", bimapd displace <| layer()
        ),
      C(1.0, "layered rotate", fun _ ->
        let rotateWidth = rotateWidth()
        let rotateAmount = rotateAmount()
        "Rotator", "Base", bimap <| rotatef rotateWidth rotateAmount (genLayerFade dna)
        ),
      C(1.0, "rotate", fun _ ->
        "Rotator", "Base", bimap <| rotate (rotateAmount())
        ),
      C(1.0, "component softmix", fun _ ->
        "Map 1", "Map 2", bimap <| genSoftmix dna
        ),
      C(1.0, "norm softmix", fun _ ->
        "Map 1", "Map 2", bimap <| genSoftmix3 dna
        ),
      C(2.0, "layer", fun _ ->
        "Layer", "Base", bimap <| layer()
        )
      )
  let b0 = dna.descend(subName1, subGen1)
  let b1 = dna.descend(subName2, subGen2)
  binop b0 b1 >> genShape dna


/// Generates a node tree recursively. E is the "energy" left in this node.
/// It limits tree complexity probabilistically in random generation but does not hinder user editing.
let rec genNode (E : float) (dna : Dna) =

  let nodeWeight complexity = exp2(E - complexity) |> clamp 1.0e-6 1.0

  dna.branch("Node",
    C(2.0 * nodeWeight 1.0, "Basis", fun _ ->
      genOffset dna >> genBasis 3 dna (dna.float32("Frequency", xerp 2.0f 64.0f)) >> genShape dna
      ),
    C(nodeWeight 5.0, "Fractalizer", fun _ ->
      genFractalizer (genNode (0.7 * E)) dna
      ),
    C(0.5 * nodeWeight 2.0, "Unary", fun _ ->
      genUnary (genNode (0.75 * E)) dna
      ),
    C(nodeWeight 5.0, "Binary", fun _ ->
      genBinop (genNode (0.5 * E)) (genNode (0.5 * E)) dna
      )
    )



let ExplorerVersion = "0.3.0"

/// Map generator for Explorer. This is placed here to enable Explorer created textures to be read on any platform.
let generateExplorerMap = RichMap3.generate(fun dna ->
  dna.addLabel("FsMap3 Explorer Version " + ExplorerVersion)
  let layout = genLayout dna
  dna.addInjector(DnaInjector.create(fun _ _ choices -> Someval(choices.numberOf((=) layout))))
  genNode 5.0 dna
  )

