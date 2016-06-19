/// Procedural generation of Map3 and related functions.
module FsMap3.Map3Dna

open Common
open Mangle
open Potential
open Basis3
open Map3
open Fractalizer
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
open WalkDna
open ShapeDna


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
  let genColorOptions (baseColor : CellColor) (dna : Dna) =
    dna.branch("Color variation",
      C(1.0, "none", fun _ ->
        baseColor),
      C(1.0, "gradient", fun _ ->
        let amount = dna.float32("Amount", lerp 0.0f 1.0f)
        let rnd = Rnd(dna.data("Angle seed"))
        let gradient = Convert.unitVec3 rnd.tick rnd.tick
        fun x (v : Vec3f) ->
          baseColor x v * (1.0f + v *. gradient * amount)
        ),
      C(1.0, "position", fun _ ->
        let amount = dna.float32("Amount", lerp 0.0f 1.0f)
        fun x (v : Vec3f) ->
          baseColor x v * (Vec3f.one + amount * v)
        )
      )
  dna.branch("Cell color",
    C(2.0, "any", genColorOptions anyColor),
    C("unit", genColorOptions unitColor),
    C("big", genColorOptions bigColor),
    C(1.0, "palette", fun _ ->
      let n = dna.int("Choices", 1, 8)
      let palette = Array.init n (fun i -> Vec3f.fromSeed(dna.data(sprintf "Color %d seed" i) |> mangle32, -1.0f, 1.0f))
      genColorOptions (paletteColor palette) dna
      )
    )


/// Generates a potential function.
let genPotential (dna : Dna) =
  let interR = Fade.reverse Fade.power3 >> lerp 0.05f 1.0f
  let interP = xerp 1.0f 8.0f
  dna.branch("Potential",
    C(3.0, "rounded cube", fun _ -> roundedCube),
    C("teardrop", fun _ -> teardrop),
    C(2.0, "rounded cylinder", fun _ ->
      roundedCylinder (dna.float32("Cylinder radius", interR))
      ),
    C(2.0, "rounded cone", fun _ ->
      roundedCone (dna.float32("Cone radius", interR))
      ),
    C(1.0, "torus", fun _ ->
      torus (dna.float32("Major axis", lerp 0.5f 0.95f))
      ),
    C(1.0, "Roman", fun _ -> roman),
    C(0.5, "superellipsoid", fun _ ->
      superellipsoid (dna.float32("Radial power", interP)) (dna.float32("Transverse power", interP))
      ),
    C(0.5, "supertorus", fun _ ->
      supertorus (dna.float32("Ring power", interP)) (dna.float32("Major axis", lerp 0.5f 0.95f))
      ),
    C(0.5, "supercone", fun _ ->
      supercone (dna.float32("Radial power", interP)) (dna.float32("Cone radius", interR))
      )
    )


/// Generates a potential function and feature radius. Constrains radius in an attempt
/// to get the potential to occupy at least 1% of the volume of the unit sphere.
let genPotentialAndRadius (dna : Dna) =
  let potential = genPotential dna
  let volume = sampleVolume 8 potential |> max 0.001f
  let minVolume = 0.01f
  let volumeFactor = volume / minVolume
  let minRadius = clamp 0.1f 0.9f (1.0f / cbrt volumeFactor)
  let radius = dna.float32("Potential radius", Fade.reverse Fade.power2 >> lerp minRadius 1.0f)
  (potential, radius)


/// Generates a feature count function.
let genFeatureCount (dna : Dna) =
  dna.branch("Features per cell",
    C(1.0, "one", fun _ -> unityCount),
    C(1.0, "rounded", fun _ -> flipCount (dna.float("Mean", xerp 0.2 4.0))),
    C(2.5, "Poisson", fun _ -> poissonCount (dna.float("Mean", xerp 0.2 4.0))),
    C(1.0, "geometric", fun _ -> geometricCount (dna.float("Mean", xerp 0.2 4.0)))
    )


/// Generates a mixing operator for octave or map mixing.
let genMixOp (dna : Dna) =
  dna.branch("Mix operator",
    C(4.0, "sum", fun _ -> Mix.sum),
    C(0.5, "difference", fun _ -> Mix.difference),
    C(0.5, "product", fun _ -> Mix.product),
    C(1.5, "over", fun _ -> Mix.over),
    C(2.0, "soft", fun _ -> Mix.soft (dna.float32("Bias", lerp -1.0f 1.0f))),
    C(1.5, "norm", fun _ -> Mix.norm (dna.float32("Hardness"))),
    C(3.0, "layer", fun _ ->
      let width   = dna.float32("Layer width", squared >> lerp 0.05f 1.0f)
      let fade    = genLayerFade dna
      // The norm multipliers below equalize distances assuming independent component values.
      let norm    = dna.category("Layer norm", C(1.5, "1-norm", fun (v : Vec3f) -> v.norm1),
                                               C(3.0, "2-norm", fun (v : Vec3f) -> 1.633f * v.length),
                                               C(1.5, "max", fun (v : Vec3f) -> 2.0f * v.maxNorm))
      let persist = dna.float32("Layer persist", lerp 0.0f 1.0f)
      Mix.layer width fade norm persist
      ),
    C(1.0, "rotate", fun _ -> Mix.rotate (dna.float32("Rotate amount", lerp 0.1f 1.0f)))
    )


/// Generates a mixing operator for intra-basis (feature) mixing.
let genBasisMixOp (dna : Dna) =
  dna.branch("Mix operator",
    C(4.0, "sum", fun _ -> Mix.sum),
    C(0.5, "difference", fun _ -> Mix.difference),
    C(0.5, "product", fun _ -> Mix.product),
    C(1.5, "over", fun _ -> Mix.over),
    C(1.0, "soft", fun _ -> Mix.soft (dna.float32("Bias", lerp -1.0f 1.0f))),
    C(1.0, "norm", fun _ -> Mix.norm (dna.float32("Hardness"))),
    C(0.8, "min", fun _ -> Mix.min),
    C(0.8, "max", fun _ -> Mix.max),
    C(1.0, "rotate", fun _ -> Mix.rotate (dna.float32("Rotate amount", lerp 0.1f 1.0f)))
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


/// Generates a unary operation.
let genUnary (subGen : Dna -> Map3) (dna : Dna) =

  let unaryShape (shapeGen : Dna -> Map3) (dna : Dna) =
    let shape = shapeGen dna
    subGen dna >> shape

  dna.branch("Unary op",
    C(1.0, "bleed", unaryShape genBleed),
    C(1.0, "shift", unaryShape genShift),
    C(1.0, "scatter", unaryShape genScatter),
    C(1.0, "wave packet", unaryShape genWavePacket),
    C(0.8, "component overdrive", unaryShape genOverdrive),
    C(0.8, "vector overdrive", unaryShape genVectorOverdrive),
    C(0.5, "component posterize", unaryShape genComponentPosterize),
    C(1.0, "vector posterize", unaryShape genVectorPosterize),
    C(0.25, "component reflect", unaryShape genComponentReflect),
    C(0.25, "vector reflect", unaryShape genVectorReflect)
    )


/// Generates a component softmix shaper.
let genSoftmix (dna : Dna) =
  softmix (dna.float32("Mix bias", lerp -6.0f 6.0f))


/// Generates a vector softmix shaper.
let genSoftmix3 (dna : Dna) =
  softmix3 (dna.float32("Mix bias", lerp -6.0f 6.0f))


/// Generates a basis function.
let rec genBasis maxDepth (dna : Dna) =
  let dualWeight = if maxDepth > 1 then 1.0 else 0.0
  let maxDepth'  = maxDepth - 1

  let genFactor() = dna.float32("Frequency factor", squared >> xerp 0.25f 4.0f)
  let genSeed() = dna.data("Cell seed")

  let recurseShapedBasis (dna : Dna) =
    let basis = genBasis maxDepth' dna
    shapeBasis (genShape dna) basis

  let recurseBasis (dna : Dna) =
    genBasis maxDepth' dna

  dna.branch("Basis",
    C(2.0, "Perlin noise", fun _ ->
      let seed = genSeed()
      let fade = dna.category("Perlin fade", C("smooth-1", Fade.smooth1), C("smooth-2", Fade.smooth2), C("smooth-3", Fade.smooth3))
      perlin (genLayout dna |> layoutFunction) fade seed
      ),
    C(1.0, "cubex noise", fun _ ->
      let seed  = genSeed()
      let color = genCellColor dna
      let fade  = dna.category("Cubex fade", C("sine", Fade.sine), C("smooth-2", Fade.smooth2), C("upward arc", Fade.uparc))
      cubex (genLayout dna |> layoutFunction) color fade seed
      ),
    C(0.5, "weave", fun _ ->
      let seed   = genSeed()
      let period = dna.int("Weave period", 2, 4)
      let fade   = dna.category("Weave", C("threaded", Fade.wave period), 
                                         C("quilted", Fade.wave period >> Fade.smooth2),
                                         C("wired", Fade.wave period >> Fade.shelf),
                                         C("tiled", Fade.clone period Fade.power4))
      // The wave fade increases the range so scale down a little.
      shapeBasis (scale 0.4f) (perlin (genFixedLayout dna |> layoutFunction) fade seed)
      ),
    C(1.0, "radial value noise", fun _ ->
      let seed  = genSeed()
      let color = genCellColor dna
      let fade  = dna.category("Radial fade", C(0.4, "smooth-1", Fade.smooth1), C(0.3, "smooth-2", Fade.smooth2), C(0.2, "smooth-3", Fade.smooth3), C(0.2, "power-2", Fade.power2))
      radial (genLayout dna |> layoutFunction) color fade seed
      ),
    C(1.0, "leopard", fun _ ->
      let seed      = genSeed()
      let radius    = dna.float32("Leopard radius", xerp 0.25f 1.0f)
      let count     = genFeatureCount dna
      let fadeWidth = dna.float32("Falloff width", lerp 0.1f 1.0f)
      let fade      = genPotentialFade dna
      let mix       = genBasisMixOp dna
      let color     = genCellColor dna
      leopard (genLayout dna |> layoutFunction) count mix color fade fadeWidth radius seed
      ),
    C(1.0, "Worley", fun _ ->
      let seed     = genSeed()
      let count    = genFeatureCount dna
      let distance = genCellDistance dna
      let P        = worleyPattern.size
      let p        = dna.data("Worley pattern", cubed P)
      let fade     = genWorleyFade dna
      worley (genLayout dna |> layoutFunction) count (p % P) (p / P % P) (p / P / P % P) distance fade seed
      ),
    C(1.0, "colored Worley", fun _ ->
      let seed     = genSeed()
      let count    = genFeatureCount dna
      let distance = genCellDistance dna
      let P        = worleyPattern.size
      let p        = dna.data("Worley pattern", cubed P)
      let fade     = genWorleyFade dna
      let line     = dna.float32("Cell fade distance", xerp 0.025f 0.25f)
      let color    = genCellColor dna
      worleyColor (genLayout dna |> layoutFunction) count (p % P) (p / P % P) (p / P / P % P) distance color fade line seed
      ),
    C(1.0, "tiles", fun _ ->
      let seed     = genSeed()
      let count    = genFeatureCount dna
      let distance = genCellDistance dna
      let line     = dna.float32("Cell fade distance", xerp 0.025f 0.25f)
      let shading  = dna.float32("Shading")
      let color    = genCellColor dna
      tiles (genLayout dna |> layoutFunction) count distance shading color line seed
      ),
    C(1.0, "peacock", fun _ ->
      let seed              = genSeed()
      let count             = genFeatureCount dna
      let potential, radius = genPotentialAndRadius dna
      let mix               = genBasisMixOp dna
      let gradient          = dna.float32("Gradient")
      let color             = genCellColor dna
      let fadeWidth         = dna.float32("Falloff width", lerp 0.1f 1.0f)
      let fade              = genPotentialFade dna
      peacock (genLayout dna |> layoutFunction) count potential mix gradient color fade fadeWidth radius seed
      ),
    C(1.0, "Julia", fun _ ->
      let lerpc = lerp -1.0f 1.0f
      let lerpq = lerp -1.0f 1.0f
      let seed       = genSeed()
      let iterations = dna.int("Iterations", 2, 10)
      let formula    = dna.category("Formula", C(Julia), C(AexionJulia), C(Makin), C(White), C(Mandelcorn))
      let count      = genFeatureCount dna
      let radius     = dna.float32("Feature radius", lerp 0.5f 1.5f)
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
      let fractalParameters = FractalParameters(cOrigin, qOrigin, cRange, qRange, cScale, qScale)
      julia (genLayout dna |> layoutFunction) count formula iterations roughness radius fade fractalParameters seed
      ),
    C(dualWeight, "Julia orbit trap", fun _ ->
      let lerpc = lerp -1.0f 1.0f
      let lerpq = lerp -1.0f 1.0f
      let seed       = genSeed()
      let iterations = dna.int("Iterations", 2, 8)
      let formula    = dna.category("Formula", C(Julia), C(AexionJulia), C(Makin), C(White), C(Mandelcorn))
      let count      = genFeatureCount dna
      let radius     = dna.float32("Feature radius", lerp 0.5f 1.5f)
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
      let color      = genCellColor dna
      let fractalParameters = FractalParameters(cOrigin, qOrigin, cRange, qRange, cScale, qScale)
      orbit (genLayout dna |> layoutFunction) count formula iterations fractalParameters fade trap trapR trapFade color mix radius seed
      ),
    C(dualWeight, "capsule flow", fun _ ->
      let seed          = genSeed()
      let radius        = dna.float32("Capsule radius", lerp 0.1f 1.0f)
      let length        = dna.float32("Capsule length", squared >> lerp 0.5f 4.0f)
      let fadeWidth     = dna.float32("Falloff width", lerp 0.1f 1.0f)
      let fade          = genPotentialFade dna
      let cellColor     = genCellColor dna
      let mixOp         = genBasisMixOp dna
      let featureCount  = genFeatureCount dna
      let flowFrequency = dna.float32("Flow frequency", squared >> lerp 0.02f 2.0f, suffix = "x")
      let basis1        = dna.descend("Flow", recurseShapedBasis)
      capflow (layoutFunction <| genLayout dna) featureCount mixOp cellColor fade fadeWidth length radius basis1 flowFrequency seed
      ),
    C(dualWeight, "potential flow", fun _ ->
      let seed              = genSeed()
      let potential, radius = genPotentialAndRadius dna
      let fadeWidth         = dna.float32("Falloff width", lerp 0.1f 1.0f)
      let fade              = genPotentialFade dna
      let gradient          = dna.float32("Gradient")
      let cellColor         = genCellColor dna
      let mixOp             = genBasisMixOp dna
      let featureCount      = genFeatureCount dna
      let flowFrequency     = dna.float32("Flow frequency", squared >> lerp 0.02f 2.0f, suffix = "x")
      let basis1            = dna.descend("Flow", recurseShapedBasis)
      impflow (layoutFunction <| genLayout dna) featureCount potential mixOp gradient cellColor fade fadeWidth radius basis1 flowFrequency seed
      ),
    C(dualWeight, "displace", fun _ ->
      let factor   = genFactor()
      let displace = shape3 (genDisplaceResponse 0.1f 1.5f dna)
      let basis1   = dna.descend("Displacer", recurseShapedBasis)
      let basis2   = dna.descend("Base", recurseBasis)
      displaceBasis displace factor basis1 basis2
      ),
    C(dualWeight, "displace and mix", fun _ ->
      let factor   = genFactor()
      let mixOp    = genBasisMixOp dna
      let displace = shape3 (genDisplaceResponse 0.0f 1.5f dna)
      let basis1   = dna.descend("Modifier", recurseShapedBasis)
      let basis2   = dna.descend("Base", recurseBasis)
      binaryBasisd (mix mixOp) displace factor basis1 basis2
      ),
    C(dualWeight, "layer", fun _ ->
      let factor = genFactor()
      let width  = dna.float32("Layer width", squared >> lerp 0.05f 1.0f)
      let fade   = genLayerFade dna
      let basis1 = dna.descend("Layer", recurseBasis)
      let basis2 = dna.descend("Base", recurseBasis)
      binaryBasis (layer width fade) factor basis1 basis2
      ),
    C(dualWeight, "component softmix", fun _ ->
      let factor = genFactor()
      let mix    = genSoftmix dna
      let basis1 = dna.descend("Basis 1", recurseBasis)
      let basis2 = dna.descend("Basis 2", recurseBasis)
      binaryBasis mix factor basis1 basis2
      ),
    C(dualWeight, "norm softmix", fun _ ->
      let factor = genFactor()
      let mix    = genSoftmix3 dna
      let basis1 = dna.descend("Basis 1", recurseBasis)
      let basis2 = dna.descend("Basis 2", recurseBasis)
      binaryBasis mix factor basis1 basis2
      ),
    C(dualWeight, "layered rotate", fun _ ->
      let factor       = genFactor()
      let rotateAmount = dna.float32("Rotate amount", lerp 2.0f 6.0f)
      let rotateWidth  = dna.float32("Rotate width", lerp 1.0f 3.0f)
      let fade         = genLayerFade dna
      let basis1       = dna.descend("Rotator", recurseShapedBasis)
      let basis2       = dna.descend("Base", recurseBasis)
      binaryBasis (rotatef rotateWidth rotateAmount fade) factor basis1 basis2
      ),
    C(dualWeight, "displace and rotate", fun _ ->
      let factor       = genFactor()
      let rotateAmount = dna.float32("Rotate amount", lerp 2.0f 6.0f)
      let displace     = shape3 (genDisplaceResponse 0.0f 1.5f dna)
      let basis1       = dna.descend("Modifier", recurseShapedBasis)
      let basis2       = dna.descend("Base", recurseBasis)
      binaryBasisd (rotate rotateAmount) displace factor basis1 basis2
      ),
    C(dualWeight, "displace and softmix", fun _ ->
      let factor   = genFactor()
      let mix      = genSoftmix3 dna
      let displace = shape3 (genDisplaceResponse 0.0f 1.5f dna)
      let basis1   = dna.descend("Modifier", recurseShapedBasis)
      let basis2   = dna.descend("Base", recurseBasis)
      binaryBasisd mix displace factor basis1 basis2
      ),
    C(dualWeight, "shape", fun _ ->
      let factor      = genFactor()
      let overdrive   = dna.float32("Overdrive")
      let monoization = dna.float32("Monoization")
      let shift       = dna.float32("Shift")
      let basis1      = dna.descend("Shaper", recurseShapedBasis)
      let basis2      = dna.descend("Base", recurseBasis)
      binaryBasis (variableShape overdrive monoization shift) factor basis1 basis2
      )
    )


/// Generates a shaped basis function.
let genShapedBasis maxDepth (dna : Dna) =
  let basis = genBasis maxDepth dna
  shapeBasis (genShape dna) basis


/// Generates a fractalizer map.
let genFractalizer (subGen : Dna -> Map3) (dna : Dna) =
  let offset          = genOffset dna
  let octaves         = dna.int("Octaves", 2, 12)
  let basef           = dna.float32("Frequency", xerp 2.0f 16.0f)
  let roughness       = dna.float32("Roughness", xerp 0.4f 0.9f)
  let minLacunarity   = 0.5f / G sqrt2
  let maxLacunarity   = 0.5f * G sqrt2
  let genLacunarity() = dna.float32("Lacunarity", xerp minLacunarity maxLacunarity)
  let basis()         = genBasis 2 dna
  let fractalizer =
    dna.branch("Fractalizer",
      C(2.0, "mix and displace", fun _ ->
        let lacunarity = genLacunarity()
        let highpass   = dna.float32("Highpass filter", lerp 0.0f 0.9f)
        let mix        = genMixOp dna
        let walk       = genWalk 0.0f 2.0f dna
        let basis      = basis()
        fractald roughness lacunarity highpass mix walk basef octaves basis
        ),
      C(1.0, "inverted mix and displace", fun _ ->
        let lacunarity = genLacunarity()
        let lowpass    = dna.float32("Lowpass filter", lerp 0.0f 0.9f)
        let mix        = genMixOp dna
        let walk       = genWalk 0.0f 1.0f dna
        let basis      = basis()
        fractaldi roughness lacunarity lowpass mix walk basef octaves basis
        ),
      C(1.0, "walk", fun _ ->
        let displace = dna.float32("Displace amount", xerp 0.2f 3.0f)
        let twist    = dna.float32("Twist amount", lerp 0.0f 3.0f)
        let basis    = basis()
        walk roughness (displace / basef) twist octaves (basis basef)
        ),
      C(2.0, "variable", fun _ ->
        let lacunarity = genLacunarity()
        let minOctaves = dna.int("Minimum octaves", 1, octaves)
        let direction  = dna.category("Octave direction", C("forward", 1), C("inverse", -1))
        let first      = dna.int("First octave", 0, octaves - 1)
        let displace   = dna.float32("Displace variation")
        let twist      = dna.float32("Twist variation")
        let mix        = genMixOp dna
        let walk       = genWalk 0.0f 1.0f dna
        let basis      = basis()
        let variable   = dna.descend("Variable map", subGen)
        fractalv roughness lacunarity mix walk displace (twist * G tau) (float32 minOctaves) (float32 octaves + 0.9999f) first direction basef variable basis
        )
      )
  offset >> fractalizer >> genShape dna


/// Generates a binary operator.
let genBinop (subGen1 : Dna -> Map3) (subGen2 : Dna -> Map3) (dna : Dna) =
  let offset         = genOffset dna
  let displacement() = shape3 (genDisplaceResponse 0.0f 0.5f dna)
  let rotateWidth()  = dna.float32("Rotate width", lerp 1.0f 4.0f)
  let rotateAmount() = dna.float32("Rotate amount", lerp 2.0f 6.0f)
  let layer()        = layer (dna.float32("Layer width", squared >> lerp 0.1f 1.0f)) (genLayerFade dna)
  let subName1, subName2, binop =
    dna.branch("Operator",
      C(1.0, "displace", fun _ ->
        let displace = displacement()
        "Modifier", "Base", fun (a : Map3) (b : Map3) (v : Vec3f) -> b (v + displace (a v))
        ),
      C(1.0, "displace and rotate", fun _ ->
        let displace = displacement()
        let rotateWidth = rotateWidth()
        let rotateAmount = rotateAmount()
        "Modifier", "Base", bimapd displace <| rotatef rotateWidth rotateAmount (genLayerFade dna)
        ),
      C(1.0, "displace and softmix", fun _ ->
        let displace = displacement()
        "Modifier", "Base", bimapd displace <| genSoftmix3 dna
        ),
      C(2.0, "displace and layer", fun _ ->
        let displace = displacement()
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

  let nodeWeight complexity =
    // Set a hard limit anyway to prevent ridiculously complex maps from being generated.
    if E > 1.0e-2 then
      exp2(E - complexity) |> clamp 1.0e-6 1.0
    else 0.0

  dna.branch("Node",
    C(2.0 * nodeWeight 1.0 |> max 1.0e-6, "Basis", fun _ ->
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



let EditorVersion = "0.3.0"

/// Map generator for FsMap3 Editor. This is placed here to enable Editor created textures to be read on any platform.
let generateEditorMap enableFilter = RichMap3.generate enableFilter (fun dna ->
  dna.addLabel("FsMap3 Editor Version " + EditorVersion)
  let layout = genLayout dna
  dna.addInjector(DnaInjector.create(fun _ _ choices -> Someval(choices.numberOf((=) layout))))
  genNode 5.0 dna
  )

let loadEditorMap = generateEditorMap false

