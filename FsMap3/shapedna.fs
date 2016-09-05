module FsMap3.ShapeDna

open Common
open Map3
open FadeDna


/// Generates a component shifting map.
let genShift (dna : Dna) =
  let dx = dna.float32("X shift", lerp -0.5f 0.5f)
  let dy = dna.float32("Y shift", lerp -0.5f 0.5f)
  let dz = dna.float32("Z shift", lerp -0.5f 0.5f)
  let wave = dna.category("Shift wave", C("triangle", trir),
                                        C("smooth triangle", fun x -> Fade.cosifyr (Fade.smoothLine 0.3f) (x - Q 1 4)),
                                        C("sine", sinrFast)
                                        )
  shift wave (Vec3f(dx, dy, dz))


/// Generates a scattering map.
let genScatter (dna : Dna) =
  let rnd = Rnd(dna.data("Scatter seed"))
  let amount = dna.float32("Scatter amount")
  let t = dna.float32("Scatter tuning")
  let p() = Vec3f.fromSeed(rnd.tick).map(squared) + t * Vec3f.fromSeed(rnd.tick).map(squared)
  let f() = Vec3f.fromSeed(rnd.tick) * Vec3f.fromSeed(rnd.tick, -0.5f, 0.5f)
  let px, py, pz = p(), p(), p()
  let fx, fy, fz = f(), f(), f()
  fun (v : Vec3f) ->
    Vec3f.lerp v (Vec3f((fx * v + px).sinr.sum, (fy * v + py).sinr.sum, (fz * v + pz).sinr.sum)) amount


/// Generates a bleed map.
let genBleed (dna : Dna) =
  // This formulation retains the sum of the components.
  let a = dna.float32("Bleed amount", lerp -1.0f 1.0f)
  fun (v : Vec3f) -> Vec3f(v.x + a * v.z - a * v.y, v.y + a * v.x - a * v.z, v.z + a * v.y - a * v.x)


/// Generates a vector posterizer map.
let genVectorPosterize (dna : Dna) =
  let levels = dna.float32("Posterize levels", xerp 0.5f 20.0f)
  let phase = dna.float32("Posterize offset")
  posterize3 (genSigmoidFade "Posterize hardness" dna) phase levels


/// Generates a component posterizer map.
let genComponentPosterize (dna : Dna) =
  let levels = dna.float32("Posterize levels", xerp 0.5f 20.0f)
  let phase = dna.float32("Posterize offset")
  posterize (genSigmoidFade "Posterize hardness" dna) phase levels


/// Generates a vector reflection map.
let genVectorReflect (dna : Dna) =
  let fade = dna.category("Reflect fade", C(0.5, "reverse power-2", Fade.reverse Fade.power2),
                                          C("line", Fade.line),
                                          C("smooth line", Fade.smoothLine 0.3f),
                                          C("smooth-2", Fade.smooth2),
                                          C("smooth-3", Fade.smooth3),
                                          C("worm", Fade.worm 2),
                                          C(0.5, "power-2", Fade.power2)
                                          )
  let amount = dna.float32("Reflect amount", squared >> lerp 0.1f 10.0f)
  reflect3f fade amount


/// Generates a component reflection map.
let genComponentReflect (dna : Dna) =
  let wave = dna.category("Reflect wave", C(1.5, "line", trir),
                                          C("smooth line", fun x -> Fade.cosifyr (Fade.smoothLine 0.3f) (x - Q 1 4)),
                                          C("sine", sinrFast),
                                          C("smooth-2", fun x -> Fade.cosifyr Fade.smooth2 (x - Q 1 4)),
                                          C("power-2", Fade.sinefyr Fade.power2)
                                          )
  let amount = dna.float32("Reflect amount", squared >> lerp 0.1f 10.0f)
  let offset = Vec3f(dna.float32("Reflect offset", lerp -1.0f 1.0f) |> (/) 8.0f)
  translate offset >> reflect wave amount


/// Generates a component overdrive map.
let genOverdrive (dna : Dna) =
  overdrive (dna.float32("Overdrive amount") |> xerp 0.5f 50.0f)


/// Generates a vector overdrive map.
let genVectorOverdrive (dna : Dna) =
  overdrive3 (dna.float32("Overdrive amount") |> xerp 0.5f 50.0f)


/// Generates a wave packet map.
let genWavePacket (dna : Dna) =
  let range = 1 <<< 23
  let seed  = dna.data("Packet seed", range)
  let mix   = dna.float32("Mix amount")
  let map   = packet (1.0f + float32 seed / float32 range)
  fun (v : Vec3f) -> lerp v (map v) mix


/// Generates a shaping function (or nothing).
let genShape (dna : Dna) =
  dna.branch("Shape",
    C(8.0, "none", always identity),
    C(1.0, "bleed", genBleed),
    C(1.0, "shift", genShift),
    C(1.0, "scatter", genScatter),
    C(1.0, "wave packet", genWavePacket),
    C(0.8, "component overdrive", genOverdrive),
    C(0.8, "vector overdrive", genVectorOverdrive),
    C(0.5, "component posterize", genComponentPosterize),
    C(1.0, "vector posterize", genVectorPosterize),
    C(0.25, "component reflect", genComponentReflect),
    C(0.25, "vector reflect", genVectorReflect)
    )
