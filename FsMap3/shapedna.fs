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
                                        C("smooth triangle", fun x -> Fade.cosifyr Fade.smoothLine (x - Q 1 4)),
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
