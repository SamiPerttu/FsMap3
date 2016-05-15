/// Fade function generators.
module FsMap3.FadeDna

open Common


/// Generates sigmoidal fades of varying hardness.
let genSigmoidFade (parameterName : string) (dna : Dna) : float32 -> float32 = 
  dna.ordered(parameterName,
    C(0.30, "0", Fade.sine),
    C(0.30, "1", Fade.smooth2),
    C(0.30, "2", Fade.smooth3),
    C(0.30, "3", Fade.smooth1 >> Fade.smooth2),
    C(0.25, "4", Fade.smooth2 >> Fade.smooth2),
    C(0.25, "5", Fade.smooth2 >> Fade.smooth3),
    C(0.25, "6", Fade.smooth3 >> Fade.smooth3),
    C(0.20, "7", Fade.smooth2 >> Fade.smooth2 >> Fade.smooth2),
    C(0.20, "8", Fade.smooth2 >> Fade.smooth2 >> Fade.smooth3),
    C(0.15, "9", Fade.smooth3 >> Fade.smooth3 >> Fade.smooth3),
    C(0.15, "10", Fade.smooth1 >> Fade.smooth1 >> Fade.smooth1 >> Fade.smooth1 >> Fade.smooth1 >> Fade.smooth2)
    )


/// Generates a fade for the layering mix operator.
let genLayerFade = genSigmoidFade "Layer hardness"


/// Generates a fade for potential functions.
let genPotentialFade (dna : Dna) : float32 -> float32 =
  dna.category("Potential fade",
    C("downward arc", Fade.downarc),
    C("reverse power-3", Fade.reverse Fade.power3),
    C("reverse power-2", Fade.reverse Fade.power2),
    C("saturated smooth", Fade.skew -1.0f >> Fade.smooth2),
    C("line", Fade.line),
    C("smooth line", Fade.smoothLine),
    C("sine", Fade.sine),
    C("smooth-2", Fade.smooth2),
    C("power-2", Fade.power2)
    )


/// Generates a fade for displacements.
let genDisplaceFade (dna : Dna) : float32 -> float32 =
  dna.category("Displace response",
    C("reverse power-4", Fade.reverse Fade.power4),
    C("reverse power-3", Fade.reverse Fade.power3),
    C("reverse power-2", Fade.reverse Fade.power2),
    C("shelf", Fade.shelf),
    C("line", Fade.line),
    C("smooth line", Fade.smoothLine),
    C("sine", Fade.sine),
    C("smooth-2", Fade.smooth2),
    C("power-2", Fade.power2),
    C("power-3", Fade.power3),
    C("power-4", Fade.power4)
    )


/// Generates a fade for the Worley cellular basis.
let genWorleyFade (dna : Dna) : float32 -> float32 =
  dna.category("Worley fade",
    C("downward arc", Fade.downarc),
    C("reverse power-3", Fade.reverse Fade.power3),
    C("reverse power-2", Fade.reverse Fade.power2),
    C("line", Fade.line),
    C("smooth line", Fade.smoothLine),
    C("sine", Fade.sine),
    C("smooth-2", Fade.smooth2),
    C("smooth-3", Fade.smooth3),
    C("power-2", Fade.power2),
    C("power-3", Fade.power3),
    C("upward arc", Fade.uparc)
    )


/// Generates a parameter interpolating fade for the fractal bases Julia and orbit.
let genJuliaFade (dna : Dna) : float32 -> float32 =
  dna.category("Feature fade",
    C("saturated sine", Fade.skew -1.0f >> Fade.sine),
    C("smooth line", Fade.smoothLine),
    C("sine", Fade.sine),
    C("smooth-2", Fade.smooth2),
    C("smooth-3", Fade.smooth3),
    C("power-2", Fade.power2),
    C("power-3", Fade.power3),
    C("upward arc", Fade.uparc)
    )


let genTrapFade (dna : Dna) : float32 -> float32 =
  dna.category("Orbit trap fade",
    C("downward arc", Fade.downarc),
    C("reverse power-3", Fade.reverse Fade.power3),
    C("reverse power-2", Fade.reverse Fade.power2),
    C("saturated smooth", Fade.saturate 0.3f >> Fade.smooth2),
    C("line", Fade.line),
    C("smooth line", Fade.smoothLine),
    C("sine", Fade.sine),
    C("smooth-2", Fade.smooth2),
    C("power-2", Fade.power2),
    C("upward arc", Fade.uparc)
    )

