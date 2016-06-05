/// Benchmarks.
module FsMap3.Benchmark

open Common


/// Logs benchmarks of some (mostly number crunching) functions.
let benchmark() =

    Log.infof "Running benchmarks (single threaded)."
    Log.infof "Leopard.leopardd 0.8 calls/s           : %.1f" (Profile.profileVec3f (Leopard.leopardd 0.8f 50.0f))
    Log.infof "Leopard.leopardd 0.5 calls/s           : %.1f" (Profile.profileVec3f (Leopard.leopardd 0.5f 50.0f))
    Log.infof "Perlin.perlind calls/s                 : %.1f" (Profile.profileVec3f (Perlin.perlind 50.0f))
    Log.infof "Cubex.cubexd calls/s                   : %.1f" (Profile.profileVec3f (Cubex.cubexd 50.0f))
    Log.infof "Peacock.peacockd torus calls/s         : %.1f" (Profile.profileVec3f (Peacock.peacockd (Potential.torus 0.7f) 1.0f 50.0f))
    Log.infof "Peacock.peacockd superellipsoid calls/s: %.1f" (Profile.profileVec3f (Peacock.peacockd (Potential.superellipsoid 1.0f 4.0f) 1.0f 50.0f))

    Log.infof "Mangle.mangle32 calls/s  : %.1f" (Profile.profileInt (Mangle.mangle32))
    Log.infof "Mangle.mangle32c calls/s : %.1f" (Profile.profileInt (Mangle.mangle32b))
    Log.infof "Mangle.manglef64 calls/s : %.1f" (Profile.profileFloat (Mangle.manglef64))

