module FsMap3.WalkDna

open Common


/// Generates a displacement response.
let genDisplaceResponse minAmount maxAmount (dna : Dna) =
  dna.branch("Displace response",
    C("line", fun _ ->
      let amount = dna.float32("Displace amount", squared >> lerp minAmount maxAmount)
      fun x -> amount * x
      ),
    C("smooth line", fun _ ->
      let amount = dna.float32("Displace amount", squared >> lerp minAmount maxAmount)
      let acceleration = dna.float32("Displace smoothness")
      let A = max 0.001f (1.0f - sqrt acceleration)
      fun x -> let z = x / A in amount * A * (if z < 1.0f then squared z * (1G - Q 1 3 * z) else z - Q 1 3)
      ),
    C("power-1/2", fun _ ->
      let amount = dna.float32("Displace amount", squared >> lerp minAmount maxAmount)
      fun x -> amount * sqrt x
      ),
    C("power-2", fun _ ->
      let amount = dna.float32("Displace amount", lerp minAmount maxAmount) * 2.0f
      fun x -> amount * squared x
      ),
    C("power-3", fun _ ->
      let amount = dna.float32("Displace amount", lerp minAmount maxAmount) * 3.0f
      fun x -> amount * cubed x
      ),
    C("power-N", fun _ ->
      let power = dna.float32("Displace power", xerp 0.25f 4.0f)
      let amount = dna.float32("Displace amount", squared >> lerp minAmount maxAmount) * power
      fun x -> amount * x ** power
      ),
    C("sigmoid", fun _ ->
      let maximum = dna.float32("Displace maximum", squared >> lerp minAmount maxAmount)
      let slope = dna.float32("Displace slope")
      fun x -> maximum * softsign(squared slope * 40.0f * x)
      )
    )


/// Generates a walk operator.
let genWalk minAmount maxAmount (dna : Dna) =
  let response() =
    genDisplaceResponse minAmount maxAmount dna
  let damping() =
    let damping = dna.float32("Displace damping")
    squared damping
  dna.branch("Walk operator",
    C("march", fun _ ->
      let response = response()
      let damping = damping()
      Walk.march response damping
      ),
    C("twist", fun _ ->
      let response = response()
      let twist = dna.float32("Twist amount")
      let damping = damping()
      Walk.twist response twist damping
      ),
    C("shuffle", fun _ ->
      let response = response()
      let damping = damping()
      Walk.shuffle response damping
      ),
    C("lurch", fun _ ->
      let response = response()
      let damping = damping()
      Walk.lurch response damping
      ),
    C("bounce", fun _ ->
      let response = response()
      let damping = damping()
      Walk.bounce response damping
      ),
    C("cross", fun _ ->
      let response = response()
      let damping = damping()
      Walk.cross response damping
      ),
    C("none", fun _ ->
      Walk.stand
      )
    )

