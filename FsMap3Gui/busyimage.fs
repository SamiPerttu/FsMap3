/// Busy animation.
module Fuse.BusyImage

open Common


let busyBitmaps =

  // Number of animation frames.
  let n      = 12
  // Outer radius of disc.
  let r1     = 9.4f
  // Inner radius of disc.
  let r0     = r1 * 0.33f
  // Spinner border width.
  let border = 0.3f
  // Antialiasing width.
  let A      = 0.9f

  let r0b  = r0 + border
  let r1b  = r1 - border
  let size = r1 * 2G |> ceil |> int

  let bitmaps = Darray.create()

  let origin = Vec2f(Q size 2)

  for i = 0 to n - 1 do

    let phi = G i / G n

    let pixmap = Pixmap.create(size, size, fun x y ->
 
      let p     = Vec2f(float32 x + 0.5f, float32 y + 0.5f) - origin
      let d     = p.length
      let angle = p.angle / G tau

      if d > r0 && d < r1 then
        // Get alpha.
        let a = delerp01 r0 (r0 + A) d * delerp01 r1 (r1 - A) d
        // Get color.
        let c = fract (angle - phi)
        let c = c * delerp01 r0b (r0b + A) d * delerp01 r1b (r1b - A) d
        Vec3f(a, c, 0.0f)
      else
        Vec3f.zero
      )

    bitmaps.add(pixmap.bitmapSourceWithAlpha(fun c -> Vec4f(c.y, sqrt c.y, sqrt c.y, c.x)))

  bitmaps.toArray

