/// Busy animation.
module FsMap3.BusyImage

open Common


let busyBitmaps =
  
  let N = 3
  let L = N - 1

  let bitmaps = Darray.create()

  let next x y =
    match x, y with
    | x, 0 when x < L -> x + 1, 0
    | L, y when y < L -> L, y + 1
    | x, L when x > 0 -> x - 1, L
    | 0, y when y > 0 -> 0, y - 1
    | _ -> failwith "BusyImage.busyBitmaps: Bug."

  let border = 1
  let dot    = 5
  let size   = dot * N + border * (N + 1)
  let trail  = 4

  let rec drawPixmap x y =

    let x, y = next x y

    let pixmap = Pixmap.create(size, size)

    let rec drawTrail dx dy i =
      let dx, dy = next dx dy
      let color = lerp (Vec3f(0.0f)) (Vec3f(1.0f)) (Q i trail)
      for px = 0 to dot - 1 do
        for py = 0 to dot - 1 do
          pixmap.[dx * (dot + border) + border + px, dy * (dot + border) + border + py] <- color
      if i < trail then
        drawTrail dx dy (i + 1)

    drawTrail x y 1
    bitmaps.add(pixmap.bitmapSource())

    if x = 0 && y = 0 then bitmaps.toArray else drawPixmap x y

  drawPixmap 0 0

