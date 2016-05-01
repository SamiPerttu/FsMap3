module FsMap3.RayGui

open Common
open Ray

open System
open System.Windows
open System.Windows.Media
open System.Windows.Shapes
open System.Windows.Controls
open System.Threading


/// Visualizes a material by displaying a scene where the camera rotates around a sphere, cylinder, and cube.
let visualize windowSize material =

  let frames = 100
  let sphere = Sphere(Vec3(0.0, -0.4, 0.0), 0.5)
  let cylinder = Cylinder(Vec3(0.5, 0.6, 0.0), 0.55, 0.45, smoothNormals = true)
  let box = Box(Vec3(-0.55, 0.55, 0.0), Vec3(0.33), smoothNormals = true)
  let light = Vec3(1.0, -2.0, -2.0)
  let w, h = windowSize, windowSize

  let background = fun (v : Vec3f) -> Vec3f(v.y * 0.1f, v.y * 0.1f, v.y * 0.1f)

  let traceableArray =
    [|
    Traceable.create(sphere, material)
    Traceable.create(cylinder, material)
    Traceable.create(box, material)
    |]

  let computeImage t =
    let cameraPos = Vec3(3.0 * sinr t, -0.3, -3.0 * cosr t)
    let lookAt = Vec3(0.0, 0.24, 0.0)
    let cameraZ = (lookAt - cameraPos).normalize
    let cameraY = Vec3(0.0, -0.78, 0.0)
    let cameraX = cameraY *+ cameraZ
    let camera = Camera.create(cameraPos, cameraZ, cameraX, cameraY)
    let scene = Scene.create(camera, traceableArray, light)
    let buffer = { RayBuffer.pixmap = Pixmap.create(w, h, Vec3f(0.22f, 0.0f, 0.14f)); traceableMap = Grid.create(w, h) }
    let yArray = [| 0 .. h - 1 |]
    Array.Parallel.iter (renderLine buffer scene background) yArray
    Array.Parallel.iter (refineLine buffer scene background) yArray
    buffer.pixmap

  let alive = Atom.Int(1)

  let window = new Window(ResizeMode = ResizeMode.CanMinimize, SizeToContent = SizeToContent.WidthAndHeight, Topmost = true, WindowStartupLocation = WindowStartupLocation.CenterScreen)
  let canvas = new Canvas(Width = float windowSize, Height = float windowSize, Background = Brushes.Black)
  let image = new Image(Width = float windowSize, Height = float windowSize)
  canvas.add(image)
  window.setPixelContent(canvas)
  window.KeyDown.Add(fun args -> if args.Key = System.Windows.Input.Key.Escape then window.Close())
  window.Closed.Add(fun _ -> alive.set(0))
  window.Show()

  let threadFunction() =
    let mutable t = 0.0
    while !alive <> 0 do
      let pixmap = computeImage t
      Wpf.dispatch(image, fun _ -> image.Source <- pixmap.bitmapSource())
      t <- fract(t + 1.0 / float frames)

  // Spawn thread to handle drawing.
  let thread = new Thread(threadFunction)
  thread.Start()


