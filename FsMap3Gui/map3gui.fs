/// Map3 user interfaces.
module FsMap3.Map3Gui

open System
open System.Windows
open System.Windows.Media
open System.Windows.Shapes
open System.Windows.Controls
open System.Windows.Controls.Primitives

open Common
open Basis3
open Map3
open Map3Dna


/// Converts the solid texture to a BitmapSource, which is also frozen.
/// The camera function is invoked with X and Y arguments in the range [0, 1[.
/// Parallelizes computations using the .NET thread pool. Map component values
/// are clamped to [-1, 1] and transformed to the unit range. Optionally, a callback
/// (which must be reentrant) is invoked after each row is rendered.
let toBitmapSource (map : Map3) width height (camera : float32 -> float32 -> Vec3f) (callback : (int -> unit) option) =
  let callback = callback >? fun _ -> ()
  // Ensure 32-bit alignment of rows, just in case it is needed somewhere.
  let stride = (width * 3 + 3) &&& ~~~3
  let bytes = stride * height
  let pixelArray = Array.create bytes 0uy
  let inline topix x = byte <| clampi 0 255 (int x)
  let computeRow y =
    let mutable j = y * stride
    let yf = float32 y / float32 height
    for x = 0 to width - 1 do
      let xf = float32 x / float32 width
      let v = camera xf yf
      let color = (map v + Vec3f.one) * (0.5f * 256.0f)
      pixelArray.[j] <- topix color.x
      pixelArray.[j + 1] <- topix color.y
      pixelArray.[j + 2] <- topix color.z
      j <- j + 3
    callback y
  Array.Parallel.iter computeRow [| 0 .. height - 1 |]
  let source = System.Windows.Media.Imaging.BitmapSource.Create(width, height, 96.0, 96.0, System.Windows.Media.PixelFormats.Rgb24, null, pixelArray, stride)
  source.Freeze()
  source


/// Converts the texture to a Pixmap.
/// The camera function is invoked with X and Y arguments in the range [0, 1[.
/// Uses Array.Parallel to distribute work. Map component values are clamped to [-1, 1]
/// and transformed to the unit range. Optionally, a callback (which must be reentrant)
/// is invoked after each row is rendered.
let toPixmap (map : Map3) width height (camera : float32 -> float32 -> Vec3f) (callback : (int -> unit) option) =
  let callback = callback >? fun _ -> ()
  let pixmap = Pixmap.create(width, height)
  let computeRow y =
    let yf = float32 y / float32 height
    for x = 0 to width - 1 do
      let xf = float32 x / float32 width
      let v = camera xf yf
      pixmap.[x, y] <- (map v + 1G) * 0.5f
    callback y
  Array.Parallel.iter computeRow [| 0 .. height - 1 |]
  pixmap


/// Shows the square slice ([0, 1[, [0, 1[, 0) of the texture in a window.
/// Assumes texture values are normalized to [-1, 1] (see Map3.normalize).
let show size (map : Map3) =

  let bitmapSource = toBitmapSource map size size (fun x y -> Vec3f(x, y, 0.0f)) None

  let window = Window(ResizeMode = ResizeMode.CanMinimize, SizeToContent = SizeToContent.WidthAndHeight, Topmost = true, WindowStartupLocation = WindowStartupLocation.CenterScreen)
  let canvas = Canvas(Width = float size, Height = float size, Background = Brushes.Black)
  let image = Image(Width = float size, Height = float size, Source = bitmapSource)
  canvas.add(image)

  window.setPixelContent(canvas)
  window.KeyDown.Add(fun args -> if args.Key = System.Windows.Input.Key.Escape then window.Close())
  window.Show()


/// Exports a Map3 image to a PNG file. The camera function is invoked with X and Y arguments in [0, 1[.
let exportMap3Png (map : Map3) (width : int) (height : int) (camera : float32 -> float32 -> Vec3f) =
  let dialog = new Microsoft.Win32.SaveFileDialog(Title = "Export Image As..", DefaultExt = ".png", Filter = "PNG files (.png)|*.png")
  let result = dialog.ShowDialog()
  if result.HasValue && result.Value = true then
    let progressWindow = new Window(Title = "Exporting Image..", Topmost = true, ResizeMode = ResizeMode.CanMinimize, SizeToContent = SizeToContent.WidthAndHeight)
    let progressBar = new ProgressBar(Maximum = float height, Width = 400.0, Height = 25.0)
    progressWindow.Content <- progressBar
    progressWindow.Show()
    let rows = Atom.Int(0)
    let callback _ =
      let row = rows.post((+) 1)
      Wpf.dispatch(progressBar, fun _ -> progressBar.Value <- max progressBar.Value (float row))
    scheduleTask(fun _ ->
      let pixmap = toPixmap map width height camera (Some(callback))
      pixmap.savePNG(dialog.FileName)
      Wpf.dispatch(progressWindow, fun _ -> progressWindow.Close())
    )


