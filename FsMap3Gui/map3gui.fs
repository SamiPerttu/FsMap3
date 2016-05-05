/// Map3 GUI stuff.
module FsMap3.Map3Gui

open System
open System.Windows
open System.Windows.Media
open System.Windows.Media.Imaging
open System.Windows.Shapes
open System.Windows.Controls
open System.Windows.Controls.Primitives

open Common
open Basis3
open Map3
open Map3Dna


type Pixmap with

  /// Converts the Map3 to a Pixmap.
  /// The camera function is invoked with X and Y arguments in the range [0, 1[.
  /// Uses Array.Parallel to distribute work. Map component values are clamped to [-1, 1]
  /// and transformed to the unit range. Optionally, a callback (which must be reentrant)
  /// is invoked after each row is rendered.
  static member createFrom(map : Map3, width, height, camera : float32 -> float32 -> Vec3f, ?callback : (int -> unit)) =
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

  let bitmapSource = BitmapSource.createFrom(map, size, size, fun x y -> Vec3f(x, y, 0.0f))

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
      let pixmap = Pixmap.createFrom(map, width, height, camera, callback)
      pixmap.savePng(dialog.FileName)
      Wpf.dispatch(progressWindow, fun _ -> progressWindow.Close())
    )


