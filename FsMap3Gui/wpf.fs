// Windows Presentation Foundation utilities and extensions.
namespace FsMap3

open System
open System.Windows
open System.Windows.Media
open System.Windows.Media.Imaging
open System.Windows.Shapes
open System.Windows.Controls
open System.Windows.Controls.Primitives

open Common
open Map3


/// Windows Presentation Foundation wrappers and utilities.
type Wpf =

  /// Windows color with RGB values in [0, 1].
  static member color(R, G, B) =
    let value8 x = clamp01 x * 255.99 |> byte
    Color.FromRgb(value8 R, value8 G, value8 B)

  /// Windows color with RGBA values in [0, 1].
  static member color(R, G, B, A) =
    let value8 x = clamp01 x * 255.99 |> byte
    Color.FromArgb(value8 A, value8 R, value8 G, value8 B)

  /// Grayscale windows color with value in [0, 1].
  static member color(V) = Wpf.color(V, V, V)

  /// Solid color brush with RGB values in [0, 1].
  static member brush(R, G, B) = SolidColorBrush(Wpf.color(R, G, B))

  /// Solid color brush with RGBA values in [0, 1].
  static member brush(R, G, B, A) = SolidColorBrush(Wpf.color(R, G, B, A))

  /// Returns a solid color brush, given a grayscale value in [0, 1].
  static member brush(V) = Wpf.brush(V, V, V)

  /// Vertical gradient brush.
  static member verticalBrush(topColor, bottomColor, ?height, ?center) =
    let height = height >? 1.0
    let center = center >? 0.5
    LinearGradientBrush(topColor, bottomColor, Point(0.5, center - height * 0.5), Point(0.5, center + height * 0.5))

  /// Dispatches a task to the UI thread of the given element. If we are that thread, the task
  /// is executed immediately; otherwise it is scheduled to be executed asynchronously.
  static member dispatch(element : Threading.DispatcherObject, task : unit -> unit) =
    if element.CheckAccess() then
      task()
    else
      element.Dispatcher.InvokeAsync(task) |> ignore

  /// Sends a task to the UI thread of the given element. Returns a handle that can be
  /// used to monitor the task.
  static member getDispatch(element : Threading.DispatcherObject, task : unit -> unit) =
    element.Dispatcher.InvokeAsync(task)

  /// Executes a task in the UI thread of the given element. If we are that thread, the task
  /// is executed immediately; otherwise we wait until the thread executes it.
  static member execute(element : Threading.DispatcherObject, task : unit -> _) =
    if element.CheckAccess() then
      task()
    else
      element.Dispatcher.Invoke(task)


  /// Loads a bitmap image and embeds it in an Image control, which is returned.
  static member loadImage(filename : string) =
    let image = Image()
    let src = Imaging.BitmapImage()
    src.BeginInit()
    src.UriSource <- Uri(filename, UriKind.Relative)
    src.CacheOption <- Imaging.BitmapCacheOption.OnLoad
    src.EndInit()
    image.Source <- src
    image.Stretch <- Stretch.Uniform
    image.Margin <- Thickness(0.0)
    image



[<AutoOpen>]
module WpfExtensions =

  type FrameworkElement with
    member this.withToolTip with set(tip : string) = this.ToolTip <- Label(Content = tip)

    member this.setPixelUnits() =
      let setTransform = lazy (this.LayoutTransform <- MatrixTransform(PresentationSource.FromVisual(this).CompositionTarget.TransformFromDevice))
      this.Loaded.Add(fun _ -> !setTransform)


  type UIElement with
    member this.withPreviewMouseDown with set(f) = this.PreviewMouseDown.Add(f)


  type Primitives.ButtonBase with
    member this.withClick with set(f) = this.Click.Add(f)


  type Control with
    member this.scaleFont(factor) = this.FontSize <- this.FontSize * factor


  type Window with
    member this.setPixelContent(element : FrameworkElement) =
      this.Content <- element
      let setTransform = lazy (element.LayoutTransform <- MatrixTransform(PresentationSource.FromVisual(this).CompositionTarget.TransformFromDevice))
      this.Loaded.Add(fun _ -> !setTransform)


  type Panel with
    member this.add(child) = ignore <| this.Children.Add(child)


  type Point with
    member this.vec2f = Vec2f(float32 this.X, float32 this.Y)


  type Grid with
    member this.add(child, column, row, ?columnSpan, ?rowSpan) =
      Grid.SetColumn(child, column)
      Grid.SetRow(child, row)
      Grid.SetColumnSpan(child, columnSpan >? 1)
      Grid.SetRowSpan(child, rowSpan >? 1)
      this.Children.Add(child) |> ignore


  type UniformGrid with
    member this.add(child, column, row) =
      Grid.SetColumn(child, column)
      Grid.SetRow(child, row)
      this.Children.Add(child) |> ignore


  type Canvas with
    member this.add(child, x, y) =
      Canvas.SetLeft(child, x)
      Canvas.SetTop(child, y)
      this.Children.Add(child) |> ignore


  type DockPanel with
    member this.add(child, dock) =
      DockPanel.SetDock(child, dock)
      this.Children.Add(child) |> ignore


  type ItemsControl with
    member this.add(child) =
      this.Items.Add(child) |> ignore


  type ComboBoxItem with
    member this.withSelected with set(x) = this.Selected.Add(x)

  
  type BitmapSource with
    /// Converts a Map3 to a BitmapSource, which is also frozen. The camera function is invoked
    /// with X and Y arguments in [0, 1[. Computes the bitmap in parallel using the .NET thread pool.
    /// Map component values are clamped to [-1, 1] and transformed to unit range.
    /// Optionally, a (reentrant) callback is invoked after each row is rendered.
    static member createFrom(map : Map3, width, height, camera : float32 -> float32 -> Vec3f, ?callback : (int -> unit)) =
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
      let source = BitmapSource.Create(width, height, 96.0, 96.0, System.Windows.Media.PixelFormats.Rgb24, null, pixelArray, stride)
      source.Freeze()
      source


  type ImageBrush with
    /// Creates a tiling image brush from a Map3. If width <> height, then only the larger dimension can tile.
    /// The viewport is the size of the brush scaled by pixelScale.
    static member createFrom(map : Map3, width, height, pixelScale) =
      let Z = 1.0f / (max width height |> float32)
      let Zx, Zy = float32 width * Z, float32 height * Z
      let bitmapSource = BitmapSource.createFrom(map, width, height, fun x y -> Vec3f(x * Zx, y * Zy, 0.0f))
      ImageBrush(bitmapSource, TileMode = TileMode.Tile, Viewport = Rect(0.0, 0.0, float width * pixelScale, float height * pixelScale), ViewportUnits = BrushMappingMode.Absolute)

