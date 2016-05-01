// Windows Presentation Foundation utilities and extensions.
namespace FsMap3

open System
open System.Windows
open System.Windows.Media
open System.Windows.Shapes
open System.Windows.Controls

open Common


/// Windows Presentation Foundation wrappers and utilities.
type Wpf =

  /// Returns a Windows color, given RGB values in [0, 1].
  static member color(R, G, B) = Color.FromRgb(byte (clamp01 R * 255.9), byte (clamp01 G * 255.9), byte (clamp01 B * 255.9))

  /// Returns a Windows color, given a grayscale value in [0, 1].
  static member color(V) = Wpf.color(V, V, V)

  /// Returns a solid color brush, given RGB values in [0, 1].
  static member brush(R, G, B) = SolidColorBrush(Wpf.color(R, G, B))

  /// Returns a solid color brush, given a grayscale value in [0, 1].
  static member brush(V) = Wpf.brush(V, V, V)

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




