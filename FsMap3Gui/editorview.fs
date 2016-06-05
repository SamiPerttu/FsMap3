namespace FsMap3

open System
open System.Windows
open System.Windows.Media
open System.Windows.Shapes
open System.Windows.Controls
open System.Windows.Controls.Primitives

open Common
open Basis3
open Map3
open Map3Info
open Map3Dna
open Map3Gui


type EditorViewMode = FullView | HalfView | ThirdView



type EditorProjectionMode = Flat | Sphere



type EditorMutationMode = Everything | ColorsEffects | ScalesOffsets | Details | Random



type EditorTool = PanTool | PanZoomTool | ZoomTool | MutateTool | JoltTool



type [<NoComparison; NoEquality>] ViewEditState =
  {
    genotype : Dna
    filename : string
    presetFilename : string
  }



type [<NoComparison; NoEquality>] EditorView<'a> =
  {
    mainMode : EditorViewMode
    gridI : int
    gridX : int
    gridY : int
    image : Image
    busyImage : Image
    mutable busyCycle : int
    busyTimer : Threading.DispatcherTimer
    pixmapView : PixmapView
    mutable filename : string
    mutable presetFilename : string
    controller : 'a PixmapController
    mutable focusShape : Rectangle[] option
  }

  member this.stop() =
    this.pixmapView.stop()

  member this.isEmpty =
    !this.controller.deep === this.controller.deepSeed

  member this.pixmapSource =
    !this.controller.pixmapSource

  member this.idle() =
    Wpf.dispatch(this.busyImage, fun _ ->
      this.busyImage.Visibility <- Visibility.Collapsed
      this.busyTimer.Stop()
      )

  member this.cycleBusy() =
    Wpf.dispatch(this.busyImage, fun _ ->
      this.busyCycle <- (this.busyCycle + 1) % BusyImage.busyBitmaps.size
      this.busyImage.Source <- BusyImage.busyBitmaps.[this.busyCycle]
      )

  member this.busy() =
    Wpf.dispatch(this.busyImage, fun _ ->
      this.busyImage.Visibility <- Visibility.Visible
      this.busyTimer.Start()
      )

  member this.layoutBusy() =
    let busyBorder = 4.0
    this.busyImage.Margin <- Thickness(Left = this.image.Margin.Left + this.image.Width - this.busyImage.Width - busyBorder,
                                       Top = this.image.Margin.Top + busyBorder)

  member this.setVisibility(visibility) =
    this.image.Visibility <- visibility
    if visibility = Visibility.Collapsed then this.idle()

  member this.focus(visibility) =
    this.focusShape.apply(fun focusShape -> for shape in focusShape do shape.Visibility <- visibility)

  static member create(mainMode, gridI, gridX, gridY, pixmapSeed, deepSeed, deepGenerator, pixmapGenerator, deepFilter, grid : Grid, visible : bool, previewLevels) : 'a EditorView =
    let image = Image(SnapsToDevicePixels = true, Margin = Thickness(0.0), HorizontalAlignment = HorizontalAlignment.Left, VerticalAlignment = VerticalAlignment.Top, Visibility = match visible with | true -> Visibility.Visible | false -> Visibility.Collapsed)
    let view = PixmapView(image, previewLevels = previewLevels)
    grid.add(image, 0, 0)
    let busySource = BusyImage.busyBitmaps.[0]
    let busyImage = Image(SnapsToDevicePixels = true, Opacity = 0.5, Margin = Thickness(0.0), HorizontalAlignment = HorizontalAlignment.Left, VerticalAlignment = VerticalAlignment.Top, Visibility = Visibility.Collapsed, Width = float busySource.PixelWidth, Height = float busySource.PixelHeight, Source = busySource)
    grid.add(busyImage, 0, 0)
    let this = 
      {
        mainMode = mainMode
        gridI = gridI
        gridX = gridX
        gridY = gridY
        image = image
        busyImage = busyImage
        busyCycle = 0
        busyTimer = Threading.DispatcherTimer(Interval = TimeSpan.FromMilliseconds(200.0))
        pixmapView = view
        filename = ""
        presetFilename = ""
        controller = PixmapController.create(view, pixmapSeed, deepSeed, deepGenerator, pixmapGenerator, deepFilter)
        focusShape = None
      }
    this.busyTimer.Tick.Add(fun _ -> this.cycleBusy())
    this.controller.workCallback <- this.busy
    view.idleCallback <- this.idle
    this

  member this.createFocusShape(grid : Grid) =
    let shape1 = Rectangle(Visibility = Visibility.Hidden, Margin = Thickness(0.0), HorizontalAlignment = HorizontalAlignment.Left, VerticalAlignment = VerticalAlignment.Top, IsHitTestVisible = false, Stroke = Wpf.brush(0.0), Opacity = 1.0, StrokeThickness = 1.0, Fill = Brushes.Transparent, SnapsToDevicePixels = true)
    let shape2 = Rectangle(Visibility = Visibility.Hidden, Margin = Thickness(0.0), HorizontalAlignment = HorizontalAlignment.Left, VerticalAlignment = VerticalAlignment.Top, IsHitTestVisible = false, Stroke = Wpf.brush(0.8), Opacity = 1.0, StrokeThickness = 2.0, Fill = Brushes.Transparent, SnapsToDevicePixels = true)
    this.focusShape <- Some([| shape1; shape2 |])
    grid.add(shape1, 0, 0)
    grid.add(shape2, 0, 0)

  member this.editState =
    {
      ViewEditState.genotype = !this.controller.dna
      filename = this.filename
      presetFilename = this.presetFilename
    }



type View =

  static member shouldRetainAlways(name) =
    name = "Layout" || name.StartsWith("View")


  /// Creates a view transform predicate.
  static member transform(?centerX : ParameterAction, ?centerY : ParameterAction, ?centerZ : ParameterAction, ?zoom : ParameterAction) =
    fun (rnd : Rnd) (dna : Dna) i ->
      match dna.[i].name, centerX, centerY, centerZ, zoom with
      | "View Center X", Some(centerX), _, _, _ -> centerX
      | "View Center Y", _, Some(centerY), _, _ -> centerY
      | "View Center Z", _, _, Some(centerZ), _ -> centerZ
      | "View Zoom", _, _, _, Some(zoom) -> zoom
      | _ -> Retain


  /// Creates a mutation predicate from the mutation mode.
  static member mutationPredicate(rnd : Rnd, view : EditorView<_>, mutateMode : EditorMutationMode) =

    let mutateMode = match mutateMode with | Random -> rnd.choose(1.0, ColorsEffects, 1.0, ScalesOffsets, 1.0, Details, 1.0, Everything) | x -> x

    // Full view has a lower mutation rate.
    let mR = match view.mainMode with
             | FullView -> rnd.exp(0.25, 0.5)
             | _ -> rnd.exp(0.5, 2.0)

    match mutateMode with

    | ColorsEffects ->
      fun (rnd : Rnd) (dna : Dna) i ->
        let name = dna.[i].name
        let parentName = dna.parentParameter(i).map(fun p -> p.name) >? ""
        if View.shouldRetainAlways name then
          Retain
        elif name = "Color space" then
          rnd.choose(2.0, Retain, 1.0 * mR, Randomize)
        elif name.StartsWith("Color") || name.StartsWith("Hue") || name.StartsWith("Saturation") || name = "Value skew" then
          Jolt01(rnd.exp(0.01, 1.0))
        elif name = "Shape" || parentName = "Shape" then
          rnd.choose(2.0, Retain, 1.0 * mR, Jolt01(rnd.exp(0.01, 1.0)))
        elif parentName = "Unary op" && name <> "Node" then
          rnd.choose(2.0, Retain, 1.0 * mR, Jolt01(rnd.exp(0.01, 1.0)))
        elif name = "Shading" || name = "Cell color" || parentName = "Cell color" then
          rnd.choose(2.0, Retain, 1.0 * mR, Jolt01(rnd.exp(0.01, 1.0)))
        else Retain

    | ScalesOffsets ->
      fun rnd (dna : Dna) i ->
        let name = dna.[i].name
        if View.shouldRetainAlways name then
          Retain
        elif name = "X offset" || name = "Y offset" || name = "Z offset" then
          rnd.choose(1.5, Retain, 1.0 * mR, Jolt01(rnd.exp(0.01, 1.0)))
        elif name = "Frequency" || name = "Frequency factor" || name = "Flow frequency" then
          rnd.choose(2.0, Retain, 1.0 * mR, Jolt01(rnd.exp(0.01, 1.0)))
        elif name = "Lacunarity" then
          rnd.choose(2.0, Retain, 1.0 * mR, Jolt01(rnd.exp(0.01, 1.0)))
        else Retain

    | Details ->
      fun rnd (dna : Dna) i ->
        let name = dna.[i].name
        let parentName = dna.parentParameter(i).map(fun p -> p.name) >? ""
        if View.shouldRetainAlways name then
          Retain
        elif name = "Roughness" then
          rnd.choose(2.0, Retain, 1.0 * mR, Jolt01(rnd.exp(0.05, 0.5)))
        elif name = "Octaves" then
          rnd.choose(2.0, Retain, 1.0 * mR, Jolt01(rnd.exp(0.05, 0.3)))
        elif name = "Curvature" then
          rnd.choose(2.0, Retain, 1.0 * mR, Jolt01(rnd.exp(0.01, 1.0)))
        elif name = "Walk operator" || parentName = "Mix operator" || parentName = "Features per cell" || parentName = "Potential" then
          rnd.choose(2.0, Retain, 1.0 * mR, Jolt01(rnd.exp(0.01, 1.0)))
        elif name.EndsWith("amount") || name.EndsWith("hardness") || name.EndsWith("width") || name.EndsWith("filter") || name.EndsWith("fade") || name.EndsWith("radius") || name.EndsWith("length") || name.EndsWith("variability") || name.EndsWith("power") then
          rnd.choose(2.0, Retain, 1.0 * mR, Jolt01(rnd.exp(0.01, 1.0)))
        else Retain

    | Everything | _ ->
      let mR = mR * rnd.exp(0.01, 1.0)
      fun rnd (dna : Dna) i ->
        let name = dna.[i].name
        if View.shouldRetainAlways name then
          Retain
        elif rnd.boolean(mR) then
          Jolt01(rnd.exp(0.01, 1.0))
        else
          Retain

