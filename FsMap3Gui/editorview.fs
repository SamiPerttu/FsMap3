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


type EditorViewMode = FullView | HalfView | QuarterView



type EditorMutationMode = Everything | ColorsEffects | ScalesOffsets | Details | Random



type EditorTool = PanTool | PanZoomTool | ZoomTool | MutateTool | JoltTool



type View =
  static member transform(?centerX : ParameterAction, ?centerY : ParameterAction, ?centerZ : ParameterAction, ?zoom : ParameterAction) =
    fun (rnd : Rnd) (dna : Dna) i ->
      match dna.[i].name, centerX, centerY, centerZ, zoom with
      | "View Center X", Some(centerX), _, _, _ -> centerX
      | "View Center Y", _, Some(centerY), _, _ -> centerY
      | "View Center Z", _, _, Some(centerZ), _ -> centerZ
      | "View Zoom", _, _, _, Some(zoom) -> zoom
      | _ -> Retain



[<NoComparison; NoEquality>]
type EditorView<'a> =
  {
    mainMode : EditorViewMode
    gridI : int
    gridX : int
    gridY : int
    image : Image
    busyImage : Image
    mutable busyCycle : int
    busyTimer : Threading.DispatcherTimer
    view : PixmapView
    mutable filename : string
    mutable presetFilename : string
    controller : 'a PixmapController
    mutable focusShape : Rectangle option
    /// View center delta for rapid panning actions.
    panDelta : Atom.Shared<Vec3f>
    /// Zoom factor for rapid zooming actions.
    zoomFactor : Atom.Shared<float32>
    /// The agent serializes user actions with respect to this view.
    mutable agent : (unit -> bool) Agent
  }

  member this.stop() =
    this.agent.Post(always false)
    this.view.stop()

  member this.post(f : unit -> unit) =
    this.agent.Post(fun _ -> f(); true)

  member this.wake() =
    this.post(ignore)

  member this.wakeAndPost(f) =
    this.wake()
    this.post(f)

  member this.isEmpty =
    !this.controller.deep === this.controller.deepSeed

  member this.pixmapSource =
    !this.controller.pixmapSource

  member this.idle() =
    //Log.infof "Idle %d %d %d" this.gridI this.gridX this.gridY
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
    //Log.infof "Busy %d %d %d" this.gridI this.gridX this.gridY
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

  static member create(mainMode, gridI, gridX, gridY, deepSeed : 'a, deepGenerator, pixmapGenerator, deepFilter, grid : Grid, visible : bool, previewLevels) : 'a EditorView =
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
        view = view
        filename = ""
        presetFilename = ""
        controller = PixmapController.create(view, deepSeed, deepGenerator, pixmapGenerator, deepFilter)
        focusShape = None
        panDelta = Atom.Shared(Vec3f.zero)
        zoomFactor = Atom.Shared(1.0f)
        agent = nullRef
      }
    this.agent <- Agent.Start(this.agentFunction)
    this.busyTimer.Tick.Add(fun _ -> this.cycleBusy())
    this.controller.workCallback <- this.busy
    view.idleCallback <- this.idle
    this

  member this.createFocusShape(grid : Grid) =
    let shape = Rectangle(Visibility = Visibility.Hidden, IsHitTestVisible = false, Stroke = Wpf.brush(0.75), Opacity = 1.0, StrokeThickness = 2.0, Fill = Brushes.Transparent, SnapsToDevicePixels = true)
    this.focusShape <- Some(shape)
    grid.add(shape, 0, 0)
    shape.Margin <- Thickness(0.0)
    shape.HorizontalAlignment <- HorizontalAlignment.Left
    shape.VerticalAlignment <- VerticalAlignment.Top

  member this.agentFunction(inbox) =
    async {
      let mutable alive = true
      while alive do
        let panDelta = this.panDelta.pre(always Vec3f.zero)
        let zoomFactor = this.zoomFactor.pre(always 1.0f)
        // Pan or zoom the view if requested; otherwise, wait for the next message.
        if panDelta.length2 > 0.0f || zoomFactor <> 1.0f then
          this.controller.alter(View.transform(centerX = ModifyFloat (fun x -> x + float panDelta.x),
                                               centerY = ModifyFloat (fun y -> y + float panDelta.y),
                                               centerZ = ModifyFloat (fun z -> z + float panDelta.z),
                                               zoom = ModifyFloat (fun zoom -> zoom * float zoomFactor)))
          // Process one message or wait a short time before checking pan and zoom again.
          let! msg = inbox.TryReceive(20)
          alive <- msg.map(fun msg -> msg()) >? alive
        else
          let! msg = inbox.Receive()
          alive <- msg()
    }



type View with

  static member shouldRetainAlways(name) =
    name = "Layout" || name.StartsWith("View")


  static member mutationPredicate(rnd : Rnd, view : EditorView<_>, mutateMode : EditorMutationMode) =

    let mutateMode = match mutateMode with | Random -> rnd.choose(1.0, ColorsEffects, 1.0, ScalesOffsets, 1.0, Details, 1.0, Everything) | x -> x

    // Half view has a higher mutation rate than full and mosaic views.
    let mR = match view.mainMode with
              | HalfView -> rnd.exp(0.5, 2.0)
              | _ -> rnd.exp(0.25, 0.5)

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
        elif name = "Shape" || parentName = "Shape" || parentName = "Unary op" then
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


  static member mosaicPredicate(rnd : Rnd, dna : Dna, view : EditorView<_>, mutateMode : EditorMutationMode) =

    let mutation = View.mutationPredicate(rnd, view, mutateMode)

    // Target number of parameters to mutate at once.
    let mosaicParameters = rnd.int(2, 8)
    let mutationParameters = 0 // rnd.int(1, 3)

    let mosaicPredicateSet = Darray.create()
    let mutationPredicateSet = Darray.create()

    let addPredicates tryHard =
      for i = 0 to dna.last do
        let name = dna.[i].name
        if dna.[i].format = Ordered && dna.[i].maxValue > 0u && name.Contains("requency") = false && name <> "X offset" && name <> "Y offset" && name <> "Z offset" && View.shouldRetainAlways(name) = false && name <> "Lacunarity" then
          let range = rnd.exp(0.1, 1.0)
          let start = clamp 0.0 (1.0 - range) (dna.[i].value01 - range * rnd.float(0.2, 0.8))
          let minValue, maxValue = (start, start + range) |> if rnd.boolean(0.5) then id else rev
          match mutation rnd dna i with
          | Retain -> if tryHard then mosaicPredicateSet.add((i, minValue, maxValue))
          | _ -> mosaicPredicateSet.add((i, minValue, maxValue))
        else
          match mutation rnd dna i with
          | Retain -> ()
          | x -> mutationPredicateSet.add(i, x)

    addPredicates false
    if mosaicPredicateSet.size < mosaicParameters then addPredicates false
    if mosaicPredicateSet.size = 0 then addPredicates true

    if mosaicPredicateSet.size > mosaicParameters then
      mosaicPredicateSet.copyFrom(rnd.shuffle(mosaicPredicateSet.toArray))
      mosaicPredicateSet.resize(mosaicParameters)

    if mutationPredicateSet.size > mutationParameters then
      mutationPredicateSet.copyFrom(rnd.shuffle(mutationPredicateSet.toArray))
      mutationPredicateSet.resize(mutationParameters)

    fun applyMutations x (rnd : Rnd) (dna : Dna) i ->
      match Fun.findArg 0 mosaicPredicateSet.last (fun j -> fst3 mosaicPredicateSet.[j] = i), Fun.findArg 0 mutationPredicateSet.last (fun j -> fst mutationPredicateSet.[j] = i) with
      | Someval(j), _ ->
        let _, minDelta, maxDelta = mosaicPredicateSet.[j] in Select01 (lerp minDelta maxDelta x)
      | _, Someval(j) ->
        if applyMutations then snd mutationPredicateSet.[j] else Retain
      | _ ->
        Retain

