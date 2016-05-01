/// Explorer GUI.
module FsMap3.Explorer

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
open Map3Gui


type ExplorerViewMode = Full | Center | Half | Quarter



type ExplorerMutateMode = Everything | ColorsEffects | ScalesOffsets | Details | Random



type ExplorerTool = ZoomTool | MutateTool | PanTool



[<NoComparison; NoEquality>]
type ExplorerView<'a> =
  {
    mainMode : ExplorerViewMode
    image : Image
    view : PixmapView
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

  static member create(mainMode, deepSeed : 'a, deepGenerator, pixmapGenerator, deepFilter, grid : Grid, visible : bool, previewLevels) : 'a ExplorerView =
    let image = Image(Visibility = match visible with | true -> Visibility.Visible | false -> Visibility.Collapsed)
    let view = PixmapView(image, previewLevels = previewLevels)
    grid.add(image, 0, 0)
    image.Margin <- Thickness(0.0)
    image.HorizontalAlignment <- HorizontalAlignment.Left
    image.VerticalAlignment <- VerticalAlignment.Top
    let this = 
      {
        mainMode = mainMode
        image = image
        view = view
        controller = PixmapController.create(view, deepSeed, deepGenerator, pixmapGenerator, deepFilter)
        focusShape = None
        panDelta = Atom.Shared(Vec3f.zero)
        zoomFactor = Atom.Shared(1.0f)
        agent = nullRef
      }
    this.agent <- Agent.Start(this.agentFunction)
    this

  member this.agentFunction(inbox) =
    async {
      let mutable alive = true
      while alive do
        let panDelta = this.panDelta.pre(always Vec3f.zero)
        let zoomFactor = this.zoomFactor.pre(always 1.0f)
        // Pan or zoom the view if requested.
        if panDelta.length2 > 0.0f || zoomFactor <> 1.0f then
          this.controller.alter(fun _ dna i ->
            if dna.[i].name = "View Center X" then ModifyFloat (fun x -> x + float panDelta.x)
            elif dna.[i].name = "View Center Y" then ModifyFloat (fun y -> y + float panDelta.y)
            elif dna.[i].name = "View Center Z" then ModifyFloat (fun z -> z + float panDelta.z)
            elif dna.[i].name = "View Zoom" then ModifyFloat (fun zoom -> zoom * float zoomFactor)
            else Retain
            )
        else
          // Otherwise, process the next message.
          let! msg = inbox.Receive()
          alive <- msg()
    }

  member this.createFocusShape(grid : Grid) =
    let shape = Rectangle(Visibility = Visibility.Hidden, IsHitTestVisible = false, Stroke = Wpf.brush(0.75), Opacity = 1.0, StrokeThickness = 2.0, Fill = Brushes.Transparent, SnapsToDevicePixels = true)
    this.focusShape <- Some(shape)
    grid.add(shape, 0, 0)
    shape.Margin <- Thickness(0.0)
    shape.HorizontalAlignment <- HorizontalAlignment.Left
    shape.VerticalAlignment <- VerticalAlignment.Top




[<NoComparison; NoEquality>]
type UserAction =
  | Zooming of view : ExplorerView<RichMap3> * source : Vec2f
  | Panning of view : ExplorerView<RichMap3> * source : Vec2f
  | Idle



[<NoComparison; NoEquality>]
type StatusLine =
  {
    statusPanel : StackPanel
    statusSymbol : Image
    statusMessage : TextBlock
  }



/// Interactive Map3 explorer.
type Explorer =

  static member start(?initialR) =

    /// Minimum resolution of each quarter view.
    let minimumR = 16
    /// Initial resolution of each quarter view. Half view is double this. Full view is four times this.
    let initialR = initialR >? 160
    /// View border thickness.
    let viewBorder = 2
    /// How many pixels user has to move the mouse before it is recognized as zooming.
    let dragMinimum = 16.0f
    /// Width of tool bar panel.
    let toolPanelWidth = 140.0

    // We have 1 full view, 1 center half view, 4 half views, and 16 quarter views.
    let fN = 1
    let cN = 1
    let hN = 4
    let qN = 16

    let rnd = Rnd(timeSeed())

    let initialCanvasWidth = initialR * 4 + viewBorder * 2
    let initialCanvasHeight = initialCanvasWidth

    let currentCanvasWidth = ref 0
    let currentCanvasHeight = ref 0

    let window = Window(Title = "Map3 Explorer", ResizeMode = ResizeMode.CanResize, SizeToContent = SizeToContent.Manual, Topmost = false, WindowStartupLocation = WindowStartupLocation.CenterScreen)

    let canvas = Grid(Background = Brushes.Black, ClipToBounds = true, Margin = Thickness(0.0, 0.0, 0.0, 0.0))
    canvas.ColumnDefinitions.Add(ColumnDefinition())
    canvas.RowDefinitions.Add(RowDefinition())
    canvas.HorizontalAlignment <- HorizontalAlignment.Stretch
    canvas.VerticalAlignment <- VerticalAlignment.Stretch

    let mapSeed = Map3.zero
    let richSeed = { RichMap3.map = mapSeed; center = Vec3f(0.5f); zoom = 1.0f; aspectRatio = 1.0f; info = Map3Info.create(mapSeed) }
    let deepGenerator = RichMap3.generate generateExplorerMap
    let pixmapGenerator = RichMap3.pixmapGenerator
    let deepFilter = RichMap3.filterDetail

    let fullView = ExplorerView.create(Full, richSeed, deepGenerator, pixmapGenerator, deepFilter, canvas, true, 4)

    let centerView = ExplorerView.create(Center, richSeed, deepGenerator, pixmapGenerator, deepFilter, canvas, false, 4)

    let halfView = Array.init hN (fun i ->
      ExplorerView.create(Half, richSeed, deepGenerator, pixmapGenerator, deepFilter, canvas, false, 4)
      )

    let quarterView = Array.init qN (fun i ->
      ExplorerView.create(Quarter, richSeed, deepGenerator, pixmapGenerator, deepFilter, canvas, false, 3)
      )

    for view in halfView do
      view.createFocusShape(canvas)

    let dragShape = Rectangle(Visibility = Visibility.Collapsed, Stroke = Wpf.brush(0.0), Opacity = 0.3, StrokeThickness = 1.0, Fill = Brushes.Blue, SnapsToDevicePixels = true)
    canvas.add(dragShape, 0, 0)
    dragShape.Margin <- Thickness(0.0)
    dragShape.HorizontalAlignment <- HorizontalAlignment.Left
    dragShape.VerticalAlignment <- VerticalAlignment.Top

    // This must be called from the UI thread.
    let layoutCanvas() =

      let canvasWidth = int canvas.ActualWidth
      let canvasHeight = int canvas.ActualHeight

      if canvasWidth <> !currentCanvasWidth || canvasHeight <> !currentCanvasHeight then

        currentCanvasWidth := canvasWidth
        currentCanvasHeight := canvasHeight

        let R = max minimumR ((min canvasWidth canvasHeight - 2 * viewBorder) / 4)

        let totalSize = R * 4 + 2 * viewBorder
        let x0 = max 0 (int canvas.ActualWidth - totalSize) / 2
        let y0 = max 0 (int canvas.ActualHeight - totalSize) / 2

        Log.infof "Laying out view canvas. Quarter view resolution = %d pixels." R

        fullView.image.Width <- float (R * 4)
        fullView.image.Height <- float (R * 4)
        fullView.image.Margin <- Thickness(Left = float (x0 + viewBorder), Top = float (y0 + viewBorder))
        fullView.view.renderWidth.set(R * 4)
        fullView.view.renderHeight.set(R * 4)

        centerView.image.Width <- float (R * 2)
        centerView.image.Height <- float (R * 2)
        centerView.image.Margin <- Thickness(Left = float (x0 + R + viewBorder), Top = float (y0 + R + viewBorder))
        centerView.view.renderWidth.set(R * 2)
        centerView.view.renderHeight.set(R * 2)

        halfView |> Array.iteri (fun i view ->
          let image = view.image
          image.Width <- float (R * 2)
          image.Height <- float (R * 2)
          let x = i % 2
          let y = i / 2
          image.Margin <- Thickness(Left = float (x0 + x * R * 2 + viewBorder), Top = float (y0 + y * R * 2 + viewBorder))
          let shape = !view.focusShape
          shape.Width <- float ((R + viewBorder) * 2)
          shape.Height <- float ((R + viewBorder) * 2)
          let x = i % 2
          let y = i / 2
          shape.Margin <- Thickness(Left = float (x0 + x * R * 2), Top = float (y0 + y * R * 2))
          view.view.renderWidth.set(R * 2)
          view.view.renderHeight.set(R * 2)
          )

        quarterView |> Array.iteri (fun i view ->
          let image = view.image
          image.Width <- float R
          image.Height <- float R
          let x = i % 4
          let y = i / 4
          image.Margin <- Thickness(Left = float (x0 + x * (R + 1)), Top = float (y0 + y * (R + 1)))
          view.view.renderWidth.set(R)
          view.view.renderHeight.set(R)
          )
     
    let toolPanel = StackPanel(Orientation = Orientation.Vertical, Width = toolPanelWidth)
    toolPanel.VerticalAlignment <- VerticalAlignment.Top

    /// Current user action. Access from UI thread only.
    let userAction = ref Idle

    /// Which PixmapViews are visible depends on the mode.
    let guiMode = ref ExplorerViewMode.Full

    let toolMode = ref ExplorerTool.MutateTool

    let layoutMode = ref Layout.Hifi

    let toolbar = StackPanel(Orientation = Orientation.Horizontal)
    let toolSize = 26.0
    let toolMargin = Thickness(1.0)
    let createToolButton imageFile = ToggleButton(Content = Wpf.loadImage(imageFile), Width = toolSize, Height = toolSize, Margin = toolMargin)
    let mutateButton = createToolButton "appbar.diagram.png"
    toolbar.add(mutateButton)
    let zoomButton = createToolButton "appbar.magnify.png"
    toolbar.add(zoomButton)
    let panButton = createToolButton "appbar.cursor.move.png"
    toolbar.add(panButton)

    let setToolMode mode =
      mutateButton.IsChecked <- Nullable(false)
      zoomButton.IsChecked <- Nullable(false)
      panButton.IsChecked <- Nullable(false)
      match mode with
      | MutateTool -> mutateButton.IsChecked <- Nullable(true)
      | ZoomTool -> zoomButton.IsChecked <- Nullable(true)
      | PanTool -> panButton.IsChecked <- Nullable(true)
      toolMode := mode

    mutateButton.PreviewMouseDown.Add(fun args -> setToolMode MutateTool; args.Handled <- true)
    zoomButton.PreviewMouseDown.Add(fun args -> setToolMode ZoomTool; args.Handled <- true)
    panButton.PreviewMouseDown.Add(fun args -> setToolMode PanTool; args.Handled <- true)

    /// Which (half) view did we maximize last?
    let minimizeView = ref halfView.[0]

    let mutateMode = ref Everything

    let focusView = Atom.Shared(Some(fullView))
    
    let iterateViews (f : ExplorerView<_> -> bool -> unit) =
      match !guiMode with
      | Full ->
        f fullView true
        f centerView false
        for view in halfView do f view false
        for view in quarterView do f view false
      | Center ->
        f fullView false
        f centerView true
        for view in halfView do f view false
        for i = 0 to qN - 1 do
          let x = i % 4
          let y = i / 4
          f quarterView.[i] (min x y > 0 && max x y < 3)
      | Half ->
        f fullView false
        f centerView false
        for view in halfView do f view true
        for view in quarterView do f view false
      | Quarter ->
        f fullView false
        f centerView false
        for view in halfView do f view false
        for view in quarterView do f view true

    let dnaView = DnaView(viewFilter = fun parameter ->
      if parameter.name = "Layout" && parameter.level > 0 then
        Hidden
      elif parameter.name = "View Zoom" || parameter.name.StartsWith("View Center") then
        Hidden
      else Editable
      )

    /// Updates the Dna view.
    let updateDna() =
      match !focusView with
      | Some(view) ->
        let dna = Dna.createCopy(!view.controller.dna)
        Wpf.dispatch(window, fun _ -> dnaView.update(dna))
      | None ->
        Wpf.dispatch(window, fun _ -> dnaView.reset())

    /// Sets focus to the view and displays its Dna.
    let setFocus view' =
      if (!focusView).isNoneOr((<>=) view') then
        focusView.set(Some(view'))
        Wpf.dispatch(window, fun _ ->
          for view in halfView do
            (!view.focusShape).Visibility <- if view' === view then Visibility.Visible else Visibility.Hidden
          )
      updateDna()

    /// Clears focus and resets the Dna view.
    let clearFocus() =
      focusView.set(None)
      updateDna()
      Wpf.dispatch(window, fun _ ->
        for view in halfView do
          (!view.focusShape).Visibility <- Visibility.Hidden
        )

    let randomizePredicate _ (dna : Dna) i =
      if dna.[i].name = "View Zoom" then SelectFloat 1.0
      elif dna.[i].name.StartsWith("View Center") then SelectFloat 0.5
      elif dna.[i].name = "Layout" then Select (layoutChoices.numberOf((=) !layoutMode))
      else Randomize

    /// Randomizes all visible views.
    let randomizeAll() =
      clearFocus()
      scheduleTask(fun _ ->
        iterateViews(fun view visible ->
          if visible then view.controller.restart(randomizePredicate)
        )
      )

    let setGuiMode(mode') =
      if !guiMode <> mode' then
        guiMode := mode'
        iterateViews (fun view visible -> view.image.Visibility <- if visible then Visibility.Visible else Visibility.Collapsed)

    dnaView.leftCallback <- fun i value ->
      match !focusView with
      | Some(view) ->
        view.post(fun _ ->
          view.controller.setValue(i, Some(value))
          updateDna()
          )
      | None -> ()
    dnaView.rightCallback <- fun i ->
      match !focusView with
      | Some(view) ->
        view.post(fun _ ->
          view.controller.setValue(i, None)
          updateDna()
          )
      | None -> ()
    dnaView.wheelCallback <- fun i delta ->
      match !focusView with
      | Some(view) ->
        view.post(fun _ ->
          view.controller.modifyValue(i, float (sign delta) * -0.02)
          updateDna()
          )
      | None -> ()


    // Add context menus and event handlers to images.
    iterateViews (fun view _ ->

      let menu = ContextMenu(Placement = Primitives.PlacementMode.Mouse)

      if view.mainMode = Half then
        let maximizeItem = MenuItem(Header = "Maximize")
        maximizeItem.Click.Add(fun _ ->
          fullView.view.reset()
          fullView.controller.copyFrom(view.controller)
          setFocus fullView
          setGuiMode Full
          minimizeView := view
         )
        menu.add(maximizeItem)
      elif view.mainMode = Full then
        let minimizeItem = MenuItem(Header = "Minimize")
        minimizeItem.Click.Add(fun _ ->
          let targetView = !minimizeView
          targetView.view.reset()
          targetView.controller.copyFrom(view.controller)
          setFocus targetView
          setGuiMode Half
          )
        menu.add(minimizeItem)

      let zoomOut = MenuItem(Header = "Zoom Out")
      zoomOut.Click.Add(fun _ ->
        view.post(fun _ ->
          setFocus view
          view.controller.alter(fun _ dna i -> if dna.[i].name = "View Zoom" then ModifyFloat(fun zoom -> zoom * 0.5) else Retain)
          )
        )
      menu.add(zoomOut)

      let resetZoom = MenuItem(Header = "Reset Zoom")
      resetZoom.Click.Add(fun _ ->
        view.post(fun _ ->
          setFocus view
          view.controller.alter(fun _ dna i -> if dna.[i].name = "View Zoom" then SelectFloat 1.0 else Retain)
          )
        )
      menu.add(resetZoom)

      let resetView = MenuItem(Header = "Reset View")
      resetView.Click.Add(fun _ ->
        view.post(fun _ ->
          setFocus view
          view.controller.alter(fun _ dna i -> if dna.[i].name = "View Zoom" then SelectFloat 1.0 elif dna.[i].name.StartsWith("View Center") then SelectFloat 0.5 else Retain)
          )
        )
      menu.add(resetView)

      let randomizeItem = MenuItem(Header = "Randomize")
      randomizeItem.Click.Add(fun _ ->
        view.post(fun _ ->
          view.controller.restart(randomizePredicate)
          setFocus view
          )
        )
      menu.add(randomizeItem)

      let save1920 = MenuItem(Header = "Export 1920 x 1920 Image..")
      save1920.Click.Add(fun _ ->
        setFocus view
        let map = !view.controller.deep
        exportMap3Png map.map 1920 1920 map.camera
        )
      menu.add(save1920)

      let save2048 = MenuItem(Header = "Export 2k Image..")
      save2048.Click.Add(fun _ ->
        setFocus view
        let map = !view.controller.deep
        exportMap3Png map.map 2048 2048 map.camera
        )
      menu.add(save2048)

      let save3840 = MenuItem(Header = "Export 3840 x 3840 Image..")
      save3840.Click.Add(fun _ ->
        setFocus view
        let map = !view.controller.deep
        exportMap3Png map.map 3840 3840 map.camera
        )
      menu.add(save3840)

      let save4096 = MenuItem(Header = "Export 4k Image..")
      save4096.Click.Add(fun _ ->
        setFocus view
        let map = !view.controller.deep
        exportMap3Png map.map 4096 4096 map.camera
        )
      menu.add(save4096)

      let showRayTrace = MenuItem(Header = "Show Ray Trace..")
      showRayTrace.Click.Add(fun _ ->
        let deep = !view.controller.deep
        let diffuse = deep.map >> map11to01 >> Map3.scale 0.7f
        let specular = Map3.constant (Vec3f 0.4f)
        let material = Ray.Material.create(Map3.zero, Ray.OrenNayar(Map3.constant (Vec3f 0.25f)), diffuse, Ray.Phong(Map3.constant (Vec3f 80.0f)), specular, Map3.zero)
        RayGui.visualize 800 material
        )
      menu.add(showRayTrace)

      let image = view.image
      image.ContextMenu <- menu

      image.PreviewMouseLeftButtonDown.Add(fun (args : Input.MouseButtonEventArgs) ->
        match !toolMode with
        | ZoomTool ->
          if (!focusView).isSomeAnd((===) view) then
            userAction := Zooming(view, args.GetPosition(canvas).vec2f)
            image.CaptureMouse() |> ignore
          else
            setFocus view

        | PanTool ->
          userAction := Panning(view, args.GetPosition(canvas).vec2f)
          image.CaptureMouse() |> ignore
          setFocus view

        | MutateTool ->
          if (!focusView).isSomeAnd((===) view) then
            // Mutate other half views.
            if view.mainMode = Half && !view.controller.deep <>= richSeed then view.post(fun _ ->
              for targetView in halfView do
                if targetView <>= view then
                  let mutateMode = match !mutateMode with | Random -> rnd.choose(1.0, ColorsEffects, 1.0, ScalesOffsets, 1.0, Details, 1.0, Everything) | x -> x
                  let mR = rnd.exp(0.5, 2.0)
                  let predicate =
                    match mutateMode with
                    | ColorsEffects ->
                      fun (rnd : Rnd) (dna : Dna) i ->
                        let name = dna.[i].name
                        let parentName = dna.parentParameter(i).map(fun p -> p.name) >? ""
                        if name = "Generator" || name = "Layout" || name = "View Zoom" || name.StartsWith("View Center") then
                          Retain
                        elif name = "Color space" then
                          rnd.choose(2.0, Retain, 1.0 * mR, Randomize)
                        elif name = "Color permutation" then
                          rnd.choose(2.0, Retain, 1.0 * mR, Randomize)
                        elif name.StartsWith("Color") || name.StartsWith("Hue") || name.StartsWith("Saturation") || name = "Gamma skew" then
                          Jolt(rnd.exp(0.01, 1.0))
                        elif name = "Shape" || parentName = "Shape" then
                          rnd.choose(2.0, Retain, 1.0 * mR, Jolt(rnd.exp(0.01, 1.0)))
                        elif name = "Cell color" || parentName = "Cell color" then
                          rnd.choose(2.0, Retain, 1.0 * mR, Jolt(rnd.exp(0.01, 1.0)))
                        else Retain
                    | ScalesOffsets ->
                      fun rnd (dna : Dna) i ->
                        let name = dna.[i].name
                        if name = "Generator" || name = "Layout" || name = "View Zoom" || name.StartsWith("View Center") then
                          Retain
                        elif name = "X offset" || name = "Y offset" || name = "Z offset" then
                          rnd.choose(1.5, Retain, 1.0 * mR, Jolt(rnd.exp(0.01, 1.0)))
                        elif name = "Frequency" || name = "Frequency factor" || name = "Flow frequency factor" then
                          rnd.choose(2.0, Retain, 1.0 * mR, Jolt(rnd.exp(0.01, 1.0)))
                        elif name = "Lacunarity" then
                          rnd.choose(2.0, Retain, 1.0 * mR, Jolt(rnd.exp(0.01, 1.0)))
                        else Retain
                    | Details ->
                      fun rnd (dna : Dna) i ->
                        let name = dna.[i].name
                        let parentName = dna.parentParameter(i).map(fun p -> p.name) >? ""
                        if name = "Generator" || name = "Layout" || name = "View Zoom" || name.StartsWith("View Center") then
                          Retain
                        elif name = "Roughness" then
                          rnd.choose(2.0, Retain, 1.0 * mR, Jolt(rnd.exp(0.05, 0.5)))
                        elif name = "Octaves" then
                          rnd.choose(2.0, Retain, 1.0 * mR, Jolt(rnd.exp(0.05, 0.3)))
                        elif name = "Layer hardness" || name = "Layer width" || name = "Rotate width" then
                          rnd.choose(2.0, Retain, 1.0 * mR, Jolt(rnd.exp(0.01, 1.0)))
                        elif name = "Walk operator" || name = "Displace amount" || name = "Basis displace amount" || name = "Rotate amount" || parentName = "Displace response" || name = "Basis displace response" then
                          rnd.choose(2.0, Retain, 1.0 * mR, Jolt(rnd.exp(0.01, 1.0)))
                        elif parentName = "Mix operator" || parentName = "Shape" || parentName = "Features per cell" || parentName = "Potential function" || parentName = "Basis" then
                          rnd.choose(2.0, Retain, 1.0 * mR, Jolt(rnd.exp(0.01, 1.0)))
                        elif name.EndsWith("fade") || name.EndsWith("shading") || name.EndsWith("radius") then
                          rnd.choose(2.0, Retain, 1.0 * mR, Jolt(rnd.exp(0.01, 1.0)))
                        else Retain
                    | Everything | _ ->
                      let mutationFrequency = rnd.exp(0.02, 0.2)
                      fun rnd (dna : Dna) i ->
                        let name = dna.[i].name
                        if name = "Generator" || name = "Layout" || name = "View Zoom" || name.StartsWith("View Center") then
                          Retain
                        elif rnd.boolean(mutationFrequency) then
                          Jolt(rnd.exp(0.01, 1.0))
                        else
                          Retain
                  targetView.controller.mutateFrom(view.controller, predicate)
              )
          else
            setFocus view

        args.Handled <- true
        )

      image.MouseMove.Add(fun (args : Input.MouseEventArgs) ->
        match !userAction with
        | Panning(_, source) ->
          let target = args.GetPosition(canvas).vec2f
          let delta = (source - target) / (float32 image.Width * (!view.controller.deep).zoom)
          view.panDelta.modify((+) <| Vec3f(delta.x, delta.y, 0.0f))
          view.post(ignore)
          userAction := Panning(view, target)
          args.Handled <- true

        | Zooming(_, source) ->
          let target = args.GetPosition(canvas).vec2f
          let size = (target - source).maxNorm
          dragShape.Margin <- Thickness(Left = float (source.x - size), Top = float (source.y - size))
          dragShape.Width <- 2.0 * float size
          dragShape.Height <- 2.0 * float size
          if dragShape.Visibility = Visibility.Collapsed && Vec2f.distance(source, target) >= dragMinimum then
            dragShape.Visibility <- Visibility.Visible
          elif dragShape.Visibility = Visibility.Visible && Vec2f.distance(source, target) < dragMinimum then
            dragShape.Visibility <- Visibility.Collapsed
          args.Handled <- true

        | _ -> ()
        )

      image.MouseWheel.Add(fun (args : Input.MouseWheelEventArgs) ->
        if args.Delta <> 0 then
          let delta = float32 (sign args.Delta) * 0.01f / (!view.controller.deep).zoom
          view.panDelta.modify((+) <| Vec3f(0.0f, 0.0f, delta))
          view.post(ignore)
        args.Handled <- true
        setFocus view
        )

      image.MouseLeftButtonUp.Add(fun (args : Input.MouseButtonEventArgs) ->
        match !userAction with
        | Idle -> ()

        | Panning(_, source) -> ()

        | Zooming(_, source) ->
          let target = args.GetPosition(canvas).vec2f
          let size = (target - source).maxNorm
          if !view.controller.deep <>= richSeed && Vec2f.distance(source, target) >= dragMinimum then
            // Zoom into view.
            let itop = Vec2f(image.Margin.Left |> float32, image.Margin.Top |> float32)
            let ibottom = Vec2f(itop.x + float32 image.Width, itop.y + float32 image.Height)
            let x0, y0, x1, y1 = (!view.controller.deep).viewBox
            let x0' = lerp x0 x1 (float32 <| delerp itop.x ibottom.x (source.x - size))
            let x1' = lerp x0 x1 (float32 <| delerp itop.x ibottom.x (source.x + size))
            let y0' = lerp y0 y1 (float32 <| delerp itop.y ibottom.y (source.y - size))
            let y1' = lerp y0 y1 (float32 <| delerp itop.y ibottom.y (source.y + size))
            view.post(fun _ ->
              view.controller.alter(fun _ dna i ->
                let name = dna.[i].name
                if name = "View Center X" then SelectFloat (float <| average x0' x1')
                elif name = "View Center Y" then SelectFloat (float <| average y0' y1')
                elif name = "View Zoom" then SelectFloat (1.0 / float (y1' - y0'))
                else Retain
                )
              )
          dragShape.Visibility <- Visibility.Collapsed

        userAction := Idle
        image.ReleaseMouseCapture()
        args.Handled <- true
        )

      image.ContextMenuOpening.Add(fun (args : ContextMenuEventArgs) ->
        // Do not open the context menu while the mouse is captured (something bad will happen if we open it).
        match !userAction with
        | Zooming(_, _) | Panning(_, _) -> args.Handled <- true
        | _ -> ()
        )

      )

    let menuPanel = StackPanel(Orientation = Orientation.Horizontal)
    menuPanel.VerticalAlignment <- VerticalAlignment.Center
    menuPanel.HorizontalAlignment <- HorizontalAlignment.Stretch
    let menu = Menu()

    let fileMenu = MenuItem(Header = "_File")

    let openItem = MenuItem(Header = "_Open..")
    openItem.Click.Add(fun _ ->
      let view = !focusView >? match !guiMode with | Full -> fullView | Center -> centerView | Half -> halfView.[0] | Quarter -> quarterView.[0]
      setFocus view
      let dialog = new Microsoft.Win32.OpenFileDialog(Title = "Load Map File..", Filter = "YAML files (.yaml)|*.yaml")
      let result = dialog.ShowDialog()
      if result.HasValue && result.Value = true then
        try
          use stream = new System.IO.StreamReader(dialog.FileName)
          let readLine() = match stream.EndOfStream with | false -> Some(stream.ReadLine()) | true -> None
          let source = DeserializerSource(readLine)
          view.controller.generate(true, dnaSource = (source :> DnaSource))
          stream.Close()
          updateDna()
        with
          | ex -> MessageBox.Show(ex.ToString()) |> ignore
      )
    fileMenu.add(openItem)

    let saveItem = MenuItem(Header = "_Save As..")
    saveItem.Click.Add(fun _ ->
      match !focusView with
      | Some(view) ->
        let dialog = new Microsoft.Win32.SaveFileDialog(Title = "Save Map File As..", DefaultExt = ".yaml", Filter = "YAML files (.yaml)|*.yaml")
        let result = dialog.ShowDialog()
        if result.HasValue && result.Value = true then
          try
            let source = SerializerSource(!view.controller.dna)
            source.generate(RichMap3.generate generateExplorerMap) |> ignore
            use stream = new System.IO.StreamWriter(dialog.FileName)
            stream.Write(source.yamlString)
            stream.Close()
          with
            | ex -> MessageBox.Show(ex.ToString()) |> ignore
      | None -> ()
      )
    fileMenu.add(saveItem)

    let quitItem = MenuItem(Header = "Quit")
    fileMenu.add(quitItem)
    quitItem.Click.Add(fun _ -> window.Close())

    menu.add(fileMenu)
    menuPanel.add(menu)

    let randomizeAllButton = Button(Content = "Randomize All")
    randomizeAllButton.Click.Add(fun _ -> randomizeAll())
    toolPanel.add(randomizeAllButton)
    toolPanel.add(Separator(Margin = Thickness(0.0, 6.0, 0.0, 0.0)))
    toolPanel.add(Label(Content = "Mutation mode"))
    let mutationModeBox = ComboBox()
    mutationModeBox.add(ComboBoxItem(Content = "Everything", IsSelected = true, withSelected = fun _ -> mutateMode := Everything))
    mutationModeBox.add(ComboBoxItem(Content = "Colors and Effects", withSelected = fun _ -> mutateMode := ColorsEffects))
    mutationModeBox.add(ComboBoxItem(Content = "Scales and Offsets", withSelected = fun _ -> mutateMode := ScalesOffsets))
    mutationModeBox.add(ComboBoxItem(Content = "Details", withSelected = fun _ -> mutateMode := Details))
    mutationModeBox.add(ComboBoxItem(Content = "Choose at Random", withSelected = fun _ -> mutateMode := Random))
    toolPanel.add(mutationModeBox)

    let layoutModeBox = ComboBox()
    for i = 0 to layoutChoices.last do
      layoutModeBox.add(ComboBoxItem(Content = layoutChoices.name(i), IsSelected = (layoutChoices.weight(i) > 1.0), withSelected = fun _ -> layoutMode := layoutChoices.value(i)))
    toolPanel.add(Separator(Margin = Thickness(0.0, 6.0, 0.0, 0.0)))
    toolPanel.add(Label(Content = "Default layout mode"))
    toolPanel.add(layoutModeBox)

    toolPanel.add(Separator(Margin = Thickness(0.0, 6.0, 0.0, 6.0)))
    toolPanel.add(toolbar)

    let splitter = GridSplitter(Width = 5.0, Margin = Thickness(0.0))
    splitter.VerticalAlignment <- VerticalAlignment.Stretch
    splitter.HorizontalAlignment <- HorizontalAlignment.Left

    dnaView.treeView.HorizontalAlignment <- HorizontalAlignment.Stretch
    dnaView.treeView.VerticalAlignment <- VerticalAlignment.Stretch

    let mainPanel = Grid()
    mainPanel.RowDefinitions.Add(RowDefinition(Height = GridLength.Auto))
    mainPanel.RowDefinitions.Add(RowDefinition())
    mainPanel.ColumnDefinitions.Add(ColumnDefinition(Width = GridLength.Auto))
    mainPanel.ColumnDefinitions.Add(ColumnDefinition(Width = GridLength.Auto))
    mainPanel.ColumnDefinitions.Add(ColumnDefinition(Width = GridLength.Auto))
    mainPanel.ColumnDefinitions.Add(ColumnDefinition())
    mainPanel.add(menuPanel, 0, 0, 3, 1)
    mainPanel.add(dnaView.treeView, 0, 1)
    mainPanel.add(splitter, 1, 1)
    mainPanel.add(toolPanel, 2, 1)
    mainPanel.add(canvas, 3, 1)

    // Create an invisible label to take up some initial space in the Dna view column.
    mainPanel.add(Label(Content = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", Height = 0.0, Visibility = Visibility.Hidden), 0, 1)

    window.setPixelContent(mainPanel)
    window.KeyDown.Add(fun args -> if args.Key = System.Windows.Input.Key.Escape then window.Close())
    window.Closed.Add(fun _ ->
      iterateViews(fun view _ -> view.stop())
      )
    //window.SizeChanged.Add(fun _ -> layoutCanvas())
    window.Loaded.Add(fun _ ->
      setToolMode MutateTool
      )
    window.LayoutUpdated.Add(fun _ -> layoutCanvas())
    window.Show()

    iterateViews(fun view _ ->
      view.view.start()
      )


    
(*
TODO

-Store generator version info in YAML.
-Add export in F# source format, i.e., DnaData constructor and function that invokes generator.
-Modify InteractiveSource & stuff so that editing of node graph becomes easier. E.g., delete parent, insert node...
-Never tile pattern atlases, there is no need.
 Possibility: temporary Dna injector.
-Add the planned mosaic GUI mode. I.e., one big picture divided into 4x4 squares which are mutated independently.
-Add .dds export for 2-D and 3-D textures.
-Add Map3 display modes: rectangle, depth slices, depth strip, sphere?
-Supporting Undo? Or maybe History? I guess we just store past Dnas in a global list?
-Status line. Where do we put this? At the bottom? I think we don't want to use the menu bar for this.
 Instead, we can put something else in the menu bar if necessary.
-Consider replacing right mouse button at Dna parameter with context menu. What do we put in the menu?
 -display range of parameter values in a gradient, pick new value by clicking.
  (how do we implement this?)
 -lock parameter.
 -increase chance of mutation of parameter?
 -randomize value.
-Add tooltips.
-Main menu bar. We should put more stuff there.
 -Defaults: default normalization mode. default tiling mode.
 -Mutation mode belongs in the tool options!
-Current focused map information in tool panel. X range, Y range, Z, zoom, description length,
 sampled histogram, sampled slopes.
-Is animation support possible?
-Cache normalization information (Map3Info). For map identification, it suffices to
 generate a subtree fingerprint from Dna combined with subgenerator ID.
-To make caching of map pixmaps themselves possible, call patterns need to be
 fingerprinted as well. These flow downstream from the root. In practice, we could
 affix a downstream fingerprint to Dna that is maintained per level.

*)
