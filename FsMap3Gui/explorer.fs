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
open Map3Info
open Map3Dna
open Map3Gui


[<NoComparison; NoEquality>]
type UserAction =
  | Zooming of view : ExplorerView<RichMap3> * source : Vec2f
  | Panning of view : ExplorerView<RichMap3> * source : Vec2f
  | Idle



/// Interactive Map3 explorer.
type Explorer =

  static member start(?initialDnaSource) =

    /// Minimum resolution of each quarter view.
    let minimumR = 16
    /// View border thickness.
    let viewBorder = 2
    /// How far user has to move the mouse before it is recognized as zooming.
    let dragMinimum = 16.0f
    /// Width of tool bar panel.
    let toolPanelWidth = 152.0
    /// Size of tool and view icons.
    let iconSize = 29.0
    let iconMargin = Thickness(0.5)

    let viewBg = Wpf.brush(0.0)
    let dnaBg = Wpf.brush(0.9)
    let menuBg = Wpf.verticalBrush(Wpf.color(0.8, 0.88, 0.9), Wpf.color(0.7, 0.78, 0.8))
    let menuItemBg = Wpf.brush(1.0, 1.0, 1.0, 0.2)
    let toolBg = Wpf.brush(0.8, 0.8, 0.8, 0.3)
    let splitterBg = Wpf.brush(0.93)

    // We have 1 full view, 4 half views and 16 quarter views.
    let fN = 1
    let hN = 4
    let qN = 16

    let rnd = Rnd(timeSeed())

    let currentCanvasWidth = ref 0
    let currentCanvasHeight = ref 0

    /// Which PixmapViews are visible depends on the mode.
    let viewMode = ref ExplorerViewMode.FullView

    /// Current tool.
    let toolMode = ref ExplorerTool.MutateTool

    /// Current default layout.
    let layoutMode = ref Layout.Hifi

    /// Current mutation mode.
    let mutateMode = ref Everything

    let logWindow = ref none<LogWindow>

    /// Current user action.
    let userAction = ref Idle

    let window = Window(Title = "FsMap3 Explorer", ResizeMode = ResizeMode.CanResize, Width = 1024.0, Height = 480.0, SizeToContent = SizeToContent.Manual, Topmost = false, WindowStartupLocation = WindowStartupLocation.CenterScreen)

    let canvas = Grid(Background = viewBg, ClipToBounds = true, Margin = Thickness(0.0, 0.0, 0.0, 0.0))
    canvas.ColumnDefinitions.Add(ColumnDefinition())
    canvas.RowDefinitions.Add(RowDefinition())
    canvas.HorizontalAlignment <- HorizontalAlignment.Stretch
    canvas.VerticalAlignment <- VerticalAlignment.Stretch

    let mapInfoBox = RichMap3InfoBox.create(toolPanelWidth)
    mapInfoBox.panel.HorizontalAlignment <- HorizontalAlignment.Center
    mapInfoBox.panel.VerticalAlignment <- VerticalAlignment.Bottom
    mapInfoBox.reset()

    let mapFilter =
      { 
        RichMap3Filter.minDetail = 50.0f
        maxDetail = 1000.0f
        minDeviation = 0.1f
        minDifference = 0.01f
        maxDifference = infinityf
      }

    let mapSeed = Map3.zero
    let richSeed = { RichMap3.map = mapSeed; palette = Map3.identity; center = Vec3f(0.5f); zoom = 1.0f; aspectRatio = 1.0f; info = Map3Info.create(mapSeed) }
    let deepGenerator = Map3Dna.generateExplorerMap
    let pixmapGenerator extraTransform =
      match extraTransform with
      | Some(transform) -> RichMap3.pixmapSourceWith(transform)
      | None -> RichMap3.pixmapSource
    let deepFilter = fun map previous -> mapFilter.filter(map, previous)

    let fullView = ExplorerView.create(FullView, 0, 0, 0, richSeed, deepGenerator, pixmapGenerator None, deepFilter, canvas, true, 4)

    let halfView = Array.init hN (fun i ->
      let x, y = i % 2, i / 2
      ExplorerView.create(HalfView, i, x, y, richSeed, deepGenerator, pixmapGenerator None, deepFilter, canvas, false, 4)
      )

    let quarterView = Array.init qN (fun i ->
      let x, y = i % 4, i / 4
      let mosaicTransform (v : Vec3f) = Vec3f((v.x + float32 x) / 4.0f, (v.y + float32 y) / 4.0f, v.z)
      ExplorerView.create(QuarterView, i, x, y, richSeed, deepGenerator, pixmapGenerator (Some mosaicTransform), deepFilter, canvas, false, 3)
      )

    let halfQuarterView = Array.append halfView quarterView

    for view in halfQuarterView do
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
        fullView.view.setRenderSize(R * 4, R * 4)

        halfView |> Array.iteri (fun i view ->
          let image = view.image
          image.Width <- float (R * 2)
          image.Height <- float (R * 2)
          let x, y = view.gridX, view.gridY
          image.Margin <- Thickness(Left = float (x0 + x * R * 2 + viewBorder), Top = float (y0 + y * R * 2 + viewBorder))
          let shape = !view.focusShape
          shape.Width <- float ((R + viewBorder) * 2)
          shape.Height <- float ((R + viewBorder) * 2)
          shape.Margin <- Thickness(Left = float (x0 + x * R * 2), Top = float (y0 + y * R * 2))
          view.view.setRenderSize(R * 2, R * 2)
         )

        quarterView |> Array.iteri (fun i view ->
          let image = view.image
          image.Width <- float R
          image.Height <- float R
          let x, y = view.gridX, view.gridY
          image.Margin <- Thickness(Left = float (x0 + x * R + viewBorder), Top = float (y0 + y * R + viewBorder))
          let shape = !view.focusShape
          shape.Width <- float (R + viewBorder * 2)
          shape.Height <- float (R + viewBorder * 2)
          shape.Margin <- Thickness(Left = float (x0 + x * R), Top = float (y0 + y * R))
          view.view.setRenderSize(R, R)
          )
     
    let toolPanel = DockPanel(Width = toolPanelWidth, Margin = Thickness(0.0), VerticalAlignment = VerticalAlignment.Stretch)

    let createIconButton imageFile tip = ToggleButton(Content = Wpf.loadImage(imageFile), withToolTip = tip, Width = iconSize, Height = iconSize, Margin = iconMargin, Background = Wpf.brush(0.0, 0.0, 0.0, 0.1))

    let toolBar = StackPanel(Orientation = Orientation.Horizontal, Margin = Thickness(1.0))
    let panButton = createIconButton "appbar.cursor.move.png" "Pan Tool: drag view to pan, mouse wheel controls Z."
    toolBar.add(panButton)
    let panZoomButton = createIconButton "pan-zoom-3.png" "Pan-Zoom Tool: drag view to pan, mouse wheel controls zoom."
    toolBar.add(panZoomButton)
    let zoomButton = createIconButton "appbar.magnify.png" "Zoom Tool: select an area to zoom into."
    toolBar.add(zoomButton)
    let mutateButton = createIconButton "appbar.diagram.png" "Mutate Tool: make other views mutations of clicked view."
    toolBar.add(mutateButton)
    let joltButton = createIconButton "appbar.camera.flash.png" "Jolt Tool: mutate the view."
    toolBar.add(joltButton)

    let setToolMode mode =
      panButton.IsChecked <- Nullable((mode = PanTool))
      panZoomButton.IsChecked <- Nullable((mode = PanZoomTool))
      zoomButton.IsChecked <- Nullable((mode = ZoomTool))
      mutateButton.IsChecked <- Nullable((mode = MutateTool))
      joltButton.IsChecked <- Nullable((mode = JoltTool))
      toolMode := mode

    panButton.PreviewMouseDown.Add(fun args -> setToolMode PanTool; args.Handled <- true)
    panZoomButton.PreviewMouseDown.Add(fun args -> setToolMode PanZoomTool; args.Handled <- true)
    zoomButton.PreviewMouseDown.Add(fun args -> setToolMode ZoomTool; args.Handled <- true)
    mutateButton.PreviewMouseDown.Add(fun args -> setToolMode MutateTool; args.Handled <- true)
    joltButton.PreviewMouseDown.Add(fun args -> setToolMode JoltTool; args.Handled <- true)

    let viewBar = StackPanel(Orientation = Orientation.Horizontal, Margin = Thickness(1.0))
    let fullViewButton = createIconButton "fullview.png" "Big View"
    viewBar.add(fullViewButton)
    let halfViewButton = createIconButton "halfview.png" "2×2 Small Views"
    viewBar.add(halfViewButton)
    let quarterViewButton = createIconButton "quarterview.png" "Mosaic View"
    viewBar.add(quarterViewButton)

    /// Which half view did we maximize last?
    let minimizeHalfView = ref halfView.[0]
    /// Which mosaic view tile did we maximize last?
    let minimizeQuarterView = ref quarterView.[0]

    let focusView = ref (Some(fullView))

    /// Iterates over all views. The second argument tells whether the view is visible.
    let iterateViews (f : ExplorerView<_> -> bool -> unit) =
      match !viewMode with
      | FullView ->
        f fullView true
        for view in halfView do f view false
        for view in quarterView do f view false
      | HalfView ->
        f fullView false
        for view in halfView do f view true
        for view in quarterView do f view false
      | QuarterView ->
        f fullView false
        for view in halfView do f view false
        for view in quarterView do f view true

    let dnaView = DnaView(viewFilter = fun parameter ->
      // The layout is generated once at the top level and then propagated as a constraint.
      if parameter.name = "Layout" && parameter.level > 0 then
        Hidden
      // View window is manipulated using the view controls.
      elif parameter.name.StartsWith("View") then
        Hidden
      // Version information is of no interest here.
      elif parameter.name.StartsWith("FsMap3") then
        Hidden
      else Editable
      )

    dnaView.treeView.Background <- dnaBg
    dnaView.addChoiceVisualizer(DnaVisualizer.fadeChoiceVisualizer 50 20)
    dnaView.addChoiceVisualizer(DnaVisualizer.colorSpaceChoiceVisualizer (int dnaView.valueBoxWidth - 8) 30)
    dnaView.addVisualizerDependency(fun parameter -> if parameter.name = "Weave" then [(1, "Weave period")] else List.empty)

    /// Updates the Dna view. This can be called from any thread.
    let updateDna() =
      Wpf.dispatch(window, fun _ ->
        match !focusView with
        | Some(view) ->
          if view.isEmpty then
            dnaView.reset()
          else
            dnaView.update(!view.controller.dna)
        | None ->
          dnaView.reset()
        )

    /// Updates the map info box. This can be called from any thread.
    let updateInfo() =
      Wpf.dispatch(window, fun _ ->
        match !focusView with
        | Some(view) ->
          if view.isEmpty then
            mapInfoBox.reset()
          else
            mapInfoBox.update(!view.controller.deep)
        | None ->
          mapInfoBox.reset()
        )

    /// Sets focus to the view and displays its Dna and info.
    let setFocus view' =
      if (!focusView).isNoneOr((<>=) view') then
        focusView := Some(view')
        Wpf.dispatch(window, fun _ ->
          for view in halfQuarterView do
            (!view.focusShape).Visibility <- if view' === view then Visibility.Visible else Visibility.Hidden
          )
      updateDna()
      updateInfo()

    /// Clears focus and resets the Dna view and the info box.
    let clearFocus() =
      focusView := None
      updateDna()
      updateInfo()
      Wpf.dispatch(window, fun _ ->
        for view in halfQuarterView do
          (!view.focusShape).Visibility <- Visibility.Hidden
        )

    /// This predicate creates a random map from scratch.
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

    let setViewMode mode =
      viewMode := mode
      iterateViews (fun view visible -> view.image.Visibility <- if visible then Visibility.Visible else Visibility.Collapsed)
      fullViewButton.IsChecked <- Nullable((mode = FullView))
      halfViewButton.IsChecked <- Nullable((mode = HalfView))
      quarterViewButton.IsChecked <- Nullable((mode = QuarterView))

    dnaView.leftCallback <- fun i value ->
      match !focusView with
      | Some(view) ->
        view.post(fun _ ->
          view.controller.setValue(i, Some(value))
          updateDna()
          updateInfo()
          )
      | None -> ()
    dnaView.rightCallback <- fun i ->
      match !focusView with
      | Some(view) ->
        view.post(fun _ ->
          view.controller.setValue(i, None)
          updateDna()
          updateInfo()
          )
      | None -> ()
    dnaView.wheelCallback <- fun i delta ->
      match !focusView with
      | Some(view) ->
        view.post(fun _ ->
          view.controller.modifyValue(i, float (sign delta) * -0.02)
          updateDna()
          updateInfo()
          )
      | None -> ()

    /// Copies the view to full view mode.
    let maximizeView view =
      fullView.view.reset()
      fullView.controller.copyFrom(view.controller)
      setFocus fullView
      setViewMode FullView
      match view.mainMode with
      | HalfView -> minimizeHalfView := view
      | QuarterView -> minimizeQuarterView := view
      | _ -> ()

    /// Copies the view to 2x2 view mode.
    let minimizeView view =
      let targetView = !minimizeHalfView
      targetView.view.reset()
      targetView.controller.copyFrom(view.controller)
      setFocus targetView
      setViewMode HalfView

    /// Copies the view to mosaic view mode.
    let mosaicifyView view =
      let targetView = !minimizeQuarterView
      for qview in quarterView do
        if qview === targetView || qview.isEmpty then
          qview.view.reset()
          qview.controller.copyFrom(view.controller)
      setFocus targetView
      setViewMode QuarterView

    fullViewButton.Click.Add(fun _ ->
      match !viewMode with
      | HalfView | QuarterView ->
        match !focusView with
        | Some(view) -> maximizeView view
        | None -> clearFocus(); setViewMode FullView
      | _ -> ()
      )

    halfViewButton.Click.Add(fun _ ->
      match !viewMode with
      | FullView -> minimizeView fullView
      | QuarterView ->
        match !focusView with
        | Some(view) -> minimizeView view
        | None -> clearFocus(); setViewMode HalfView
      | _ -> ()
      )

    quarterViewButton.Click.Add(fun _ ->
      match !viewMode with
      | FullView -> mosaicifyView fullView
      | HalfView ->
        match !focusView with
        | Some(view) -> mosaicifyView view
        | None -> clearFocus(); setViewMode QuarterView
      | _ -> ()
      )

    // Add context menus and event handlers to images.
    iterateViews (fun view _ ->

      let menu = ContextMenu(Placement = Primitives.PlacementMode.Mouse)

      if view.mainMode = HalfView || view.mainMode = QuarterView then
        let maximizeItem = MenuItem(Header = "Maximize")
        maximizeItem.Click.Add(fun _ -> maximizeView view)
        menu.add(maximizeItem)
      elif view.mainMode = FullView || view.mainMode = QuarterView then
        let minimizeItem = MenuItem(Header = "View in 2x2")
        minimizeItem.Click.Add(fun _ -> minimizeView view)
        menu.add(minimizeItem)
      if view.mainMode = FullView || view.mainMode = HalfView then
        let mosaicifyItem = MenuItem(Header = "View in Mosaic")
        mosaicifyItem.Click.Add(fun _ -> mosaicifyView view)
        menu.add(mosaicifyItem)

      let zoomIn = MenuItem(Header = "Zoom In")
      zoomIn.Click.Add(fun _ ->
        view.post(fun _ ->
          setFocus view
          view.controller.alter(View.transform(zoom = ModifyFloat((*) 2.0)))
          updateInfo()
          )
        )
      menu.add(zoomIn)

      let zoomOut = MenuItem(Header = "Zoom Out")
      zoomOut.Click.Add(fun _ ->
        view.post(fun _ ->
          setFocus view
          view.controller.alter(View.transform(zoom = ModifyFloat((*) 0.5)))
          updateInfo()
          )
        )
      menu.add(zoomOut)

      let resetZoom = MenuItem(Header = "Reset Zoom")
      resetZoom.Click.Add(fun _ ->
        view.post(fun _ ->
          setFocus view
          view.controller.alter(View.transform(zoom = SelectFloat 1.0))
          updateInfo()
          )
        )
      menu.add(resetZoom)

      let resetView = MenuItem(Header = "Reset View")
      resetView.Click.Add(fun _ ->
        view.post(fun _ ->
          setFocus view
          view.controller.alter(View.transform(centerX = SelectFloat 0.5, centerY = SelectFloat 0.5, centerZ = SelectFloat 0.5, zoom = SelectFloat 1.0))
          updateInfo()
          )
        )
      menu.add(resetView)

      let randomizeItem = MenuItem(Header = "Randomize")
      randomizeItem.Click.Add(fun _ ->
        view.post(fun _ ->
          view.controller.restart(randomizePredicate)
          setFocus view
          updateInfo()
          )
        )
      menu.add(randomizeItem)

      let openNewWindow = MenuItem(Header = "Open in New Window")
      openNewWindow.Click.Add(fun _ ->
        Explorer.start(DnaData(!view.controller.dna))
        )
      menu.add(openNewWindow)

      let copySource = MenuItem(Header = "Copy F# Code to Clipboard")
      copySource.Click.Add(fun _ ->
        let data = DnaData(!view.controller.dna)
        System.Windows.Clipboard.SetText(data.sourceCode + ".generate(Map3Dna.generateExplorerMap)\n")
        )
      menu.add(copySource)

      let showRayTrace = MenuItem(Header = "Show Ray Trace")
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

      // The logic here is: if just pressing the left button causes something to happen,
      // then the first click on an unfocused view only brings the view to focus.
      image.PreviewMouseLeftButtonDown.Add(fun (args : Input.MouseButtonEventArgs) ->
        if view.isEmpty then
          setFocus view
        else
          match !toolMode with
          | ZoomTool ->
            userAction := Zooming(view, args.GetPosition(canvas).vec2f)
            image.CaptureMouse() |> ignore
            setFocus view

          | PanTool | PanZoomTool ->
            userAction := Panning(view, args.GetPosition(canvas).vec2f)
            image.CaptureMouse() |> ignore
            setFocus view

          | MutateTool ->
            if (!focusView).isSomeAnd((===) view) then
              // Mutate other half or mosaic views.
              match view.mainMode with
              | FullView -> ()
              | HalfView ->
                view.post(fun _ ->
                  for targetView in halfView do
                    if targetView <>= view then
                      let predicate = View.mutationPredicate(rnd, view, !mutateMode)
                      targetView.controller.mutateFrom(view.controller, predicate)
                  )
              | QuarterView ->
                view.post(fun _ ->
                  let predicate = View.mosaicPredicate(rnd, !view.controller.dna, view, !mutateMode)
                  let motherView = quarterView.[kronecker view.gridI 0]
                  motherView.controller.mutateFrom(view.controller, predicate true (float motherView.gridI / float quarterView.last), false)
                  for targetView in quarterView do
                    if targetView <>= view && targetView <>= motherView then
                      targetView.controller.mutateFrom(motherView.controller, predicate false (float targetView.gridI / float quarterView.last), true)
                  )
            else
              setFocus view

          | JoltTool ->
            if (!focusView).isSomeAnd((===) view) then
              // Mutate the view.
              view.post(fun _ ->
                let predicate = View.mutationPredicate(rnd, view, !mutateMode)
                view.controller.mutateFrom(view.controller, predicate)
                updateDna()
                updateInfo()
                )
            else
              setFocus view

        args.Handled <- true
        )

      let iteratePanZoomViews(f) =
        if view.isEmpty = false then
          match view.mainMode with
          | QuarterView -> for qview in quarterView do f qview
          | _ -> f view

      image.MouseMove.Add(fun (args : Input.MouseEventArgs) ->
        match !userAction with
        | Panning(view, source) ->
          let target = args.GetPosition(canvas).vec2f
          let delta = (source - target) / (float32 image.Width * (!view.controller.deep).zoom) * if view.mainMode = QuarterView then 0.25f else 1.0f
          iteratePanZoomViews(fun panView ->
            panView.panDelta.modify((+) <| Vec3f(delta.x, delta.y, 0.0f))
            panView.wakeAndPost(updateInfo)
            )
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
          match !toolMode with
          | PanZoomTool ->
            let delta = if args.Delta > 0 then 1.1f else 1.0f / 1.1f
            iteratePanZoomViews(fun zoomView ->
              zoomView.zoomFactor.modify((*) delta)
              zoomView.wakeAndPost(updateInfo)
              )
          | _ ->
            let delta = float32 (sign args.Delta) * 0.01f / (!view.controller.deep).zoom
            iteratePanZoomViews(fun panView ->
              panView.panDelta.modify((+) <| Vec3f(0.0f, 0.0f, delta))
              panView.wakeAndPost(updateInfo)
              )
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
            iteratePanZoomViews(fun zoomView ->
              zoomView.controller.alter(View.transform(centerX = SelectFloat (average x0' x1' |> float),
                                                       centerY = SelectFloat (average y0' y1' |> float),
                                                       zoom = SelectFloat (1.0 / float (y1' - y0'))))
              if zoomView === view then updateInfo()
              )
          dragShape.Visibility <- Visibility.Collapsed

        userAction := Idle
        image.ReleaseMouseCapture()
        args.Handled <- true
        )

      image.ContextMenuOpening.Add(fun (args : ContextMenuEventArgs) ->
        // Do not open the context menu while the mouse is captured (something bad will happen if we open it).
        match !userAction with
        | Zooming(_, _) | Panning(_, _) ->
          args.Handled <- true
        | _ ->
          // Disable most items if the view is empty.
          if view.isEmpty then
            for item in menu.Items do
              match item with
              | :? MenuItem as item -> item.IsEnabled <- unbox item.Header = "Randomize"
              | _ -> ()
          else
            for item in menu.Items do
              match item with
              | :? MenuItem as item -> item.IsEnabled <- true
              | _ -> ()
        )

      )

    let menuPanel = StackPanel(Orientation = Orientation.Horizontal, Background = menuBg)
    menuPanel.VerticalAlignment <- VerticalAlignment.Center
    menuPanel.HorizontalAlignment <- HorizontalAlignment.Stretch
    let menu = Menu(Background = menuItemBg, IsMainMenu = true, Margin = Thickness(0.0))

    let makeTopMenuItem label =
      MenuItem(Header = label, Margin = Thickness(4.0, 0.0, 4.0, 0.0), Background = Wpf.brush(1.0, 1.0, 1.0, 0.1), Foreground = Wpf.brush(0.0))

    let viewMenu = Menu(Background = menuItemBg, Margin = Thickness(0.0))
    let filterMenu = makeTopMenuItem "Filters"

    let minDetailItem = MenuItem(Header = "Minimum Detail Level")
    let minDetailAnyItem = MenuItem(Header = "any", IsCheckable = true)
    minDetailItem.add(minDetailAnyItem)
    let minDetail20Item = MenuItem(Header = "20 px", IsCheckable = true)
    minDetailItem.add(minDetail20Item)
    let minDetail50Item = MenuItem(Header = "50 px", IsCheckable = true)
    minDetailItem.add(minDetail50Item)
    let minDetail100Item = MenuItem(Header = "100 px", IsCheckable = true)
    minDetailItem.add(minDetail100Item)
    let minDetail200Item = MenuItem(Header = "200 px", IsCheckable = true)
    minDetailItem.add(minDetail200Item)

    let setMinDetailLevel level =
      mapFilter.minDetail <- level
      minDetailAnyItem.IsChecked <- (level = 0.0f)
      minDetail20Item.IsChecked <- (level = 20.0f)
      minDetail50Item.IsChecked <- (level = 50.0f)
      minDetail100Item.IsChecked <- (level = 100.0f)
      minDetail200Item.IsChecked <- (level = 200.0f)

    minDetailAnyItem.Click.Add(fun _ -> setMinDetailLevel 0.0f)
    minDetail20Item.Click.Add(fun _ -> setMinDetailLevel 20.0f)
    minDetail50Item.Click.Add(fun _ -> setMinDetailLevel 50.0f)
    minDetail100Item.Click.Add(fun _ -> setMinDetailLevel 100.0f)
    minDetail200Item.Click.Add(fun _ -> setMinDetailLevel 200.0f)
    filterMenu.add(minDetailItem)

    let maxDetailItem = MenuItem(Header = "Maximum Detail Level")
    let maxDetail500Item = MenuItem(Header = "500 px", IsCheckable = true)
    maxDetailItem.add(maxDetail500Item)
    let maxDetail1000Item = MenuItem(Header = "1000 px", IsCheckable = true)
    maxDetailItem.add(maxDetail1000Item)
    let maxDetail2000Item = MenuItem(Header = "2000 px", IsCheckable = true)
    maxDetailItem.add(maxDetail2000Item)
    let maxDetail4000Item = MenuItem(Header = "4000 px", IsCheckable = true)
    maxDetailItem.add(maxDetail4000Item)
    let maxDetail8000Item = MenuItem(Header = "8000 px", IsCheckable = true)
    maxDetailItem.add(maxDetail8000Item)
    let maxDetailUnlimitedItem = MenuItem(Header = "unlimited")
    maxDetailItem.add(maxDetailUnlimitedItem)

    let setMaxDetailLevel level =
      mapFilter.maxDetail <- level
      maxDetail500Item.IsChecked <- (level = 500.0f)
      maxDetail1000Item.IsChecked <- (level = 1000.0f)
      maxDetail2000Item.IsChecked <- (level = 2000.0f)
      maxDetail4000Item.IsChecked <- (level = 4000.0f)
      maxDetail8000Item.IsChecked <- (level = 8000.0f)
      maxDetailUnlimitedItem.IsChecked <- (level = infinityf)

    maxDetail500Item.Click.Add(fun _ -> setMaxDetailLevel 500.0f)
    maxDetail1000Item.Click.Add(fun _ -> setMaxDetailLevel 1000.0f)
    maxDetail2000Item.Click.Add(fun _ -> setMaxDetailLevel 2000.0f)
    maxDetail4000Item.Click.Add(fun _ -> setMaxDetailLevel 4000.0f)
    maxDetail8000Item.Click.Add(fun _ -> setMaxDetailLevel 8000.0f)
    maxDetailUnlimitedItem.Click.Add(fun _ -> setMaxDetailLevel infinityf)
    filterMenu.add(maxDetailItem)

    let fileMenu = makeTopMenuItem "_File"

    let openItem = MenuItem(Header = "_Open..")
    openItem.Click.Add(fun _ ->
      let view = !focusView >? match !viewMode with | FullView -> fullView | HalfView -> halfView.[0] | QuarterView -> quarterView.[0]
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
            // SerializerSource works by regenerating the specimen we want to serialize.
            // During generation it obtains parameters in user readable units.
            source.generate(deepGenerator) |> ignore
            use stream = new System.IO.StreamWriter(dialog.FileName)
            stream.Write(source.yamlString)
            stream.Close()
          with
            | ex -> MessageBox.Show(ex.ToString()) |> ignore
      | None -> ()
      )
    fileMenu.add(saveItem)

    let exportItem = MenuItem(Header = "Export PNG Image")

    let addExportItem resolution =
      let exportResolutionItem = MenuItem(Header = sprintf "%d x %d" resolution resolution)
      exportResolutionItem.Click.Add(fun _ ->
        match !focusView with
        | Some(view) ->
          let map = !view.controller.deep
          exportMap3Png map.map resolution resolution map.camera
        | None -> ()
        )
      exportItem.add(exportResolutionItem)

    addExportItem 512
    addExportItem 1024
    addExportItem 1920
    addExportItem 2048
    addExportItem 3840
    addExportItem 4096
    addExportItem 8192

    fileMenu.add(exportItem)

    let quitItem = MenuItem(Header = "Quit")
    fileMenu.add(quitItem)
    quitItem.Click.Add(fun _ -> window.Close())

    // Disable save & export if there is no focus or if the focused view is empty.
    fileMenu.SubmenuOpened.Add(fun (args : RoutedEventArgs) ->
      if (!focusView).isNoneOr(fun view -> view.isEmpty) then
        for item in fileMenu.Items do
          match item with
          | :? MenuItem as item -> item.IsEnabled <- item <>= exportItem && item <>= saveItem
          | _ -> ()
      else
        for item in fileMenu.Items do
          match item with
          | :? MenuItem as item -> item.IsEnabled <- true
          | _ -> ()
      )

    menu.add(fileMenu)

    let toolsMenu = makeTopMenuItem "_Tools"
    let logItem = MenuItem(Header = "Log")
    logItem.Click.Add(fun _ ->
      match !logWindow with
      | Some(logWindow) when logWindow.isOpen -> logWindow.show()
      | _ -> logWindow := Some(LogWindow())
      )
    toolsMenu.add(logItem)
    menu.add(toolsMenu)

    let helpMenu = makeTopMenuItem "_Help"

    let manualItem = MenuItem(Header = "Online Manual")
    manualItem.Click.Add(fun _ ->
      System.Diagnostics.Process.Start("https://cdn.rawgit.com/SamiPerttu/FsMap3/master/docs/UserGuide.html") |> ignore
      )
    helpMenu.add(manualItem)

    let githubItem = MenuItem(Header = "GitHub Page")
    githubItem.Click.Add(fun _ ->
      System.Diagnostics.Process.Start("https://github.com/SamiPerttu/FsMap3") |> ignore
      )
    helpMenu.add(githubItem)

    let aboutItem = MenuItem(Header = "About..")
    aboutItem.Click.Add(fun _ ->
      let bold = FontWeight.FromOpenTypeWeight(900)
      let medium = FontWeight.FromOpenTypeWeight(500)
      let effect = Effects.DropShadowEffect(BlurRadius = 3.0, Color = Wpf.color(1.0), Opacity = 1.0, ShadowDepth = 0.0)
      let map =
        let data = [|
          DnaData("ZSjV4+ZJwJ10-1lcK0ZRQ10+03-ac3a0+-KSC0+nwml++VRah+-beny0-QY660-OMGC0001YFuyXvZGbHq+YUAoyv2ZHJuV0Zp0lt00ZvOAd0Y5KfMA0ZtGet010dZFMGb0-vMky008701ZK8nC0YENchw-Pvmq04+cFxm0ZaTNW+ZQlHo++k3lO0-nxay0ZOl+h0-InEZ+ZjlbV0008302-OXJ7m+mkAi0YKlGOw4YvVTOfYQOdavZV-l6++s3ae0-nxay0ZySyp0-gthW0ZjlbV002+ZMO20+haza0"B)
          |]
        data.[rnd.int(data.size)].generate(Map3Dna.generateExplorerMap)
      let w = 600.0
      let h = 300.0
      let bgImage = Image(Width = w, Height = h)
      let bgView = PixmapView(bgImage, int w, int h)
      bgView.start(RichMap3.pixmapSource(map))
      let aboutWindow = Window(Title = "About FsMap3 Explorer", SizeToContent = SizeToContent.WidthAndHeight, ResizeMode = ResizeMode.NoResize)
      let aboutCanvas = Canvas(Width = w, Height = h)
      aboutCanvas.add(bgImage, 0.0, 0.0)
      let title = Label(Content = "FsMap3 Explorer", FontSize = 40.0, FontWeight = bold, Effect = effect)
      let version = Label(Content = "Version " + Map3Dna.ExplorerVersion, FontSize = 16.0, FontWeight = bold, Effect = effect)
      aboutCanvas.add(title, 10.0, 10.0)
      aboutCanvas.add(version, 12.0, 60.0)
      let copyright = Label(Content = "© Copyright 2016 Sami Perttu", FontSize = 20.0, FontWeight = medium, Effect = effect)
      aboutCanvas.add(copyright, 30.0, 120.0)
      let license = Label(Content = "This program is distributed under the MIT license.", FontSize = 16.0, FontWeight = medium, Effect = effect)
      aboutCanvas.add(license, 30.0, 150.0)
      let license2 = Label(Content = "See the file LICENSE.md for more details.", FontSize = 16.0, FontWeight = medium, Effect = effect)
      aboutCanvas.add(license2, 30.0, 172.0)
      let closeButton = Button(Content = "Close", Width = 100.0, Height = 25.0, Background = Wpf.brush(1.0, 1.0, 1.0, 0.3), withClick = fun _ -> aboutWindow.Close())
      aboutCanvas.add(closeButton, 490.0, 265.0)
      aboutWindow.Content <- aboutCanvas
      aboutWindow.ShowDialog() |> ignore
      bgView.stop()
      )
    helpMenu.add(aboutItem)

    menu.add(filterMenu)

    menu.add(helpMenu)

    menuPanel.add(menu)
    
    let randomizeAllButton = Button(Content = "Randomize All", Background = toolBg, Margin = Thickness(1.0, 2.0, 1.0, 1.0))
    randomizeAllButton.Click.Add(fun _ -> randomizeAll())
    toolPanel.add(randomizeAllButton, Dock.Top)
    toolPanel.add(Separator(Margin = Thickness(0.0, 6.0, 0.0, 0.0)), Dock.Top)

    toolPanel.add(Label(Content = "Mutation mode"), Dock.Top)
    let mutationModeBox = ComboBox(Background = toolBg, Margin = Thickness(1.0))
    mutationModeBox.add(ComboBoxItem(Content = "Everything", IsSelected = true, withSelected = fun _ -> mutateMode := Everything))
    mutationModeBox.add(ComboBoxItem(Content = "Colors and Effects", withSelected = fun _ -> mutateMode := ColorsEffects))
    mutationModeBox.add(ComboBoxItem(Content = "Scales and Offsets", withSelected = fun _ -> mutateMode := ScalesOffsets))
    mutationModeBox.add(ComboBoxItem(Content = "Details", withSelected = fun _ -> mutateMode := Details))
    mutationModeBox.add(ComboBoxItem(Content = "Choose at Random", withSelected = fun _ -> mutateMode := ExplorerMutateMode.Random))
    toolPanel.add(mutationModeBox, Dock.Top)

    let layoutModeBox = ComboBox(Background = toolBg, Margin = Thickness(1.0))
    for i = 0 to layoutChoices.last do
      layoutModeBox.add(ComboBoxItem(Content = layoutChoices.name(i), IsSelected = (layoutChoices.weight(i) > 1.0), withSelected = fun _ -> layoutMode := layoutChoices.value(i)))

    toolPanel.add(Separator(Margin = Thickness(0.0, 6.0, 0.0, 0.0)), Dock.Top)
    toolPanel.add(Label(Content = "Default layout mode"), Dock.Top)
    toolPanel.add(layoutModeBox, Dock.Top)
    toolPanel.add(Separator(Margin = Thickness(0.0, 6.0, 0.0, 6.0)), Dock.Top)
    toolPanel.add(viewBar, Dock.Top)

    toolPanel.add(Separator(Margin = Thickness(0.0, 6.0, 0.0, 6.0)), Dock.Top)
    toolPanel.add(toolBar, Dock.Top)

    toolPanel.add(mapInfoBox.panel, Dock.Bottom)

    let splitter = GridSplitter(Width = 4.0, Margin = Thickness(0.0), Padding = Thickness(0.0), Background = splitterBg, VerticalAlignment = VerticalAlignment.Stretch, HorizontalAlignment = HorizontalAlignment.Center)

    dnaView.treeView.HorizontalAlignment <- HorizontalAlignment.Stretch
    dnaView.treeView.VerticalAlignment <- VerticalAlignment.Stretch

    let mainPanel = Grid()
    mainPanel.RowDefinitions.Add(RowDefinition(Height = GridLength.Auto))
    mainPanel.RowDefinitions.Add(RowDefinition())
    mainPanel.ColumnDefinitions.Add(ColumnDefinition(Width = GridLength.Auto))
    mainPanel.ColumnDefinitions.Add(ColumnDefinition(Width = GridLength.Auto))
    mainPanel.ColumnDefinitions.Add(ColumnDefinition(Width = GridLength.Auto))
    mainPanel.ColumnDefinitions.Add(ColumnDefinition(Width = GridLength(1.0, GridUnitType.Star)))
    mainPanel.add(menuPanel, 0, 0, 4, 1)
    mainPanel.add(dnaView.treeView, 0, 1)
    mainPanel.add(splitter, 1, 1)
    mainPanel.add(toolPanel, 2, 1)
    mainPanel.add(canvas, 3, 1)

    // Create an invisible label to take up some initial space in the Dna view column.
    mainPanel.add(Label(Content = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", Height = 0.0, Visibility = Visibility.Hidden), 0, 1)

    canvas.setPixelUnits()

    window.Content <- mainPanel
    window.Closed.Add(fun _ ->
      iterateViews(fun view _ -> view.stop())
      (!logWindow).apply(fun logWindow -> logWindow.close())
      )
    window.LayoutUpdated.Add(fun _ -> layoutCanvas())

    window.Show()

    iterateViews(fun view _ ->
      view.view.start()
      )

    initialDnaSource.apply(fun source ->
      fullView.controller.generate(true, dnaSource = source)
      updateDna()
      updateInfo()
      )

    setToolMode PanTool
    setViewMode FullView
    setMinDetailLevel 50.0f
    setMaxDetailLevel 1000.0f

   
(*
TODO

-Check CIELch conversion, the palette looks a little weird.
-Modify InteractiveSource & stuff so that editing of node tree becomes easier. E.g., delete parent, insert node...
-Never tile pattern atlases, there is no need. Possibility: temporary Dna injector.
-Figure out whether atlases are even a good idea. 
-Figure out a nice way of setting Map3Info sampling diameter based on layout.
-Add .dds export for 2-D and 3-D textures.
-Add Map3 display modes: rectangle, depth slices, depth strip, sphere, spiral?
-Supporting Undo? Or maybe History? I guess we just store past Dnas in a global list?
-Add either hover options or extra buttons to parameters in Dna view:
 -display range of parameter values in a gradient, pick new value by clicking.
 -lock parameter.
 -increase chance of mutation of parameter?
-Is animation support possible?
-Add screen space filters to PixmapView. These would be run when the first mosaic level is computed, to decide
 whether to display and continue or reject and stop.
-Add file name info to views to make a "Save" menu item possible. Display file name in window title.

*)


(*
0.3.0 Binary Release TODO

-Presets.
-View info box: add at least detail level. Maybe colored bars representing histogram as well -
 we can represent two dimensions at once in a bar, so could have (average hue + saturation) + value, saturation + hue?
-Rewrite scatter shape. Revisit bleed & shift.

*)
