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
    /// How many pixels user has to move the mouse before it is recognized as zooming.
    let dragMinimum = 16.0f
    /// Width of tool bar panel.
    let toolPanelWidth = 150.0

    // We have 1 full view, 1 center half view, 4 half views, and 16 quarter views.
    let fN = 1
    let cN = 1
    let hN = 4
    let qN = 16

    let rnd = Rnd(timeSeed())

    let currentCanvasWidth = ref 0
    let currentCanvasHeight = ref 0

    /// Which PixmapViews are visible depends on the mode.
    let guiMode = ref ExplorerViewMode.FullView

    /// Current tool.
    let toolMode = ref ExplorerTool.MutateTool

    /// Current default layout.
    let layoutMode = ref Layout.Hifi

    /// Current mutation mode.
    let mutateMode = ref Everything

    /// Current user action.
    let userAction = ref Idle

    let window = Window(Title = "Map3 Explorer", ResizeMode = ResizeMode.CanResize, Width = 1024.0, Height = 512.0, SizeToContent = SizeToContent.Manual, Topmost = false, WindowStartupLocation = WindowStartupLocation.CenterScreen)

    let canvas = Grid(Background = Brushes.Black, ClipToBounds = true, Margin = Thickness(0.0, 0.0, 0.0, 0.0))
    canvas.ColumnDefinitions.Add(ColumnDefinition())
    canvas.RowDefinitions.Add(RowDefinition())
    canvas.HorizontalAlignment <- HorizontalAlignment.Stretch
    canvas.VerticalAlignment <- VerticalAlignment.Stretch

    let mapInfoBox = RichMap3InfoBox.create(toolPanelWidth)
    mapInfoBox.panel.HorizontalAlignment <- HorizontalAlignment.Center
    mapInfoBox.panel.VerticalAlignment <- VerticalAlignment.Bottom

    let mapSeed = Map3.zero
    let richSeed = { RichMap3.map = mapSeed; center = Vec3f(0.5f); zoom = 1.0f; aspectRatio = 1.0f; info = Map3Info.create(mapSeed) }
    let deepGenerator = RichMap3.generate(generateExplorerMap)
    let pixmapGenerator extraTransform =
      match extraTransform with
      | Some(transform) -> RichMap3.pixmapGenerator(transform)
      | None -> RichMap3.pixmapGenerator()
    let deepFilter = RichMap3.filter

    let fullView = ExplorerView.create(FullView, richSeed, deepGenerator, pixmapGenerator None, deepFilter, canvas, true, 4)

    let centerView = ExplorerView.create(CenterView, richSeed, deepGenerator, pixmapGenerator None, deepFilter, canvas, false, 4)

    let halfXY i = (i % 2, i / 2)
    let halfView = Array.init hN (fun i ->
      ExplorerView.create(HalfView, richSeed, deepGenerator, pixmapGenerator None, deepFilter, canvas, false, 4)
      )

    let quarterXY i = (i % 4, i / 4)
    let quarterView = Array.init qN (fun i ->
      let x, y = quarterXY i
      let mosaicTransform (v : Vec3f) = Vec3f((v.x + float32 x) / 4.0f, (v.y + float32 y) / 4.0f, v.z)
      ExplorerView.create(QuarterView, richSeed, deepGenerator, pixmapGenerator (Some mosaicTransform), deepFilter, canvas, false, 3)
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

        centerView.image.Width <- float (R * 2)
        centerView.image.Height <- float (R * 2)
        centerView.image.Margin <- Thickness(Left = float (x0 + R + viewBorder), Top = float (y0 + R + viewBorder))
        centerView.view.setRenderSize(R * 2, R * 2)

        halfView |> Array.iteri (fun i view ->
          let image = view.image
          image.Width <- float (R * 2)
          image.Height <- float (R * 2)
          let x, y = halfXY i
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
          let x, y = quarterXY i
          image.Margin <- Thickness(Left = float (x0 + x * R + viewBorder), Top = float (y0 + y * R + viewBorder))
          let shape = !view.focusShape
          shape.Width <- float (R + viewBorder * 2)
          shape.Height <- float (R + viewBorder * 2)
          shape.Margin <- Thickness(Left = float (x0 + x * R), Top = float (y0 + y * R))
          view.view.setRenderSize(R, R)
          )
     
    let toolPanel = DockPanel(Width = toolPanelWidth, Margin = Thickness(1.0))
    toolPanel.VerticalAlignment <- VerticalAlignment.Stretch

    let iconSize = 26.0
    let iconMargin = Thickness(1.0)
    let createIconButton imageFile tip = ToggleButton(Content = Wpf.loadImage(imageFile), withToolTip = tip, Width = iconSize, Height = iconSize, Margin = iconMargin)

    let toolBar = StackPanel(Orientation = Orientation.Horizontal)
    let panButton = createIconButton "appbar.cursor.move.png" "Pan Tool: drag the view."
    toolBar.add(panButton)
    let zoomButton = createIconButton "appbar.magnify.png" "Zoom Tool: select an area to zoom into."
    toolBar.add(zoomButton)
    let mutateButton = createIconButton "appbar.diagram.png" "Mutate Tool: make other views mutations of chosen view."
    toolBar.add(mutateButton)
    let joltButton = createIconButton "appbar.camera.flash.png" "Jolt Tool: mutate the view."
    toolBar.add(joltButton)

    let setToolMode mode =
      mutateButton.IsChecked <- Nullable((mode = MutateTool))
      zoomButton.IsChecked <- Nullable((mode = ZoomTool))
      panButton.IsChecked <- Nullable((mode = PanTool))
      joltButton.IsChecked <- Nullable((mode = JoltTool))
      toolMode := mode

    mutateButton.PreviewMouseDown.Add(fun args -> setToolMode MutateTool; args.Handled <- true)
    zoomButton.PreviewMouseDown.Add(fun args -> setToolMode ZoomTool; args.Handled <- true)
    panButton.PreviewMouseDown.Add(fun args -> setToolMode PanTool; args.Handled <- true)
    joltButton.PreviewMouseDown.Add(fun args -> setToolMode JoltTool; args.Handled <- true)

    let viewBar = StackPanel(Orientation = Orientation.Horizontal)
    let fullViewButton = createIconButton "fullview.png" "1 Big View"
    viewBar.add(fullViewButton)
    let halfViewButton = createIconButton "halfview.png" "2x2 Small Views"
    viewBar.add(halfViewButton)
    let quarterViewButton = createIconButton "quarterview.png" "4x4 Mosaic View"
    viewBar.add(quarterViewButton)

    /// Which half view did we maximize last?
    let minimizeHalfView = ref halfView.[0]
    /// Which quarter view did we maximize last?
    let minimizeQuarterView = ref quarterView.[0]

    let focusView = ref (Some(fullView))

    /// Iterates over views. The second argument tells whether the view is visible.
    let iterateViews (f : ExplorerView<_> -> bool -> unit) =
      match !guiMode with
      | FullView ->
        f fullView true
        f centerView false
        for view in halfView do f view false
        for view in quarterView do f view false
      | CenterView ->
        f fullView false
        f centerView true
        for view in halfView do f view false
        for i = 0 to qN - 1 do
          let x, y = quarterXY i
          f quarterView.[i] (min x y > 0 && max x y < 3)
      | HalfView ->
        f fullView false
        f centerView false
        for view in halfView do f view true
        for view in quarterView do f view false
      | QuarterView ->
        f fullView false
        f centerView false
        for view in halfView do f view false
        for view in quarterView do f view true

    let dnaView = DnaView(viewFilter = fun parameter ->
      // The layout is generated once at the top level and then injected as a constraint.
      if parameter.name = "Layout" && parameter.level > 0 then
        Hidden
      // View parameters are manipulated using the view controls.
      elif parameter.name.StartsWith("View") then
        Hidden
      else Editable
      )

    /// Updates the Dna view. This can be called from any thread.
    let updateDna() =
      match !focusView with
      | Some(view) ->
        let dna = Dna.createCopy(!view.controller.dna)
        Wpf.dispatch(window, fun _ -> dnaView.update(dna))
      | None ->
        Wpf.dispatch(window, fun _ -> dnaView.reset())

    /// Updates the map info box. This can be called from any thread.
    let updateInfo() =
      Wpf.dispatch(window, fun _ ->
        match !focusView with
        | Some(view) ->
          let map = !view.controller.deep
          if map <>= richSeed then
            mapInfoBox.update(!view.controller.deep)
          else
            mapInfoBox.reset()
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

    let setGuiMode mode =
      if !guiMode <> mode then
        guiMode := mode
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

    let maximizeView view =
      fullView.view.reset()
      fullView.controller.copyFrom(view.controller)
      setFocus fullView
      setGuiMode FullView
      match view.mainMode with
      | HalfView -> minimizeHalfView := view
      | QuarterView -> minimizeQuarterView := view
      | _ -> ()

    let minimizeView view =
      let targetView = !minimizeHalfView
      targetView.view.reset()
      targetView.controller.copyFrom(view.controller)
      setFocus targetView
      setGuiMode HalfView

    let mosaicifyView view =
      let targetView = !minimizeQuarterView
      targetView.view.reset()
      targetView.controller.copyFrom(view.controller)
      setFocus targetView
      setGuiMode QuarterView

    fullViewButton.Click.Add(fun _ ->
      match !guiMode with
      | HalfView | QuarterView ->
        match !focusView with
        | Some(view) -> maximizeView view
        | None -> clearFocus(); setGuiMode FullView
      | _ -> ()
      )

    halfViewButton.Click.Add(fun _ ->
      match !guiMode with
      | FullView -> minimizeView fullView
      | QuarterView ->
        match !focusView with
        | Some(view) -> minimizeView view
        | None -> clearFocus(); setGuiMode HalfView
      | _ -> ()
      )

    quarterViewButton.Click.Add(fun _ ->
      match !guiMode with
      | FullView -> mosaicifyView fullView
      | HalfView ->
        match !focusView with
        | Some(view) -> mosaicifyView view
        | None -> clearFocus(); setGuiMode QuarterView
      | _ -> ()
      )

    // Add context menus and event handlers to images.
    iterateViews (fun view _ ->

      let menu = ContextMenu(Placement = Primitives.PlacementMode.Mouse)

      if view.mainMode = HalfView || view.mainMode = QuarterView then
        let maximizeItem = MenuItem(Header = "Maximize")
        maximizeItem.Click.Add(fun _ -> maximizeView view)
        menu.add(maximizeItem)
      elif view.mainMode = FullView then
        let minimizeItem = MenuItem(Header = "Minimize")
        minimizeItem.Click.Add(fun _ -> minimizeView view)
        menu.add(minimizeItem)
      if view.mainMode = FullView || view.mainMode = HalfView then
        let mosaicifyItem = MenuItem(Header = "View Mosaic")
        mosaicifyItem.Click.Add(fun _ -> mosaicifyView view)
        menu.add(mosaicifyItem)

      let openNewWindow = MenuItem(Header = "Open in New Window..")
      openNewWindow.Click.Add(fun _ ->
        if !view.controller.deep <>= richSeed then
          let dna = !view.controller.dna
          Explorer.start(DnaData(dna))
        )
      menu.add(openNewWindow)

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
            // Mutate other half or mosaic views.
            if (view.mainMode = HalfView || view.mainMode = QuarterView) && !view.controller.deep <>= richSeed then view.post(fun _ ->
              for targetView in (if view.mainMode = HalfView then halfView else quarterView) do
                if targetView <>= view then
                  let predicate = View.mutationPredicate(rnd, view, !mutateMode)
                  targetView.controller.mutateFrom(view.controller, predicate)
              )
          else
            setFocus view

        | JoltTool ->
          if (!focusView).isSomeAnd((===) view) then
            // Mutate the view.
            if !view.controller.deep <>= richSeed then view.post(fun _ ->
              let predicate = View.mutationPredicate(rnd, view, !mutateMode)
              view.controller.mutateFrom(view.controller, predicate)
              updateDna()
              updateInfo()
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
          match view.mainMode with
          | QuarterView ->
            // TODO. Panning all 16 views at once is too much for our program. What we could do instead is
            // pan up to 2x2 views, depending on the position of the mouse when the left button is pressed:
            // near an edge, include the neighboring view(s) into the pan.
            let delta = delta * 0.25f
            let qview = view
            //for qview in quarterView do
            qview.panDelta.modify((+) <| Vec3f(delta.x, delta.y, 0.0f))
            qview.wakeAndPost(updateInfo)
          | _ ->
            view.panDelta.modify((+) <| Vec3f(delta.x, delta.y, 0.0f))
            view.wakeAndPost(updateInfo)
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
          view.wakeAndPost(updateInfo)
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
              view.controller.alter(View.transform(centerX = SelectFloat (average x0' x1' |> float),
                                                centerY = SelectFloat (average y0' y1' |> float),
                                                zoom = SelectFloat (1.0 / float (y1' - y0'))))
              updateInfo()
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

    let menuPanel = StackPanel(Orientation = Orientation.Horizontal, Background = Wpf.verticalBrush(Wpf.color(0.8), Wpf.color(1.0)))
    menuPanel.VerticalAlignment <- VerticalAlignment.Center
    menuPanel.HorizontalAlignment <- HorizontalAlignment.Stretch
    let menu = Menu()

    let fileMenu = MenuItem(Header = "_File")

    let openItem = MenuItem(Header = "_Open..")
    openItem.Click.Add(fun _ ->
      let view = !focusView >? match !guiMode with | FullView -> fullView | CenterView -> centerView | HalfView -> halfView.[0] | QuarterView -> quarterView.[0]
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
            source.generate(deepGenerator) |> ignore
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
    toolPanel.add(randomizeAllButton, Dock.Top)
    toolPanel.add(Separator(Margin = Thickness(0.0, 6.0, 0.0, 0.0)), Dock.Top)
    toolPanel.add(Label(Content = "Mutation mode"), Dock.Top)
    let mutationModeBox = ComboBox()
    mutationModeBox.add(ComboBoxItem(Content = "Everything", IsSelected = true, withSelected = fun _ -> mutateMode := Everything))
    mutationModeBox.add(ComboBoxItem(Content = "Colors and Effects", withSelected = fun _ -> mutateMode := ColorsEffects))
    mutationModeBox.add(ComboBoxItem(Content = "Scales and Offsets", withSelected = fun _ -> mutateMode := ScalesOffsets))
    mutationModeBox.add(ComboBoxItem(Content = "Details", withSelected = fun _ -> mutateMode := Details))
    mutationModeBox.add(ComboBoxItem(Content = "Choose at Random", withSelected = fun _ -> mutateMode := ExplorerMutateMode.Random))
    toolPanel.add(mutationModeBox, Dock.Top)

    let layoutModeBox = ComboBox()
    for i = 0 to layoutChoices.last do
      layoutModeBox.add(ComboBoxItem(Content = layoutChoices.name(i), IsSelected = (layoutChoices.weight(i) > 1.0), withSelected = fun _ -> layoutMode := layoutChoices.value(i)))
    toolPanel.add(Separator(Margin = Thickness(0.0, 6.0, 0.0, 0.0)), Dock.Top)
    toolPanel.add(Label(Content = "Default layout mode"), Dock.Top)
    toolPanel.add(layoutModeBox, Dock.Top)

    toolPanel.add(Separator(Margin = Thickness(0.0, 6.0, 0.0, 6.0)), Dock.Top)
    toolPanel.add(viewBar, Dock.Top)

    toolPanel.add(Separator(Margin = Thickness(0.0, 6.0, 0.0, 6.0)), Dock.Top)
    toolPanel.add(toolBar, Dock.Top)

    toolPanel.add(mapInfoBox.container, Dock.Bottom)

    let splitter = GridSplitter(Width = 4.0, Margin = Thickness(0.0), Foreground = Wpf.brush(0.7), VerticalAlignment = VerticalAlignment.Stretch, HorizontalAlignment = HorizontalAlignment.Center)

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
    //window.KeyDown.Add(fun args -> if args.Key = System.Windows.Input.Key.Escape then window.Close())
    window.Closed.Add(fun _ ->
      iterateViews(fun view _ -> view.stop())
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

   
(*
TODO

-Figure out a better way to mutate in the mosaic view.
-Store generator version info in YAML.
-Add export in F# source format, i.e., DnaData constructor and function that invokes generator.
-Modify InteractiveSource & stuff so that editing of node tree becomes easier. E.g., delete parent, insert node...
-Never tile pattern atlases, there is no need.
 Possibility: temporary Dna injector.
-Add .dds export for 2-D and 3-D textures.
-Add Map3 display modes: rectangle, depth slices, depth strip, sphere?
-Supporting Undo? Or maybe History? I guess we just store past Dnas in a global list?
-Status line. Where do we put this? At the bottom? I think we don't want to use the menu bar for this.
 Instead, we can put something else in the menu bar if necessary.
-Add either hover options or extra buttons to parameters in Dna view:
 -display range of parameter values in a gradient, pick new value by clicking.
 -lock parameter.
 -increase chance of mutation of parameter?
-Main menu bar. We should put more stuff there.
 -Defaults: default normalization mode. default tiling mode.
 -Mutation mode belongs in tool options!
-Is animation support possible?
-Caching of subtree pixmaps is harder than caching of normalization info because call patterns
 need to be fingerprinted as well. Call patterns flow downstream from the root in Dna but not all
 drawn parameters have an effect on them. Look into it at some point.

*)
