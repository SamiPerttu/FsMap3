// FsMap3 Editor.
namespace Fuse

open System
open System.Windows
open System.Windows.Input
open System.Windows.Media
open System.Windows.Shapes
open System.Windows.Controls
open System.Windows.Controls.Primitives

open Common
open Basis3
open Map3Info
open Map3Dna
open Map3Gui


[<NoComparison; NoEquality>]
type EditorUserState =
  | Zooming of view : EditorView<RichMap3> * source : Vec2f
  | Panning of view : EditorView<RichMap3> * source : Vec2f
  | Idle



[<NoComparison; NoEquality>]
type ClipboardItem =
  {
    dna : Dna
    map : RichMap3
    filename : string
    presetFilename : string
  }



/// Interactive Map3 editor.
type Editor =

  static member start(?sourceView) =

    let settings = EditorSettings.read()

    let presetDirectory = "Presets"

    /// Minimum resolution of a view.
    let minimumR = 16
    /// View border thickness.
    let viewBorder = 3
    /// How far user has to move the mouse before it is recognized as dragging.
    let dragMinimum = 16.0f
    /// Width of tool bar panel.
    let toolPanelWidth = 152.0
    /// Size of tool and view icons.
    let iconSize = 29.0
    let iconMargin = Thickness(0.5)
    /// Map preview size in preview menu and create preview dialog.
    let previewSize = 160.0

    // Colors.
    let viewBg = Wpf.brush(0.0)
    let dnaBg = Wpf.brush(0.91)
    let menuBg = Wpf.verticalBrush(Wpf.color(0.8, 0.88, 0.9), Wpf.color(0.7, 0.78, 0.8))
    let topMenuBg = Wpf.brush(1.0, 1.0, 1.0, 0.1)
    let menuItemBg = Wpf.brush(1.0, 1.0, 1.0, 0.2)
    let toolBg = Wpf.brush(0.8, 0.8, 0.8, 0.3)
    let splitterBg = Wpf.brush(0.93)

    let rnd = Rnd(timeSeed())

    // Most recent view canvas (which is actually a Grid) dimensions used for laying out views.
    let currentCanvasWidth = ref 0
    let currentCanvasHeight = ref 0

    /// Which PixmapViews are visible depends on the mode.
    let viewMode = ref EditorViewMode.FullView

    /// Current tool.
    let toolMode = ref EditorTool.MutateTool

    /// Current default layout.
    let layoutMode = ref Layout.Hifi

    /// Current mutation mode.
    let mutateMode = ref Everything

    /// Current log window. Must be checked when accessing, as it could be closed already.
    let logWindow = ref none<LogWindow>

    /// Current user action.
    let userAction = ref Idle

    /// Whether map filtering is enabled.
    let mapFiltering = ref true

    /// Current copy buffer contents.
    let copyBuffer = ref none<ClipboardItem>

    let window = Window(Title = "FsMap3 Editor",
                        ResizeMode = ResizeMode.CanResize,
                        Width = 1024.0, Height = 480.0,
                        SizeToContent = SizeToContent.Manual,
                        Topmost = false,
                        WindowStartupLocation = WindowStartupLocation.CenterScreen)

    let createShortcut key modifier handler =
      let command = RoutedCommand()
      command.InputGestures.Add(KeyGesture(key, modifier)) |> ignore
      window.CommandBindings.Add(CommandBinding(command, fun _ _ -> handler())) |> ignore
      command

    let setMenuItemShortcut (item : MenuItem) key modifier handler =
      item.Command <- createShortcut key modifier handler

    let canvas = Grid(Background = viewBg, ClipToBounds = false, Margin = Thickness(0.0), HorizontalAlignment = HorizontalAlignment.Stretch, VerticalAlignment = VerticalAlignment.Stretch)
    canvas.ColumnDefinitions.Add(ColumnDefinition())
    canvas.RowDefinitions.Add(RowDefinition())

    let mapInfoBox = RichMap3InfoBox.create(toolPanelWidth)
    mapInfoBox.canvas.HorizontalAlignment <- HorizontalAlignment.Center
    mapInfoBox.canvas.VerticalAlignment <- VerticalAlignment.Bottom
    mapInfoBox.reset()

    let isTextureFile (file : string) =
      System.IO.Path.GetExtension(file) = ".yaml"

    let mapSeed = Map3.zero
    let pixmapSeed = {
      new IPixmapSource with
        member this.start(_, _) = ()
        member this.getPixel(w, h, x, y) =
          let u = float32 x / float32 (w - 1) * 2.0f - 1.0f
          let v = float32 y / float32 (h - 1) * 2.0f - 1.0f
          Vec3f(0.5f - 0.0666f * (squared u + squared v))
        member this.finish() = ()
        member this.postFx(_) = ()
      }
    let richSeed = RichMap3.zero

    let deepGenerator = fun (dna : Dna) -> Map3Dna.generateEditorMap (!mapFiltering) dna
    let pixmapGenerator = RichMap3.pixmapSource

    let mapFilter =
      { 
        RichMap3Filter.minDetail = 50.0f
        maxDetail = 1000.0f
        minDeviation = 0.1f
        minDifference = 0.01f
        maxDifference = infinityf
      }
    let deepFilter = fun map previous -> if !mapFiltering then mapFilter.filter(map, previous) else true

    let fullView = EditorView.create(FullView, 0, 0, 0, pixmapSeed, richSeed, deepGenerator, pixmapGenerator, deepFilter, canvas, true, 4)

    let halfView = Array.init 4 (fun i ->
      let x, y = i % 2, i / 2
      EditorView.create(HalfView, i, x, y, pixmapSeed, richSeed, deepGenerator, pixmapGenerator, deepFilter, canvas, false, 4)
      )

    let thirdView = Array.init 9 (fun i ->
      let x, y = i % 3, i / 3
      EditorView.create(ThirdView, i, x, y, pixmapSeed, richSeed, deepGenerator, pixmapGenerator, deepFilter, canvas, false, 3)
      )

    let notFullView = Array.append halfView thirdView

    for view in notFullView do
      view.createFocusShape(canvas)

    let dragShape = Rectangle(Visibility = Visibility.Collapsed, Stroke = Wpf.brush(0.0), Opacity = 0.3, StrokeThickness = 1.0, Fill = Brushes.Blue, SnapsToDevicePixels = true)
    canvas.add(dragShape, 0, 0)
    dragShape.Margin <- Thickness(0.0)
    dragShape.HorizontalAlignment <- HorizontalAlignment.Left
    dragShape.VerticalAlignment <- VerticalAlignment.Top

    // This must be called from the UI thread.
    let layoutCanvas() =

      let canvasWidth = int canvas.ActualWidth + 1
      let canvasHeight = int canvas.ActualHeight

      if canvasWidth <> !currentCanvasWidth || canvasHeight <> !currentCanvasHeight then

        currentCanvasWidth := canvasWidth
        currentCanvasHeight := canvasHeight

        let FR = max minimumR (min canvasWidth canvasHeight - 2 * viewBorder)
        let HR = max minimumR (min canvasWidth canvasHeight - 2 * viewBorder) / 2
        let TR = max minimumR (min canvasWidth canvasHeight - 2 * viewBorder) / 3

        let totalSize = FR + 2 * viewBorder
        let x0 = max 0 (int canvas.ActualWidth - totalSize) / 2
        let y0 = max 0 (int canvas.ActualHeight - totalSize) / 2

        Log.infof "Laying out view canvas. Full view resolution %d | half view resolution %d | third view resolution %d" FR HR TR

        fullView.image.Width <- float FR
        fullView.image.Height <- float FR
        fullView.image.Margin <- Thickness(Left = float (x0 + viewBorder), Top = float (y0 + viewBorder))
        fullView.layoutBusy()
        fullView.pixmapView.setRenderSize(FR, FR)

        halfView |> Array.iteri (fun i view ->
          let image = view.image
          image.Width <- float HR
          image.Height <- float HR
          let x, y = view.gridX, view.gridY
          image.Margin <- Thickness(Left = float (x0 + x * HR + viewBorder), Top = float (y0 + y * HR + viewBorder))
          let focusShape = !view.focusShape
          let innerShape = focusShape.[0]
          innerShape.Width <- float (HR + 2)
          innerShape.Height <- float (HR + 2)
          innerShape.Margin <- Thickness(Left = float (x0 + x * HR + viewBorder - 1), Top = float (y0 + y * HR + viewBorder - 1))
          let outerShape = focusShape.[1]
          outerShape.Width <- float (HR + 6)
          outerShape.Height <- float (HR + 6)
          outerShape.Margin <- Thickness(Left = float (x0 + x * HR + viewBorder - 3), Top = float (y0 + y * HR + viewBorder - 3))
          view.layoutBusy()
          view.pixmapView.setRenderSize(HR, HR)
         )

        thirdView |> Array.iteri (fun i view ->
          let image = view.image
          image.Width <- float TR
          image.Height <- float TR
          let x, y = view.gridX, view.gridY
          image.Margin <- Thickness(Left = float (x0 + x * TR + viewBorder), Top = float (y0 + y * TR + viewBorder))
          let focusShape = !view.focusShape
          let innerShape = focusShape.[0]
          innerShape.Width <- float (TR + 2)
          innerShape.Height <- float (TR + 2)
          innerShape.Margin <- Thickness(Left = float (x0 + x * TR + viewBorder - 1), Top = float (y0 + y * TR + viewBorder - 1))
          let outerShape = focusShape.[1]
          outerShape.Width <- float (TR + 6)
          outerShape.Height <- float (TR + 6)
          outerShape.Margin <- Thickness(Left = float (x0 + x * TR + viewBorder - 3), Top = float (y0 + y * TR + viewBorder - 3))
          view.layoutBusy()
          view.pixmapView.setRenderSize(TR, TR)
          )
     
    /// Iterates over all views. The second argument tells whether the view is visible.
    let iterateViews (f : EditorView<_> -> bool -> unit) =
      match !viewMode with
      | FullView ->
        f fullView true
        for view in halfView do f view false
        for view in thirdView do f view false
      | HalfView ->
        f fullView false
        for view in halfView do f view true
        for view in thirdView do f view false
      | ThirdView ->
        f fullView false
        for view in halfView do f view false
        for view in thirdView do f view true

    let toolPanel = DockPanel(Width = toolPanelWidth, Margin = Thickness(0.0), VerticalAlignment = VerticalAlignment.Stretch)

    let createIconButton imageFile tip = ToggleButton(Content = Wpf.loadImage(imageFile), withToolTip = tip, Width = iconSize, Height = iconSize, Margin = iconMargin, Background = Wpf.brush(0.0, 0.0, 0.0, 0.1))

    let toolBar = StackPanel(Orientation = Orientation.Horizontal, Margin = Thickness(1.0))
    let panButton = createIconButton "appbar.cursor.move.png" "Pan Tool: drag view to pan; mouse wheel controls Z."
    toolBar.add(panButton)
    let panZoomButton = createIconButton "pan-zoom-3.png" "Pan-Zoom Tool: drag view to pan; mouse wheel controls zoom."
    toolBar.add(panZoomButton)
    let zoomButton = createIconButton "appbar.magnify.png" "Zoom Tool: select an area to zoom into."
    toolBar.add(zoomButton)
    let mutateButton = createIconButton "appbar.diagram.png" "Mutate Tool: click view to make mutated copies of it."
    toolBar.add(mutateButton)
    let joltButton = createIconButton "appbar.camera.flash.png" "Jolt Tool: mutate the view."
    toolBar.add(joltButton)

    let setToolMode mode =
      panButton.IsChecked <- Nullable((mode = PanTool))
      panZoomButton.IsChecked <- Nullable((mode = PanZoomTool))
      zoomButton.IsChecked <- Nullable((mode = ZoomTool))
      mutateButton.IsChecked <- Nullable((mode = MutateTool))
      joltButton.IsChecked <- Nullable((mode = JoltTool))
      match mode with
      | PanTool | PanZoomTool -> iterateViews (fun view _ -> view.setCursors(Input.Cursors.SizeAll, null))
      | ZoomTool              -> iterateViews (fun view _ -> view.setCursors(Input.Cursors.Cross, null))
      | MutateTool            -> iterateViews (fun view _ -> view.setCursors((match view.mainMode with | FullView -> Input.Cursors.No | _ -> Input.Cursors.Hand), null))
      | JoltTool              -> iterateViews (fun view _ -> view.setCursors(Input.Cursors.Hand, null))
      toolMode := mode

    panButton.PreviewMouseDown.Add(fun args -> setToolMode PanTool; args.Handled <- true)
    panZoomButton.PreviewMouseDown.Add(fun args -> setToolMode PanZoomTool; args.Handled <- true)
    zoomButton.PreviewMouseDown.Add(fun args -> setToolMode ZoomTool; args.Handled <- true)
    mutateButton.PreviewMouseDown.Add(fun args -> setToolMode MutateTool; args.Handled <- true)
    joltButton.PreviewMouseDown.Add(fun args -> setToolMode JoltTool; args.Handled <- true)

    let viewBar = StackPanel(Orientation = Orientation.Horizontal, Margin = Thickness(1.0))
    let fullViewButton = createIconButton "fullview.png" "Big View"
    viewBar.add(fullViewButton)
    let halfViewButton = createIconButton "halfview.png" "Quad View"
    viewBar.add(halfViewButton)
    let thirdViewButton = createIconButton "thirdview.png" "Nono View"
    viewBar.add(thirdViewButton)

    /// Which half view did we maximize last?
    let minimizeHalfView = ref halfView.[0]
    /// Which third view did we maximize last?
    let minimizeThirdView = ref thirdView.[0]

    let focusView = ref (Some(fullView))

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
    dnaView.addVisualizerDependency(fun parameter -> match parameter.name with | "Weave" -> [(1, "Weave period")] | _ -> [])
    dnaView.addVisualizerDependency(fun parameter -> match parameter.name with | "Falloff shape" -> [(1, "Falloff saturation")] | _ -> [])

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

    /// Updates window title. This can be called from any thread.
    let updateTitle() =
      Wpf.dispatch(window, fun _ ->
        window.Title <- "FsMap3 Editor" + (match !focusView with | Some(view) when view.filename <> "" -> ": " + System.IO.Path.GetFileNameWithoutExtension(view.filename) | _ -> "")
        )

    let updateAll() =
      updateDna()
      updateInfo()
      updateTitle()

    /// Sets focus to the view and displays its Dna and info.
    let setFocus view' =
      if (!focusView).isNoneOr((<>=) view') then
        focusView := Some(view')
        Wpf.dispatch(window, fun _ ->
          for view in notFullView do
            view.focus(if view' === view then Visibility.Visible else Visibility.Hidden)
          )
      updateAll()

    /// Clears focus and resets the Dna view and the info box.
    let clearFocus() =
      focusView := None
      updateAll()
      Wpf.dispatch(window, fun _ ->
        for view in notFullView do
          view.focus(Visibility.Hidden)
        )

    /// This predicate creates a random map from scratch.
    let randomizePredicate _ (dna : Dna) i =
      if dna.[i].name = "View Zoom" then SelectFloat 1.0
      elif dna.[i].name.StartsWith("View Center") then SelectFloat 0.5
      elif dna.[i].name = "Layout" then Select (layoutChoices.numberOf((=) !layoutMode))
      else Randomize

    let setViewMode mode =
      Wpf.dispatch(window, fun _ ->
        viewMode := mode
        iterateViews (fun view visible -> view.setVisibility(if visible then Visibility.Visible else Visibility.Collapsed))
        fullViewButton.IsChecked <- Nullable((mode = FullView))
        halfViewButton.IsChecked <- Nullable((mode = HalfView))
        thirdViewButton.IsChecked <- Nullable((mode = ThirdView))
        )

    let controller = EditorController.create(setFocus, setViewMode, updateAll)

    dnaView.leftCallback <- fun dna i value ->
      match !focusView with
      | Some(view) ->
        controller.postAction(view, dna.[i].name, fun rnd dna' i' ->
          if i' = i && dna'.[i'].name = dna.[i].name then Select(value) else Retain
          )
      | None -> ()
    dnaView.dragCallback <- dnaView.leftCallback
    dnaView.releaseCallback <- fun _ _ -> controller.post(CloseAction)
    dnaView.rightCallback <- fun dna i ->
      match !focusView with
      | Some(view) ->
        controller.postAction(view, "Randomize " + dna.[i].name, fun rnd dna' i' ->
          if i' = i && dna'.[i'].name = dna.[i].name then Randomize else Retain
          )
        controller.post(CloseAction)
      | None -> ()
    dnaView.wheelCallback <- fun dna i delta ->
      match !focusView with
      | Some(view) ->
        controller.postAction(view, dna.[i].name, fun rnd dna' i' ->
          if i' = i && dna'.[i'].name = dna.[i].name then Adjust01(float (sign delta) * -0.02) else Retain
          )
      | None -> ()

    /// Copies the view to full view mode.
    let maximizeView (view : EditorView<RichMap3>) =
      if view.isEmpty = false then
        controller.post(ApplyMapCopy(fullView, view, "Maximize"))
      setViewMode FullView
      setFocus fullView
      match view.mainMode with
      | HalfView -> minimizeHalfView := view
      | ThirdView -> minimizeThirdView := view
      | _ -> ()

    /// Copies the view to quad view mode.
    let minimizeView (view : EditorView<RichMap3>) =
      let targetView = !minimizeHalfView
      setViewMode HalfView
      if view.isEmpty = false then
        controller.post(ApplyMapCopy(targetView, view, "Go to Quad View"))
        setFocus targetView
      else
        clearFocus()

    /// Copies the view to mosaic view mode.
    let thirdifyView (view : EditorView<RichMap3>) =
      let targetView = !minimizeThirdView
      setViewMode ThirdView
      if view.isEmpty = false then
        controller.post(ApplyMapCopy(targetView, view, "Go to Nono View"))
        setFocus targetView
      else
        clearFocus()

    fullViewButton.Click.Add(fun _ ->
      match !viewMode with
      | HalfView | ThirdView ->
        match !focusView with
        | Some(view) -> maximizeView view
        | _ -> setViewMode FullView; setFocus fullView
      | _ -> ()
      )

    halfViewButton.Click.Add(fun _ ->
      match !viewMode with
      | FullView -> minimizeView fullView
      | ThirdView ->
        match !focusView with
        | Some(view) -> minimizeView view
        | None -> clearFocus(); setViewMode HalfView
      | _ -> ()
      )

    thirdViewButton.Click.Add(fun _ ->
      match !viewMode with
      | FullView -> thirdifyView fullView
      | HalfView ->
        match !focusView with
        | Some(view) -> thirdifyView view
        | None -> clearFocus(); setViewMode ThirdView
      | _ -> ()
      )

    let loadYamlPreview (file : string) =
      try
        use stream = new System.IO.StreamReader(file)
        let readLine() = match stream.EndOfStream with | false -> Some(stream.ReadLine()) | true -> None
        let source = DeserializerSource(readLine)
        let map = source.generate(deepGenerator)
        stream.Close()
        Some(map)
      with
        | _ -> None

    let saveYaml setViewFilename setViewPresetFilename (file : string) =
      match !focusView with
      | Some(view) ->
        try
          let source = SerializerSource(!view.controller.dna)
          // SerializerSource works by regenerating the specimen we want to serialize.
          // During generation it obtains parameters in user readable units.
          source.generate(deepGenerator) |> ignore
          use stream = new System.IO.StreamWriter(file)
          stream.Write(source.yamlString)
          stream.Close()
          if setViewFilename then
            view.filename <- file
            updateTitle()
          if setViewPresetFilename then
            view.presetFilename <- file
        with
          | ex -> MessageBox.Show(ex.ToString()) |> ignore
      | _ -> ()

    let menuPanel = StackPanel(Orientation = Orientation.Horizontal, Background = menuBg)
    menuPanel.VerticalAlignment <- VerticalAlignment.Center
    menuPanel.HorizontalAlignment <- HorizontalAlignment.Stretch
    let menu = Menu(Background = menuItemBg, IsMainMenu = true, Margin = Thickness(0.0))

    let makeTopMenuItem label =
      MenuItem(Header = label, Margin = Thickness(4.0, 0.0, 4.0, 0.0), Background = topMenuBg, Foreground = Wpf.brush(0.0))

    let fileMenu = makeTopMenuItem "_File"

    let openView() = 
      !focusView >? match !viewMode with | FullView -> fullView | HalfView -> halfView.[0] | ThirdView -> thirdView.[0]

    let newWindowItem = MenuItem(Header = "New Window")
    setMenuItemShortcut newWindowItem Key.N ModifierKeys.Control (fun _ ->
      Editor.start()
      )
    fileMenu.add(newWindowItem)

    let openItem = MenuItem(Header = "_Open..")
    fileMenu.add(openItem)

    let openRecentItem = MenuItem(Header = "Open Recent")
    let rec refreshRecentItems() =
      openRecentItem.Items.Clear()
      for file in settings.recentFiles do
        let recentFileItem = MenuItem(Header = file)
        recentFileItem.Click.Add(fun _ ->
          let view = openView()
          setFocus view
          controller.post(ApplyMapLoad(view, "Open", file, false))
          settings.addRecent(file)
          refreshRecentItems()
          )
        openRecentItem.add(recentFileItem)
      if settings.recentFiles.size > 0 then
        openRecentItem.add(Separator())
        let clearRecentItem = MenuItem(Header = "Clear Recent Files")
        clearRecentItem.Click.Add(fun _ ->
          settings.recentFiles <- Array.empty
          refreshRecentItems()
          )
        openRecentItem.add(clearRecentItem)
    refreshRecentItems()
    fileMenu.add(openRecentItem)

    setMenuItemShortcut openItem Key.O ModifierKeys.Control (fun _ ->
      let dialog = new Microsoft.Win32.OpenFileDialog(Title = "Load Map File..", Filter = "YAML files (.yaml)|*.yaml")
      let result = dialog.ShowDialog()
      if result.HasValue && result.Value then
        let file = dialog.FileName
        let view = openView()
        setFocus view
        controller.post(ApplyMapLoad(view, "Open", file, false))
        settings.addRecent(file)
        refreshRecentItems()
      )

    let saveItem = MenuItem(Header = "Save")
    setMenuItemShortcut saveItem Key.S ModifierKeys.Control (fun _ ->
      match !focusView with
      | Some(view) when view.filename <> "" ->
        let file = view.filename
        saveYaml true false file
        settings.addRecent(file)
        refreshRecentItems()
      | _ -> ()
      )
    fileMenu.add(saveItem)

    let saveAsItem = MenuItem(Header = "Save As..")
    saveAsItem.Click.Add(fun _ ->
      match !focusView with
      | Some(view) ->
        let dialog = new Microsoft.Win32.SaveFileDialog(Title = "Save Map File As..",
                                                        AddExtension = true,
                                                        OverwritePrompt = true,
                                                        ValidateNames = true,
                                                        DefaultExt = ".yaml",
                                                        Filter = "YAML files (.yaml)|*.yaml")
        let result = dialog.ShowDialog()
        if result.HasValue && result.Value then
          let file = dialog.FileName
          settings.addRecent(file)
          refreshRecentItems()
          saveYaml true false file
      | None -> ()
      )
    fileMenu.add(saveAsItem)

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
    setMenuItemShortcut quitItem Key.Q ModifierKeys.Control (fun _ -> window.Close())

    // Enable file menu items selectively.
    fileMenu.SubmenuOpened.Add(fun (args : RoutedEventArgs) ->
      for item in fileMenu.Items do
        match item with
        | :? MenuItem as item -> item.IsEnabled <- true
        | _ -> ()
      match !focusView with
      | Some(view) when view.isEmpty = false ->
        if view.filename = "" then saveItem.IsEnabled <- false
      | _ ->
        exportItem.IsEnabled <- false
        saveItem.IsEnabled <- false
        saveAsItem.IsEnabled <- false
      openRecentItem.IsEnabled <- settings.recentFiles.size > 0
      )

    menu.add(fileMenu)

    let editMenu = makeTopMenuItem "_Edit"
    let undoItem = MenuItem(Header = "Undo")
    setMenuItemShortcut undoItem Key.Z ModifierKeys.Control (fun _ -> controller.postUndo())
    editMenu.add(undoItem)
    let redoItem = MenuItem(Header = "Redo")
    setMenuItemShortcut redoItem Key.Y ModifierKeys.Control (fun _ -> controller.postRedo())
    editMenu.add(redoItem)
    let copyItem = MenuItem(Header = "Copy")
    setMenuItemShortcut copyItem Key.C ModifierKeys.Control (fun _ ->
      match !focusView with
      | Some(view) ->
        // Copy is (by purpose) not an undoable action. Technically, we should still synchronize here
        // but I haven't gotten round to implementing it.
        copyBuffer := Some { dna = !view.controller.dna; map = !view.controller.deep; filename = view.filename; presetFilename = view.presetFilename }
      | None -> ()
      )
    editMenu.add(copyItem)
    let pasteItem = MenuItem(Header = "Paste")
    setMenuItemShortcut pasteItem Key.V ModifierKeys.Control (fun _ ->
      match !focusView, !copyBuffer with
      | Some(view), Some(item) ->
        controller.post(ApplyMapPaste(view, "Paste", item.filename, item.presetFilename, item.dna, item.map))
      | _ -> ()
      )

    // Set edit menu item states appropriately.
    editMenu.SubmenuOpened.Add(fun (args : RoutedEventArgs) ->
      let undo, redo = controller.undoStack.undoRedoTitles
      match undo with
      | Some(title) ->
        undoItem.Header <- "Undo " + title
        undoItem.IsEnabled <- true
      | None ->
        undoItem.Header <- "Undo"
        undoItem.IsEnabled <- false
      match redo with
      | Some(title) ->
        redoItem.Header <- "Redo " + title
        redoItem.IsEnabled <- true
      | None ->
        redoItem.Header <- "Redo"
        redoItem.IsEnabled <- false
      match !focusView with
      | Some(view) ->
        copyItem.IsEnabled <- not view.isEmpty
        pasteItem.IsEnabled <- (!copyBuffer).isSome
      | None ->
        copyItem.IsEnabled <- false
        pasteItem.IsEnabled <- false
      )

    menu.add(editMenu)

    let toolsMenu = makeTopMenuItem "_Tools"
    let logItem = MenuItem(Header = "Log")
    logItem.Click.Add(fun _ ->
      match !logWindow with
      | Some(logWindow) when logWindow.isOpen -> logWindow.show()
      | _ -> logWindow := Some(LogWindow())
      )
    toolsMenu.add(logItem)
    menu.add(toolsMenu)

    let filterMenu = makeTopMenuItem "Filters"

    let enableFilteringItem = MenuItem(Header = "Enable Map Filtering", IsCheckable = true, IsChecked = true)
    enableFilteringItem.Click.Add(fun _ -> mapFiltering := enableFilteringItem.IsChecked)
    filterMenu.add(enableFilteringItem)

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
    menu.add(filterMenu)

    let presetMenu = makeTopMenuItem "_Presets"
    let refreshPresetItem = MenuItem(Header = "Refresh")

    let rec addPresets (menuItem : MenuItem) directory =
      try
        for directory in System.IO.Directory.GetDirectories(directory) |> Array.sortWith (fun a b -> String.Compare(a, b, ignoreCase = true)) do
          let directoryItem = MenuItem(Header = System.IO.Path.GetFileName(directory))
          addPresets directoryItem directory
          if directoryItem.Items.IsEmpty = false then
            menuItem.add(directoryItem)
        for file in System.IO.Directory.GetFiles(directory) |> Array.sortWith (fun a b -> String.Compare(a, b, ignoreCase = true)) do
          if System.IO.Path.GetExtension(file) = ".yaml" then
            let presetItem = MenuItem(Header = System.IO.Path.GetFileNameWithoutExtension(file))
            let openPopup = ref none<Popup>
            let closePopup() =
              match !openPopup with
              | Some(popup) ->
                popup.IsOpen <- false
                openPopup := None
              | _ -> ()
            let contextMenu = ContextMenu()
            presetItem.ContextMenu <- contextMenu
            contextMenu.add(MenuItem(Header = "Delete", Margin = Thickness(0.0), withClick = fun _ ->
              closePopup()
              if FileUtil.confirmDelete "delete" file then
                recreatePresets()
              ))
            presetItem.GotFocus.Add(fun _ ->
              let w = previewSize
              let h = previewSize
              let popup = Popup(IsOpen = true, PlacementTarget = presetItem, Placement = PlacementMode.RelativePoint, HorizontalOffset = 280.0, IsHitTestVisible = false)
              openPopup := Some(popup)
              match loadYamlPreview file with
              | Some(map) ->
                let image = Image(Width = w, Height = h, Margin = Thickness(1.0), SnapsToDevicePixels = true)
                let view = PixmapView(image, int w, int h, quitWhenReady = true)
                view.start(RichMap3.pixmapSource(map))
                popup.Child <- image
              | None ->
                popup.Child <- Label(Width = w * 0.5, Height = h * 0.5, Content = "?", FontSize = 40.0)
              )
            // NOTE. This does not work well if the popup overlaps with the menu item: if the mouse moves over
            // the popup, the menu item loses focus, causing the popup to disappear. Then GotFocus fires once more
            // on the menu item, creating a new popup, and so on. A supposedly clean solution would be to
            // create a trigger for IsMouseOver, which involves templates and styles.
            presetItem.LostFocus.Add(fun _ -> closePopup())
            presetItem.Click.Add(fun _ ->
              let view = !focusView >? match !viewMode with | FullView -> fullView | HalfView -> halfView.[0] | ThirdView -> thirdView.[0]
              setFocus view
              controller.post(ApplyMapLoad(view, "Load Preset", file, true))
              )
            menuItem.add(presetItem)
      with | _ -> ()

    and recreatePresets() =
      presetMenu.Items.Clear()
      presetMenu.add(refreshPresetItem)
      presetMenu.add(Separator())
      addPresets presetMenu presetDirectory

    refreshPresetItem.Click.Add(fun _ -> recreatePresets())
    recreatePresets()
    menu.add(presetMenu)

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
      EditorAbout.showAboutWindow()
      )
    helpMenu.add(aboutItem)

    menu.add(helpMenu)

    menuPanel.add(menu)

    // Add context menus and event handlers to images.
    iterateViews (fun view _ ->

      let menu = ContextMenu(Placement = Primitives.PlacementMode.Mouse)

      if view.mainMode = HalfView || view.mainMode = ThirdView then
        let maximizeItem = MenuItem(Header = "Maximize")
        maximizeItem.Click.Add(fun _ -> maximizeView view)
        menu.add(maximizeItem)
      elif view.mainMode = FullView || view.mainMode = ThirdView then
        let minimizeItem = MenuItem(Header = "Go to Quad View")
        minimizeItem.Click.Add(fun _ -> minimizeView view)
        menu.add(minimizeItem)
      if view.mainMode = FullView || view.mainMode = HalfView then
        let mosaicifyItem = MenuItem(Header = "Go to Nono View")
        mosaicifyItem.Click.Add(fun _ -> thirdifyView view)
        menu.add(mosaicifyItem)

      menu.add(Separator(Margin = Thickness(0.0, 0.0, 0.0, 0.0)))

      let zoomIn = MenuItem(Header = "Zoom In")
      zoomIn.Click.Add(fun _ ->
        setFocus view
        controller.postAction(view, "Zoom In", View.transform(zoom = ModifyFloat((*) 2.0)))
        controller.post(CloseAction)
        )
      menu.add(zoomIn)

      let zoomOut = MenuItem(Header = "Zoom Out")
      zoomOut.Click.Add(fun _ ->
        setFocus view
        controller.postAction(view, "Zoom Out", View.transform(zoom = ModifyFloat((*) 0.5)))
        controller.post(CloseAction)
        )
      menu.add(zoomOut)

      let resetZoom = MenuItem(Header = "Reset Zoom")
      resetZoom.Click.Add(fun _ ->
        setFocus view
        controller.postAction(view, "Reset Zoom", View.transform(zoom = SelectFloat 1.0))
        controller.post(CloseAction)
        )
      menu.add(resetZoom)

      let resetView = MenuItem(Header = "Reset View")
      resetView.Click.Add(fun _ ->
        setFocus view
        controller.postAction(view, "Reset View", View.transform(centerX = SelectFloat 0.5, centerY = SelectFloat 0.5, centerZ = SelectFloat 0.5, zoom = SelectFloat 1.0))
        controller.post(CloseAction)
        )
      menu.add(resetView)

      menu.add(Separator(Margin = Thickness(0.0)))

      let randomizeItem = MenuItem(Header = "Randomize")
      randomizeItem.Click.Add(fun _ ->
        setFocus view
        controller.post(ApplyMapRestart([| view |], "Randomize", randomizePredicate))
        )
      menu.add(randomizeItem)

      let openNewWindow = MenuItem(Header = "Open in New Window")
      openNewWindow.Click.Add(fun _ -> Editor.start(view))
      menu.add(openNewWindow)

      let createPreset = MenuItem(Header = "Create Preset..")
      createPreset.Click.Add(fun _ ->
        let grid = Grid()
        let presetWindow = Window(Title = "Add Preset", ResizeMode = ResizeMode.CanMinimize, SizeToContent = SizeToContent.WidthAndHeight, Content = grid)
        grid.RowDefinitions.Add(RowDefinition(Height = GridLength.Auto))
        grid.RowDefinitions.Add(RowDefinition(Height = GridLength.Auto))
        grid.RowDefinitions.Add(RowDefinition(Height = GridLength.Auto))
        grid.RowDefinitions.Add(RowDefinition(Height = GridLength.Auto))
        grid.RowDefinitions.Add(RowDefinition(Height = GridLength.Auto))
        grid.ColumnDefinitions.Add(ColumnDefinition(Width = GridLength.Auto))
        grid.ColumnDefinitions.Add(ColumnDefinition(Width = GridLength.Auto))
        grid.ColumnDefinitions.Add(ColumnDefinition(Width = GridLength.Auto))

        let image = Image(Width = previewSize, Height = previewSize, Margin = Thickness(4.0), SnapsToDevicePixels = true)
        let preview = PixmapView(image, int previewSize, int previewSize, quitWhenReady = true)
        preview.start(view.pixmapSource)

        let boxWidth = 200.0
        let boxHeight = 24.0
        let boxMargin = Thickness(5.0, 2.0, 5.0, 2.0)
        let submenuLabel = Label(Content = "Submenu", HorizontalAlignment = HorizontalAlignment.Right, VerticalAlignment = VerticalAlignment.Center)
        let submenuBox = TextBox(Width = boxWidth, Height = boxHeight, Margin = boxMargin)
        let submenuCombo = ComboBox(Width = boxWidth, Height = boxHeight, Margin = boxMargin, VerticalAlignment = VerticalAlignment.Top)
        submenuCombo.add(ComboBoxItem(Content = "", withPreviewMouseDown = fun _ -> submenuBox.Text <- ""))

        let presetDirectories, presetFiles = FileUtil.getDirectoryContents presetDirectory
        for relativePath, absolutePath in presetDirectories do
          submenuCombo.add(ComboBoxItem(Content = relativePath, withPreviewMouseDown = fun _ -> submenuBox.Text <- relativePath))

        let nameLabel = Label(Content = "Preset name", HorizontalAlignment = HorizontalAlignment.Right, VerticalAlignment = VerticalAlignment.Center)
        let nameBox = TextBox(Width = boxWidth, Height = boxHeight, Margin = boxMargin)

        let saveIt() = 
          if nameBox.Text.size > 0 then
            let file = nameBox.Text + ".yaml"
            let submenuDirectory = if submenuBox.Text.size > 0 then System.IO.Path.Combine(presetDirectory, submenuBox.Text) else presetDirectory
            let path = System.IO.Path.Combine(submenuDirectory, file)
            try
              let directory = System.IO.Path.GetDirectoryName(path)
              if System.IO.Directory.Exists(directory) = false then
                System.IO.Directory.CreateDirectory(directory) |> ignore
            with
              | ex -> MessageBox.Show(ex.ToString()) |> ignore
            if FileUtil.confirmDelete "overwrite" path then
              saveYaml false true path
              recreatePresets()
            presetWindow.Close()

        nameBox.PreviewKeyDown.Add(fun (args : Input.KeyEventArgs) ->
          if args.Key =. Input.Key.Return then
            saveIt()
            args.Handled <- true
          )
        let nameCombo = ComboBox(Width = boxWidth, Height = boxHeight, Margin = boxMargin, VerticalAlignment = VerticalAlignment.Top)
        nameCombo.add(ComboBoxItem(Content = "", withPreviewMouseDown = fun _ -> nameBox.Text <- ""; submenuBox.Text <- ""))
        for relX, absX in presetFiles do
          if isTextureFile relX then
            nameCombo.add(ComboBoxItem(Content = System.IO.Path.ChangeExtension(relX, nullRef), withPreviewMouseDown = fun _ -> submenuBox.Text <- System.IO.Path.GetDirectoryName(relX); nameBox.Text <- System.IO.Path.GetFileNameWithoutExtension(relX)))

        let buttonWidth = 90.0
        let buttonBg = Wpf.brush(1.0, 1.0, 1.0, 0.3)
        let saveButton = Button(Content = "Save", Background = buttonBg, HorizontalAlignment = HorizontalAlignment.Left, Width = buttonWidth, Margin = Thickness(4.0, 2.0, 4.0, 4.0), Padding = Thickness(2.0))
        saveButton.Click.Add(fun _ -> saveIt())
        let cancelButton = Button(Content = "Cancel", Background = buttonBg, HorizontalAlignment = HorizontalAlignment.Right, Width = buttonWidth, Margin = Thickness(4.0, 2.0, 4.0, 4.0), Padding = Thickness(2.0))
        cancelButton.Click.Add(fun _ -> presetWindow.Close())

        grid.add(Border(Background = Wpf.verticalBrush(Wpf.color(0.9), Wpf.color(0.8))), 0, 4, 3, 1)
        grid.add(submenuLabel, 0, 0)
        grid.add(submenuBox, 1, 0)
        grid.add(submenuCombo, 1, 1)
        grid.add(nameLabel, 0, 2)
        grid.add(nameBox, 1, 2)
        grid.add(nameCombo, 1, 3)
        grid.add(image, 2, 0, 1, 4)
        grid.add(saveButton, 0, 4)
        grid.add(cancelButton, 2, 4)
        nameBox.Focus() |> ignore
        presetWindow.ShowDialog() |> ignore
        )
      menu.add(createPreset)

      let overwritePreset = MenuItem(Header = "Update Preset")
      overwritePreset.Click.Add(fun _ ->
        if FileUtil.confirmDelete "overwrite" view.presetFilename then
          saveYaml false true view.presetFilename
          recreatePresets()
        )
      menu.add(overwritePreset)

      let copySource = MenuItem(Header = "Copy F# Code to Clipboard")
      copySource.Click.Add(fun _ ->
        let data = DnaData(!view.controller.dna)
        System.Windows.Clipboard.SetText(data.sourceCode + ".generate(Map3Dna.loadEditorMap)\n")
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
              // Mutate other half or third views.
              match view.mainMode with
              | FullView -> ()
              | HalfView ->
                controller.post(ApplyMapMutation([| for i in 1 .. halfView.last -> halfView.[(view.gridI + i) % halfView.size] |],
                                                 view,
                                                 "Mutate",
                                                 fun rnd -> View.mutationPredicate(rnd, view, !mutateMode)))
              | ThirdView ->
                controller.post(ApplyMapMutation([| for i in 1 .. thirdView.last -> thirdView.[(view.gridI + i) % thirdView.size] |],
                                                 view,
                                                 "Mutate",
                                                 fun rnd -> View.mutationPredicate(rnd, view, !mutateMode)))
            else
              setFocus view

          | JoltTool ->
            if (!focusView).isSomeAnd((===) view) then
              // Mutate the view.
              controller.post(ApplyMapMutation([| view |], view, "Jolt", fun rnd -> View.mutationPredicate(rnd, view, !mutateMode)))
              controller.post(CloseAction)
            else
              setFocus view

        args.Handled <- true
        )

      image.MouseMove.Add(fun (args : Input.MouseEventArgs) ->
        match !userAction with
        | Panning(view, source) ->
          let target = args.GetPosition(canvas).vec2f
          let delta = (source - target) / (float32 image.Width * (!view.controller.deep).zoom)
          controller.postAction(view, "Pan", View.transform(centerX = ModifyFloat (fun x -> x + float delta.x),
                                                            centerY = ModifyFloat (fun y -> y + float delta.y)))
          userAction := Panning(view, target)
          args.Handled <- true

        | Zooming(_, source) ->
          let target = args.GetPosition(canvas).vec2f
          let size = (target - source).maxNorm
          dragShape.Margin <- Thickness(Left = float (source.x - size), Top = float (source.y - size))
          dragShape.Width <- 2.0 * float size
          dragShape.Height <- 2.0 * float size
          if dragShape.Visibility =. Visibility.Collapsed && Vec2f.distance(source, target) >= dragMinimum then
            dragShape.Visibility <- Visibility.Visible
          elif dragShape.Visibility =. Visibility.Visible && Vec2f.distance(source, target) < dragMinimum then
            dragShape.Visibility <- Visibility.Collapsed
          args.Handled <- true

        | Idle -> ()
        )

      image.MouseWheel.Add(fun (args : Input.MouseWheelEventArgs) ->
        if args.Delta <> 0 then
          match !toolMode with
          | PanZoomTool ->
            let delta = if args.Delta > 0 then 1.1f else 1.0f / 1.1f
            controller.postAction(view, "Zoom", View.transform(zoom = ModifyFloat(fun zoom -> zoom * float delta)))
          | _ ->
            let delta = float32 (sign args.Delta) * 0.01f / (!view.controller.deep).zoom
            controller.postAction(view, "Pan Z", View.transform(centerZ = ModifyFloat(fun z -> z + float delta)))
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
            controller.postAction(view, "Zoom", View.transform(centerX = SelectFloat (average x0' x1' |> float),
                                                               centerY = SelectFloat (average y0' y1' |> float),
                                                               zoom = SelectFloat (1.0 / float (y1' - y0'))))
            controller.post(CloseAction)
          dragShape.Visibility <- Visibility.Collapsed

        controller.post(CloseAction)
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
            if view.presetFilename.size > 0 then
              overwritePreset.Header <- "Update Preset '" + System.IO.Path.GetFileNameWithoutExtension(view.presetFilename) + "'"
            else
              overwritePreset.Header <- "Update Preset"
              overwritePreset.IsEnabled <- false
        )

      )
    
    let randomizeAllButton = Button(Content = "Randomize All", Background = toolBg, Margin = Thickness(1.0, 2.0, 1.0, 1.0), withToolTip = "Randomize all visible views.")
    randomizeAllButton.Command <- createShortcut Key.R ModifierKeys.Control (fun _ ->
      let views = Darray.create()
      iterateViews(fun view visible -> if visible then views.add(view))
      controller.post(ApplyMapRestart(views.toArray, "Randomize All", randomizePredicate))
      match !viewMode with
      | FullView -> setFocus fullView
      | _ -> clearFocus()
      )
    toolPanel.add(randomizeAllButton, Dock.Top)
    toolPanel.add(Separator(Margin = Thickness(0.0, 6.0, 0.0, 0.0)), Dock.Top)

    toolPanel.add(Label(Content = "Mutation mode"), Dock.Top)
    let mutationModeBox = ComboBox(Background = toolBg, Margin = Thickness(1.0))
    mutationModeBox.add(ComboBoxItem(Content = "Everything", IsSelected = true, withSelected = fun _ -> mutateMode := Everything))
    mutationModeBox.add(ComboBoxItem(Content = "Colors and Effects", withSelected = fun _ -> mutateMode := ColorsEffects))
    mutationModeBox.add(ComboBoxItem(Content = "Scales and Offsets", withSelected = fun _ -> mutateMode := ScalesOffsets))
    mutationModeBox.add(ComboBoxItem(Content = "Details", withSelected = fun _ -> mutateMode := Details))
    mutationModeBox.add(ComboBoxItem(Content = "Choose at Random", withSelected = fun _ -> mutateMode := EditorMutationMode.Random))
    toolPanel.add(mutationModeBox, Dock.Top)

    let layoutModeBox = ComboBox(Background = toolBg, Margin = Thickness(1.0))
    for i = 0 to layoutChoices.last do
      layoutModeBox.add(ComboBoxItem(Content = layoutChoices.name(i), IsSelected = (layoutChoices.weight(i) > 1.0), withSelected = fun _ -> layoutMode := layoutChoices.value(i)))

    toolPanel.add(Separator(Margin = Thickness(0.0, 6.0, 0.0, 0.0)), Dock.Top)
    toolPanel.add(Label(Content = "Default layout mode", withToolTip = "Initial layout mode for random textures."), Dock.Top)
    toolPanel.add(layoutModeBox, Dock.Top)
    toolPanel.add(Separator(Margin = Thickness(0.0, 6.0, 0.0, 6.0)), Dock.Top)
    toolPanel.add(viewBar, Dock.Top)

    toolPanel.add(Separator(Margin = Thickness(0.0, 6.0, 0.0, 6.0)), Dock.Top)
    toolPanel.add(toolBar, Dock.Top)

    toolPanel.add(mapInfoBox.canvas, Dock.Bottom)

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
      controller.stop()
      iterateViews(fun view _ -> view.stop())
      (!logWindow).apply(fun logWindow -> logWindow.close())
      settings.write()
      )
    window.PreviewKeyDown.Add(fun (args : KeyEventArgs) ->
      let scroll (text : string) (dx : float32) (dy : float32) =
        match !focusView with
        | Some(view) ->
          // We want to scroll by a pixel multiple to take advantage of caching.
          let pixels = max !view.pixmapView.renderWidth !view.pixmapView.renderHeight
          let delta = round (float32 pixels * 0.125f) / float32 pixels / (!view.controller.deep).zoom
          controller.postAction(view, text, View.transform(centerX = ModifyFloat(fun x -> x + float (dx * delta)), centerY = ModifyFloat(fun y -> y + float (dy * delta))))
          args.Handled <- true
        | _ -> ()
      match args.Key with
      | Key.Up    -> scroll "Pan Up" 0.0f -1.0f
      | Key.Down  -> scroll "Pan Down" 0.0f 1.0f
      | Key.Right -> scroll "Pan Right" 1.0f 0.0f
      | Key.Left  -> scroll "Pan Left" -1.0f 0.0f
      | _ -> ()
      )
    window.PreviewTextInput.Add(fun (args : TextCompositionEventArgs) ->
      let zoom (text : string) delta =
        match !focusView with
        | Some(view) ->
          controller.postAction(view, text, View.transform(zoom = ModifyFloat(fun zoom -> zoom * delta)))
          args.Handled <- true
        | _ -> ()
      match args.Text with
      | "+" -> zoom "Zoom In" 1.25
      | "-" -> zoom "Zoom Out" 0.8
      | _ -> ()
     )
    window.LayoutUpdated.Add(fun _ -> layoutCanvas())

    window.Show()

    iterateViews(fun view _ ->
      view.pixmapView.start(pixmapSeed)
      )

    setToolMode PanTool
    setViewMode FullView
    setMinDetailLevel 50.0f
    setMaxDetailLevel 2000.0f

    sourceView.apply(fun sourceView ->
      fullView.controller.restart(bypassFilter = true, dnaSource = DnaData(!sourceView.controller.dna))
      fullView.filename <- sourceView.filename
      updateAll()
      )

   
(*
TODO

-Crossover tool.
-Aspect ratio support.
-Export dialog with options.
-DDS export, 2-D and 3-D.
-Add Map3 projection modes (in absence of depth channel): rectangle, sphere, ?
-Add channels like specular, normal (and/or displacement), depth.
-Add some non-tiling post fx options like bloom and enhance; although, may have to think about
 HDR tone mapping for these and channels.
-Palette editing tool where user can set pixel colors directly.
-History.
-Add either hover options or extra buttons to parameters in Dna view:
 -display range of parameter values in a gradient, pick new value by clicking.
 -reset value (if parameter has neutral, numerical value).
 -lock parameter / subtree.
 -mutate only subtree.
 -randomize subtree.
-Add screen space filter to PixmapView to reject mutations that do not influence the displayed
 2-slice. The filter would be run when the first mosaic level is computed to decide whether to
 display or reject it.

*)

