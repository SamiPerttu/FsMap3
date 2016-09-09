// Pixmap and IPixmapSource related GUIs.
namespace Fuse

open System.Windows
open System.Windows.Media
open System.Windows.Shapes
open System.Windows.Controls
open System.Drawing

open Common


[<AutoOpen>]
module PixmapExtensions =

  type Pixmap with

    /// Creates a frozen BitmapSource from the pixmap.
    member this.bitmapSource() =
      // Ensure 32-bit alignment of rows, just in case it is needed somewhere.
      let stride = (this.width * 3 + 3) &&& ~~~3
      let bytes = stride * this.height
      let pixelArray = Array.create bytes 0uy
      let inline topix x = byte <| clampi 0 255 (x * 256.0f |> int)
      let mutable i = 0
      for y = 0 to this.lastY do
        let mutable j = y * stride
        for x = 0 to this.lastX do
          let color = this.a.[i]
          pixelArray.[j] <- topix color.x
          pixelArray.[j + 1] <- topix color.y
          pixelArray.[j + 2] <- topix color.z
          i <- i + 1
          j <- j + 3
      let source = System.Windows.Media.Imaging.BitmapSource.Create(this.width, this.height, 96.0, 96.0, System.Windows.Media.PixelFormats.Rgb24, null, pixelArray, stride)
      source.Freeze()
      source


    /// Creates a frozen BitmapSource from the pixmap.
    /// Colors are mapped from 3 components to non-premultiplied RGBA with the supplied function.
    member this.bitmapSourceWithAlpha(f : Vec3f -> Vec4f) =
      let stride = this.width * 4
      let bytes = stride * this.height
      let pixelArray = Array.create bytes 0uy
      let inline topix x = byte <| clampi 0 255 (x * 256.0f |> int)
      let mutable i = 0
      let mutable j = 0
      for y = 0 to this.lastY do
        for x = 0 to this.lastX do
          let color = f this.a.[i]
          pixelArray.[j] <- topix (color.w * color.z)
          pixelArray.[j + 1] <- topix (color.w * color.y)
          pixelArray.[j + 2] <- topix (color.w * color.x)
          pixelArray.[j + 3] <- topix color.w
          i <- i + 1
          j <- j + 4
      let source = System.Windows.Media.Imaging.BitmapSource.Create(this.width, this.height, 96.0, 96.0, System.Windows.Media.PixelFormats.Pbgra32, null, pixelArray, stride)
      source.Freeze()
      source


    /// Saves the pixmap as a PNG file.
    member this.savePng(filename : string) =
      let bitmap = this.bitmapSource()
      use stream = new System.IO.FileStream(filename, System.IO.FileMode.Create)
      let encoder = Imaging.PngBitmapEncoder(Interlace = Imaging.PngInterlaceOption.Off)
      encoder.Frames.Add(Imaging.BitmapFrame.Create(bitmap))
      encoder.Save(stream)
      stream.Close()


    /// Shows the pixmap in a window.
    member this.show() =
      let window = Window(ResizeMode = ResizeMode.CanMinimize, SizeToContent = SizeToContent.WidthAndHeight, Topmost = true, WindowStartupLocation = WindowStartupLocation.CenterScreen)
      let canvas = Controls.Canvas(Width = float this.width, Height = float this.height)
      let image = Controls.Image(Width = float this.width, Height = float this.height, Source = this.bitmapSource())
      canvas.add(image)
      window.setPixelContent(canvas)
      window.KeyDown.Add(fun args -> if args.Key =. System.Windows.Input.Key.Escape then window.Close())
      window.Show()



[<AutoOpen>]
module IPixmapSourceExtensions =

  type IPixmapSource with

    /// Exports this source as a PNG file. Asks the user for the filename. This must be called from the UI thread.
    static member exportPng(source : IPixmapSource, width, height) =
      let dialog = Microsoft.Win32.SaveFileDialog(Title = "Export Image As..", DefaultExt = ".png", Filter = "PNG files (.png)|*.png")
      let result = dialog.ShowDialog()
      if result.HasValue && result.Value = true then
        let progressBar = ProgressBar(Maximum = float height, Width = 400.0, Height = 25.0)
        let progressWindow = Window(Title = "Exporting Image", Content = progressBar, ResizeMode = ResizeMode.CanMinimize, SizeToContent = SizeToContent.WidthAndHeight)
        progressWindow.Show()
        let rows = Atom.Int(0)
        let callback _ =
          let row = rows.post((+) 1)
          Wpf.dispatch(progressBar, fun _ -> progressBar.Value <- max progressBar.Value (float row))
        scheduleTask(fun _ ->
          let pixmap = PixmapSource.toPixmap(source, width, height, callback)
          pixmap.savePng(dialog.FileName)
          Wpf.dispatch(progressWindow, fun _ -> progressWindow.Close())
        )



[<NoEquality; NoComparison>]
type PixmapViewMessage =
  | SetSource of source : IPixmapSource
  | Reset
  | Quit



/// Maintains a view into IPixmapSource content in an Image control.
type PixmapView(image : System.Windows.Controls.Image, ?renderWidth, ?renderHeight) =
  
  let mutable agent = none<Agent<PixmapViewMessage>>
  let mutable currentSource = PixmapSource.zero

  member val renderWidth = Atom.Int(renderWidth >? 16) with get
  member val renderHeight = Atom.Int(renderHeight >? 16) with get

  /// For single shot views: quit after rendering any pixmap.
  member val quitWhenReady = false with get, set

  member this.setRenderSize(width, height) =
    this.renderWidth.set(width)
    this.renderHeight.set(height)

  /// How many preview levels to render. Each preview level halves resolution.
  member val previewLevels = 3 with get, set

  /// This callback is invoked whenever we are about to start waiting for new messages.
  member val idleCallback = fun () -> () with get, set

  member this.setSource(source) =
    agent.apply(fun agent -> agent.Post(SetSource source))

  member this.reset() =
    agent.apply(fun agent -> agent.Post(Reset))

  member this.start(?initialSource) =
    enforce (agent.isNone) "PixmapView.start: Renderer has already been started."
    agent <- Some(Agent.Start(this.agentFunction))
    match initialSource with
    | Some(source) -> this.setSource(source)
    | None -> this.reset()

  member this.stop() =
    agent.apply(fun agent -> agent.Post(Quit))
    agent <- None

  /// Renders a level. Level zero is full resolution.
  member private this.render(inbox : Agent<PixmapViewMessage>, source : IPixmapSource, renderWidth, renderHeight, level, previous : Pixmap option) =
    let w = renderWidth >>> level
    let h = renderHeight >>> level
    let pixmap = Pixmap.create(w, h)

    let computeRows (Pair(yi0, yi1)) =
      for yi = yi0 to yi1 do
        for xi = 0 to pixmap.lastX do pixmap.[xi, yi] <- source.getPixel(renderWidth, renderHeight, xi <<< level, yi <<< level)

    let computeRowsWith (previous : Pixmap) (Pair(yi0, yi1)) =
      for yi = yi0 to yi1 do
        if (yi &&& 1 = 1) || (yi >>> 1 >= previous.sizeY) then
          for xi = 0 to pixmap.lastX do pixmap.[xi, yi] <- source.getPixel(renderWidth, renderHeight, xi <<< level, yi <<< level)
        else
          let mutable xi = 0
          while xi < pixmap.sizeX do
            if xi >>> 1 < previous.sizeX then
              pixmap.set(xi, yi, previous.at(xi >>> 1, yi >>> 1))
              xi <- xi + 1
            if xi < pixmap.sizeX then pixmap.[xi, yi] <- source.getPixel(renderWidth, renderHeight, xi <<< level, yi <<< level)
            xi <- xi + 1

    /// How many pixels are rendered per task.
    let taskPixels = 2000
    let taskRows = max 1 (taskPixels / w)
    /// How many rows are rendered in a batch before checking for updates.
    let batchRows = if level = this.previewLevels && level > 0 then h else taskRows * 8

    let mutable interrupted = false
    let mutable row = 0
    while row < h do
      let row' = min h (row + batchRows)
      let rows = row' - row
      let tasks = (rows + taskRows - 1) / taskRows
      let computef = match previous with | Some(previous) -> computeRowsWith previous | _ -> computeRows
      let taskArray = Array.init tasks (fun i -> let row0 = row + i * taskRows in Pair(row0, min (row' - 1) (row0 + taskRows - 1)))
      Array.Parallel.iter computef taskArray
      if inbox.CurrentQueueLength = 0 then
        row <- row'
      else
        row <- h
        if row' < h then interrupted <- true

    if interrupted then
      None
    else
      Some(pixmap)


  // Main function.
  member private this.agentFunction(inbox) = async {
    let mutable alive = true
    while alive do

      if inbox.CurrentQueueLength = 0 then this.idleCallback()

      let! msg = inbox.Receive()
      match msg with

      | Quit ->
        alive <- false

      | Reset ->
        currentSource <- PixmapSource.zero
        Wpf.dispatch(image, fun _ ->
          image.Source <- Pixmap.create(16, 16, Vec3f(0.5f)).bitmapSource()
          )

      | SetSource(source) ->
        currentSource <- source

        // Renders and shows a level. Returns whether rendering was finished.
        let rec handleLevel level (previousPixmap : Pixmap option) renderWidth renderHeight =
          let t0 = Common.timeNow()
          match this.render(inbox, source, renderWidth, renderHeight, level, previousPixmap) with
          | Some(levelPixmap) ->
            // Don't bother showing this level if we can expect to finish the next one quickly.
            let showThisLevel = level <= 1 || Common.timeNow() - t0 > Q 1 100 || inbox.CurrentQueueLength > 0
            let previousPixmap' =
              if level > 0 then
                if showThisLevel then Some(Pixmap.createCopy(levelPixmap)) else Some(levelPixmap)
              else None
            if showThisLevel then
              source.postFx(levelPixmap)
              Wpf.dispatch(image, fun _ -> image.Source <- levelPixmap.bitmapSource())
            if level > 0 && inbox.CurrentQueueLength = 0 then
              handleLevel (level - 1) previousPixmap' renderWidth renderHeight
            else level = 0
          | None ->
            false

        if inbox.CurrentQueueLength = 0 then
          let renderWidth = !this.renderWidth
          let renderHeight = !this.renderHeight
          source.start(renderWidth, renderHeight)
          let ready = handleLevel this.previewLevels None renderWidth renderHeight
          source.finish()
          if ready && this.quitWhenReady then
            alive <- false
    }



/// Helps with IPixmapSource generation and bookkeeping for a PixmapView. The IPixmapSource objects
/// are generated from the deep type 'a, which is in turn generated from Dna. We avoid explicit
/// synchronization by keeping all shared objects immutable after publication.
[<NoComparison; NoEquality>]
type PixmapController<'a> =
  {
    pixmapView : PixmapView
    /// The deep seed corresponds to an empty genotype.
    deepSeed : 'a
    /// The pixmap seed corresponds to an empty genotype.
    pixmapSeed : IPixmapSource
    rnd : Rnd
    dna : Atom.Shared<Dna>
    deep : Atom.Shared<'a>
    pixmapSource : Atom.Shared<IPixmapSource>
    editSource : InteractiveSource
    fitnessCounter : Counter<float>
    deepGenerator : Dna -> 'a
    pixmapGenerator : 'a -> IPixmapSource
    deepFilter : 'a -> 'a option -> bool
    /// This callback is invoked whenever we are starting something that will end up updating the PixmapView.
    mutable workCallback : unit -> unit
  }

  static member create(pixmapView, pixmapSeed, deepSeed : 'a, deepGenerator, pixmapGenerator, ?deepFilter) =
    let rnd = Rnd(hash pixmapView + Common.timeSeed())
    {
      pixmapView = pixmapView
      deepSeed = deepSeed
      pixmapSeed = pixmapSeed
      rnd = rnd
      dna = Atom.Shared(Dna.create())
      deep = Atom.Shared(deepSeed)
      pixmapSource = Atom.Shared(pixmapSeed)
      editSource = InteractiveSource(rnd.tick)
      fitnessCounter = createCounter 1.0
      deepGenerator = deepGenerator
      pixmapGenerator = pixmapGenerator
      deepFilter = deepFilter >? fun _ _ -> true
      workCallback = ignore
    }

  member this.reset() =
    this.dna.set(Dna.create())
    this.deep.set(this.deepSeed)
    this.pixmapSource.set(this.pixmapSeed)
    this.pixmapView.setSource(this.pixmapSeed)

  member this.generate(bypassFilter : bool, ?previous, ?dnaSource, ?preAction) =
    this.workCallback()

    let dnaSource = dnaSource >? (this.editSource :> DnaSource)

    let dna = Dna.create()
    let fingerprint = (!this.dna).fingerprint

    let mutable attemptsLeft = 20
    let mutable success = false

    while success = false do
      preAction.apply(fun action -> action())
      let deep' = dna.generate(dnaSource, this.deepGenerator)
      if attemptsLeft = 0 || bypassFilter || (this.deepFilter deep' previous && dna.fingerprint <> fingerprint) then
        success <- true
        this.deep.set(deep')
        let pixmapSource = this.pixmapGenerator deep'
        this.pixmapSource.set(pixmapSource)
        this.pixmapView.setSource(pixmapSource)
        this.dna.set(dna)
        this.editSource.observe(dna, this.fitnessCounter.tick)
      else
        attemptsLeft <- attemptsLeft - 1

  /// Creates the Dna from scratch.
  member this.restart(?predicate, ?bypassFilter, ?dnaSource) =
    this.editSource.reset()
    predicate.apply(fun predicate -> this.editSource.parameterPredicate <- predicate)
    let dnaSource = dnaSource >? (this.editSource :> DnaSource)
    this.generate(bypassFilter >? false, dnaSource = dnaSource)

  /// Alters Dna with the predicate.
  member this.alter(predicate) =
    this.editSource.parameterPredicate <- predicate
    this.generate(true)

  /// Sets outright the Dna and the associated deep phenotype.
  member this.set(dna, deep) =
    this.dna.set(dna)
    this.deep.set(deep)
    // The pixmap generator may differ in this controller, so we rerun it here.
    let pixmapSource = this.pixmapGenerator deep
    this.pixmapSource.set(pixmapSource)
    this.pixmapView.setSource(pixmapSource)
    this.workCallback()
    this.editSource.reset()
    this.editSource.observe(dna, this.fitnessCounter.tick)

  /// Makes us a mutation of the contents of the source, which can be this controller or another controller.
  member this.mutateFrom(source : PixmapController<'a>, ?predicate, ?bypassFilter, ?dnaSource, ?preAction) =
    let sourceDna, sourceDeep = !source.dna, !source.deep
    this.editSource.reset()
    predicate.apply(fun predicate -> this.editSource.parameterPredicate <- predicate)
    let dnaSource = dnaSource >? (this.editSource :> DnaSource)
    dnaSource.observe(sourceDna, this.fitnessCounter.tick)
    this.generate(bypassFilter >? false, sourceDeep, dnaSource = dnaSource, ?preAction = preAction)

