// Pixmap and IPixmapSource related GUIs.
namespace FsMap3

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
      let inline topix x = byte <| clampi 0 255 (int x)
      let a = this.a
      let mutable i = 0
      for y = 0 to this.lastY do
        let mutable j = y * stride
        for x = 0 to this.lastX do
          let color = a.[i] * 256.0f
          i <- i + 1
          pixelArray.[j] <- topix color.x
          pixelArray.[j + 1] <- topix color.y
          pixelArray.[j + 2] <- topix color.z
          j <- j + 3
      let source = System.Windows.Media.Imaging.BitmapSource.Create(this.width, this.height, 96.0, 96.0, System.Windows.Media.PixelFormats.Rgb24, null, pixelArray, stride)
      source.Freeze()
      source


    /// Saves the pixmap as a PNG file.
    member this.savePNG(filename : string) =
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
      window.KeyDown.Add(fun args -> if args.Key = System.Windows.Input.Key.Escape then window.Close())
      window.Show()



[<AutoOpen>]
module IPixmapSourceExtensions =

  type IPixmapSource with

    /// Exports this source as a PNG file. Asks the user for the filename. This must be called from the UI thread.
    static member exportPng(source : IPixmapSource, width, height) =
      let dialog = Microsoft.Win32.SaveFileDialog(Title = "Export Image As..", DefaultExt = ".png", Filter = "PNG files (.png)|*.png")
      let result = dialog.ShowDialog()
      if result.HasValue && result.Value = true then
        let progressWindow = Window(Title = "Exporting Image", Topmost = true, ResizeMode = ResizeMode.CanMinimize, SizeToContent = SizeToContent.WidthAndHeight)
        let progressBar = ProgressBar(Maximum = float height, Width = 400.0, Height = 25.0)
        progressWindow.Content <- progressBar
        progressWindow.Show()
        let rows = Atom.Int(0)
        let callback _ =
          let row = rows.post((+) 1)
          Wpf.dispatch(progressBar, fun _ -> progressBar.Value <- max progressBar.Value (float row))
        scheduleTask(fun _ ->
          let pixmap = PixmapSource.toPixmap(source, width, height, callback)
          pixmap.savePNG(dialog.FileName)
          Wpf.dispatch(progressWindow, fun _ -> progressWindow.Close())
        )



[<NoEquality; NoComparison>]
type PixmapViewMessage =
  | SetSource of source : IPixmapSource
  | Reset
  | Quit



type PixmapView(image : System.Windows.Controls.Image) =
  
  let mutable agent = none<Agent<PixmapViewMessage>>
  let mutable currentSource = PixmapSource.zero

  member val renderWidth = Atom.Int(16) with get, set
  member val renderHeight = Atom.Int(16) with get, set

  member this.setRenderSize(width, height) =
    this.renderWidth.set(width)
    this.renderHeight.set(height)

  /// How many preview levels to render. Each preview level halves resolution.
  member val previewLevels = 3 with get, set

  member this.setSource(source) = agent.apply(fun agent -> agent.Post(SetSource source))

  member this.reset() = agent.apply(fun agent -> agent.Post(Reset))

  member this.start() =
    enforce (agent.isNone) "PixmapView.start: Renderer has already been started."
    agent <- Some(Agent.Start(this.agentFunction))
    this.reset()

  member this.stop() =
    agent.apply(fun agent -> agent.Post(Quit))

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
        if (yi &&& 1 = 1) || (yi >>> 1 > previous.lastY) then
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

      let! msg = inbox.Receive()
      match msg with

      | Quit ->
        alive <- false

      | Reset ->
        currentSource <- PixmapSource.zero
        Wpf.dispatch(image, fun _ -> image.Source <- Pixmap.create(16, 16, Vec3f(0.5f)).bitmapSource())

      | SetSource(source) ->
        currentSource <- source

        let rec handleLevel level (previousPixmap : Pixmap option) renderWidth renderHeight =
          let t0 = Common.timeNow()
          match this.render(inbox, source, renderWidth, renderHeight, level, previousPixmap) with
          | Some(levelPixmap) ->
            // Don't bother showing this level if we can expect to finish the next one quickly.
            let showThisLevel = level = 0 || Common.timeNow() - t0 > Q 1 100
            let previousPixmap' =
              if level > 0 then
                if showThisLevel then Some(Pixmap.createCopy(levelPixmap)) else Some(levelPixmap)
              else None
            // Call IPixmapSource.finish only if all pixels have been retrieved.
            if level = 0 then
              source.finish()
            if showThisLevel then
              source.postFx(levelPixmap)
              Wpf.dispatch(image, fun _ -> image.Source <- levelPixmap.bitmapSource())
            if level > 0 then
              handleLevel (level - 1) previousPixmap' renderWidth renderHeight
          | None -> ()

        if inbox.CurrentQueueLength = 0 then
          let renderWidth = !this.renderWidth
          let renderHeight = !this.renderHeight
          source.start(renderWidth, renderHeight)
          handleLevel this.previewLevels None renderWidth renderHeight
    }



/// Helps with IPixmapSource generation and bookkeeping for a PixmapView. The IPixmapSource objects
/// are generated from the deep type 'a, which is in turn generated from Dna. We avoid explicit
/// synchronization by keeping all shared objects immutable after publication.
[<NoComparison; NoEquality>]
type PixmapController<'a> =
  {
    pixmapView : PixmapView
    deepSeed : 'a
    rnd : Rnd
    dna : Atom.Shared<Dna>
    deep : Atom.Shared<'a>
    pixmapSource : Atom.Shared<IPixmapSource>
    editSource : InteractiveSource
    fitnessCounter : Counter<float>
    deepGenerator : Dna -> 'a
    pixmapGenerator : 'a -> IPixmapSource
    deepFilter : 'a -> bool
  }

  static member create(pixmapView, deepSeed : 'a, deepGenerator, pixmapGenerator, ?deepFilter) =
    let rnd = Rnd(hash pixmapView + Common.timeSeed())
    {
      pixmapView = pixmapView
      deepSeed = deepSeed
      rnd = rnd
      dna = Atom.Shared(Dna.create())
      deep = Atom.Shared(deepSeed)
      pixmapSource = Atom.Shared(PixmapSource.zero)
      editSource = InteractiveSource(rnd.tick)
      fitnessCounter = createCounter 1.0
      deepGenerator = deepGenerator
      pixmapGenerator = pixmapGenerator
      deepFilter = deepFilter >? fun _ -> true
    }

  member this.generate(bypassFilter : bool, ?dnaSource) =
    let dnaSource = dnaSource >? (this.editSource :> DnaSource)

    let dna = Dna.create()
    let fingerprint = (!this.dna).fingerprint

    let mutable attemptsLeft = 10
    let mutable success = false

    while success = false do
      let deep' = dna.generate(dnaSource, this.deepGenerator)
      if attemptsLeft = 0 || bypassFilter || (this.deepFilter deep' && dna.fingerprint <> fingerprint) then
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
  member this.restart(predicate) =
    this.editSource.reset()
    this.editSource.mutationPredicate <- predicate
    this.generate(false)

  /// Sets a parameter in the Dna.
  member this.setValue(index, value) =
    this.editSource.mutationPredicate <-
      match value with
      | Some(v) -> fun _ _ i -> if i = index then Select(v) else Retain
      | _ -> fun _ _ i -> if i = index then Jolt(1.0) else Retain
    this.generate(true)
    
  /// Modifies a parameter in the Dna.
  member this.modifyValue(index, delta) =
    this.editSource.mutationPredicate <-
      fun _ _ i -> if i = index then Modify(delta) else Retain
    this.generate(true)

  /// Alters Dna with the predicate.
  member this.alter(predicate) =
    this.editSource.mutationPredicate <- predicate
    this.generate(true)

  /// Copies the contents of another controller here.
  member this.copyFrom(source : PixmapController<_>) =
    let dna = !source.dna
    this.dna.set(dna)
    let deep = !source.deep
    this.deep.set(deep)
    // The pixmap generator may differ in this controller, so we rerun it here.
    let pixmapSource = this.pixmapGenerator deep
    this.pixmapSource.set(pixmapSource)
    this.pixmapView.setSource(pixmapSource)
    this.editSource.reset()
    this.editSource.observe(dna, this.fitnessCounter.tick)

  /// Makes us a mutation of the contents of the source, which can be this controller or another controller.
  member this.mutateFrom(source : PixmapController<_>, predicate) =
    let dna = !source.dna
    // TODO. Do we always want to reset the edit memory?
    this.editSource.reset()
    this.editSource.observe(dna, this.fitnessCounter.tick)
    this.editSource.mutationPredicate <- predicate
    this.generate(false)

