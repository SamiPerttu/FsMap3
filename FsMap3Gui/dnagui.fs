/// Dna GUI components.
namespace Fuse

open System
open System.Windows
open System.Windows.Media
open System.Windows.Shapes
open System.Windows.Controls
open System.Windows.Controls.Primitives

open Common


type ParameterDisplayMode = Editable | ReadOnly | Hidden



[<NoComparison; NoEquality>]
type DnaItem = struct
  val displayMode : ParameterDisplayMode
  val treeItem : TreeViewItem option
  val valueCanvas : Canvas option

  new(displayMode, treeItem, valueCanvas) = { displayMode = displayMode; treeItem = treeItem; valueCanvas = valueCanvas }
end



/// Plug-in for custom choice visualization. Value goes into ComboBoxItem.
type ChoiceVisualizer = DnaParameter -> int -> obj option


/// Plug-in for custom value visualization.
type ValueVisualizer = DnaParameter -> UIElement option


/// Expresses extra-genetic dependencies in the visualization of a parameter as a list of
/// pairs (maximum distance to dependency source, dependency source parameter name).
/// These are needed if the visualization of a parameter can change even when
/// the value and value string remain the same.
type VisualizerDependency = DnaParameter -> (int * string) list



/// Displays a tree view of the parameters of a Dna.
/// Synchronization model: Construct from the UI thread. After publication, access only from the UI thread.
type DnaView() =

  /// Structural hash -> expanded or not.
  let expandMap = HashMap<int64, bool>.create(int)

  /// Tree item -> structural hash. This is used to update expandMap above.
  let structMap = HashRefMap<TreeViewItem, int64>.create()

  /// Extra indexed info on current Dna parameters.
  let itemArray = Darray<DnaItem>.create()

  /// Displayed genotype.
  let dna = Dna.create()

  let mutable dragState = None

  let choiceVisualizers = Darray.create()
  let valueVisualizers = Darray.create()
  let visualizerDependencies = Darray.create()

  member this.addChoiceVisualizer(v : ChoiceVisualizer) = choiceVisualizers.add(v)
  member this.addValueVisualizer(v : ValueVisualizer) = valueVisualizers.add(v)
  member this.addVisualizerDependency (v : VisualizerDependency) = visualizerDependencies.add(v)

  /// The tree view component should be added to a GUI by the client.
  member val treeView = TreeView(BorderThickness = Thickness(0.0))

  member val nameWidth = 108.0 with get, set
  member val valueBoxBg = Wpf.brush(0.1, 0.1, 0.2) with get, set
  member val valueBoxFg = Wpf.brush(0.25, 0.35, 0.6) with get, set
  member val valueBoxText = Wpf.brush(1.0, 1.0, 1.0) with get, set
  member val valueBoxWidth = 180.0 with get, set
  member val valueBoxHeight = 22.0 with get, set
  member val valueBoxMargin = 1.0 with get, set

  /// Parameter display filtering. Read-only by default. Note that read-only parameters still receive callbacks.
  member val viewFilter = fun (_ : DnaParameter) -> ReadOnly with get, set

  /// Called when the user clicks the left mouse button on the value of a parameter.
  /// The callback is invoked only if the value is different from the displayed value.
  member val leftCallback = fun (dna : Dna) (i : int) (v : uint) -> () with get, set

  /// Called when the user drags on the value of a parameter with the left mouse button held down.
  /// The callback is invoked only if the value differs from the previous callback.
  member val dragCallback = fun (dna : Dna) (i : int) (v : uint) -> () with get, set

  /// Called when the user releases the left mouse button after clicking it on the value of a parameter.
  member val releaseCallback = fun (dna : Dna) (i : int) -> () with get, set

  /// Called when the user scrolls with the mouse wheel on the value of an ordered parameter.
  member val wheelCallback = fun (dna : Dna) (i : int) (delta : int) -> () with get, set

  /// Called when the user clicks the right mouse button on the value of a parameter.
  member val rightCallback = fun (dna : Dna) (i : int) -> () with get, set


  /// Resets the view.
  member this.reset() =
    // Save item expandedness information.
    structMap.iter(fun item hash -> expandMap.[hash] <- item.IsExpanded)
    structMap.reset()
    itemArray.reset()
    dna.reset()
    this.treeView.Items.Clear()


  member private this.populateItemCanvas(i : int, parameter : DnaParameter, editable : bool, vcanvas : Canvas) =
    if parameter.format = Ordered then
      let vrect = Rectangle(Fill = this.valueBoxFg, Width = max 1.0 ((this.valueBoxWidth - this.valueBoxMargin * 2.0) * (float parameter.value / float parameter.maxValue)), Height = this.valueBoxHeight - this.valueBoxMargin * 2.0)
      vcanvas.add(vrect, this.valueBoxMargin, this.valueBoxMargin)
    let vtext = TextBlock(Documents.Span(Documents.Run(parameter.valueString)), Foreground = this.valueBoxText)
    vcanvas.add(vtext, 2.0, 2.0)

  /// Creates a UI element for the parameter.
  member private this.createItemPanel(i : int, parameter : DnaParameter, editable : bool) =
    let itemPanel = StackPanel(Orientation = Orientation.Horizontal)
    itemPanel.add(TextBlock(Documents.Run(parameter.name), Width = this.nameWidth, VerticalAlignment = VerticalAlignment.Center))
    // If we have editable choices, show them in a list.
    if editable && parameter.choices.isSome then
      let choices = !parameter.choices
      let valueBox = ComboBox(Margin = Thickness(2.0, 0.0, 0.0, 0.0))
      itemPanel.Margin <- Thickness(0.0, 2.0, 0.0, 2.0)
      for j = 0 to choices.choiceCount - 1 do
        let weight = choices.choiceWeight(j)
        if weight > 0.0 then
          let content =
            match Fun.find 0 choiceVisualizers.last (fun k -> choiceVisualizers.[k] parameter j) Option.isSome with
            | Someval(Some content) -> content
            | _ -> choices.choiceName(j) |> box
          let valueItem = ComboBoxItem(Content = content)
          if parameter.value = uint j then valueItem.IsSelected <- true
          valueBox.add(valueItem)
          valueItem.Selected.Add(fun _ -> this.leftCallback dna i (uint j))
      valueBox.PreviewMouseRightButtonDown.Add(fun _ -> this.rightCallback dna i)
      valueBox.PreviewMouseWheel.Add(fun (args : Input.MouseWheelEventArgs) ->
        if args.Delta <> 0 then this.wheelCallback dna i args.Delta
        args.Handled <- true
        )
      itemPanel.add(valueBox)
      itemPanel, None
    elif parameter.maxValue > 0u || parameter.valueString.Length > 0 then
      let vcanvas = Canvas(Margin = Thickness(2.0, 0.0, 0.0, 0.0), Width = this.valueBoxWidth, Height = this.valueBoxHeight, Background = this.valueBoxBg)
      let positionToValue x =
        uint <| round (lerp -0.49 (float parameter.maxValue + 0.49) (delerp01 4.0 (this.valueBoxWidth - 4.0) x))
      vcanvas.PreviewMouseLeftButtonDown.Add(fun (args : Input.MouseButtonEventArgs) ->
        let value = args.GetPosition(vcanvas).X |> positionToValue
        if i < dna.size && value <> dna.[i].value then this.leftCallback dna i value
        vcanvas.CaptureMouse() |> ignore
        dragState <- Some(vcanvas, value)
        args.Handled <- true
        )
      vcanvas.PreviewMouseMove.Add(fun (args : Input.MouseEventArgs) ->
        match dragState with
        | Some(vcanvas', value) when i < dna.size && vcanvas' === vcanvas ->
          let value' = args.GetPosition(vcanvas).X |> positionToValue
          if value' <> value then this.dragCallback dna i value'
          dragState <- Some(vcanvas, value')
        | _ ->
          dragState <- None
          vcanvas.ReleaseMouseCapture()
        args.Handled <- true
        )
      vcanvas.MouseLeftButtonUp.Add(fun (args : Input.MouseButtonEventArgs) ->
        if dragState.isSome && i < dna.size then this.releaseCallback dna i
        dragState <- None
        vcanvas.ReleaseMouseCapture()
        args.Handled <- true
        )
      vcanvas.PreviewMouseRightButtonDown.Add(fun _ -> this.rightCallback dna i)
      vcanvas.PreviewMouseWheel.Add(fun (args : Input.MouseWheelEventArgs) ->
        if args.Delta <> 0 then this.wheelCallback dna i args.Delta
        args.Handled <- true
        )
      this.populateItemCanvas(i, parameter, editable, vcanvas)
      itemPanel.add(vcanvas)
      itemPanel, Some(vcanvas)
    else
      itemPanel, None


  /// Updates the tree view to display the genotype.
  member this.update(dna' : Dna) =

    // First, we check whether we can update the existing view instead of recreating everything from scratch.
    // For this to work, the two Dnas must be identical except for parameter values, and all parameters must
    // retain their display status.
    let dnaIsCompatible = dna.size = dna'.size && Fun.forall 0 dna.last (fun i ->
      dna.[i].semanticId = dna'.[i].semanticId && dna.[i].format = dna'.[i].format && dna.[i].name = dna'.[i].name && dna.[i].maxValue = dna'.[i].maxValue
      )

    let displayArray = Array.init dna'.size (fun i -> this.viewFilter dna'.[i])

    let displayIsCompatible = displayArray.size = itemArray.size && Fun.forall 0 displayArray.last (fun i ->
      itemArray.[i].displayMode = displayArray.[i]
      )

    if dnaIsCompatible && displayIsCompatible then

      // If the fingerprint is identical as well we assume nothing has changed.
      if dna'.fingerprint <> dna.fingerprint then

        let valueHasChanged = Array.init dna.size (fun i -> dna'.[i].value <> dna.[i].value || dna'.[i].valueString <> dna.[i].valueString)

        dna.fingerprint <- dna'.fingerprint

        for i = 0 to dna'.last do

          let updateIsNeeded =
            valueHasChanged.[i] || Fun.exists 0 visualizerDependencies.last (fun j ->
              visualizerDependencies.[j] dna.[i] |> List.exists (fun (windowSize, parameterName) ->
                Fun.exists (max 0 (i - windowSize)) (i - 1) (fun k -> valueHasChanged.[k] && dna.[k].name = parameterName))
              )
                 
          dna.parameterArray.[i] <- dna'.[i]
          if displayArray.[i] <> Hidden && updateIsNeeded then
            match itemArray.[i].valueCanvas with
            | Some(valueCanvas) ->
              valueCanvas.Children.Clear()
              this.populateItemCanvas(i, dna.[i], displayArray.[i] = Editable, valueCanvas)
              match dragState with
              | Some(vcanvas, _) when vcanvas === valueCanvas -> valueCanvas.CaptureMouse() |> ignore
              | _ -> ()
            | None ->
              let item = !itemArray.[i].treeItem
              let itemPanel, itemCanvas = this.createItemPanel(i, dna.[i], displayArray.[i] = Editable)
              itemArray.[i] <- DnaItem(displayArray.[i], Some(item), itemCanvas)
              item.Header <- itemPanel

    else

      this.reset()
      dna.copyFrom(dna')

      for i = 0 to dna.last do
        let parameter = dna.[i]
        let displayAction = displayArray.[i]
        if displayAction = Hidden then
          itemArray.add(DnaItem(Hidden, None, None))
        else
          let item = TreeViewItem(Margin = Thickness(-6.0, 1.0, 1.0, 1.0))
          let itemPanel, itemCanvas = this.createItemPanel(i, parameter, (displayAction = Editable))
          itemArray.add(DnaItem(displayAction, Some item, itemCanvas))
          structMap.[item] <- parameter.structuralId
          item.IsExpanded <- expandMap.find(parameter.structuralId) >? true
          item.Header <- itemPanel

          let rec addItem i item =
            match dna.[i].level, dna.[i].parent with
            | 0, _ ->
              // Item is at root level.
              this.treeView.add(item)
            | _, Someval j ->
              match itemArray.[j].treeItem with
              | Some parentItem ->
                parentItem.add(item)
              | None ->
                // Create a blank parent item.
                let blankItem = TreeViewItem(Margin = Thickness(1.0))
                itemArray.[j] <- DnaItem(itemArray.[j].displayMode, Some blankItem, None)
                addItem j blankItem
                blankItem.add(item)
            | _ ->
              // This is not supposed to happen - every parameter not at root should have a parent.
              Log.warnf "Dna parameter '%s' does not have a parent." dna.[i].name
              this.treeView.add(item)

          addItem i item

