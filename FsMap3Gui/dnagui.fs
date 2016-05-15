/// Dna GUI components.
namespace FsMap3

open System
open System.Windows
open System.Windows.Media
open System.Windows.Shapes
open System.Windows.Controls

open Common


type ParameterDisplayMode = Editable | ReadOnly | Hidden



[<NoComparison; NoEquality>]
type DnaItem = struct
  val displayMode : ParameterDisplayMode
  val treeItem : TreeViewItem option

  new(displayMode, treeItem) = { displayMode = displayMode; treeItem = treeItem }
end



/// Plug-in for custom choice visualization. Goes into ComboBoxItem.
type ChoiceVisualizer = Parameter -> int -> obj option


/// Plug-in for custom value visualization.
type ValueVisualizer = Parameter -> UIElement option



/// Displays a tree view of the parameters of a Dna.
/// Synchronization model: Construct from the UI thread. After publication, access only from the UI thread.
type DnaView() =

  /// Structural hash -> expanded or not.
  let expandMap = HashMap<int64, bool>.create(int)

  /// Tree item -> structural hash. This is used to update expandMap above.
  let structMap = HashMap<TreeViewItem, int64>.create(hash)

  /// Extra indexed info on current Dna parameters.
  let itemArray = Darray<DnaItem>.create()

  /// Displayed genotype.
  let dna = Dna.create()

  let choiceVisualizers = Darray.create()
  let valueVisualizers = Darray.create()

  member this.addChoiceVisualizer(v : ChoiceVisualizer) = choiceVisualizers.add(v)
  member this.addValueVisualizer(v : ValueVisualizer) = valueVisualizers.add(v)

  /// The tree view component should be added to a GUI by the client.
  member val treeView = TreeView(BorderThickness = Thickness(0.0))

  /// Parameter display filtering. Read-only by default. Note that read-only parameters still receive callbacks.
  member val viewFilter = fun (_ : Parameter) -> ReadOnly with get, set

  /// This callback is invoked when the user clicks the left mouse button on the value of a parameter.
  /// The arguments are parameter number and the chosen value. The callback is invoked only if the
  /// value is different from the displayed value.
  member val leftCallback = fun (i : int) (v : uint) -> () with get, set

  /// This callback is invoked when the user scrolls with the mouse wheel on the value of an ordered parameter.
  /// The arguments are parameter number and mouse wheel delta.
  member val wheelCallback = fun (i : int) (delta : int) -> () with get, set

  /// This callback is invoked when the user clicks the right mouse button on the value of a parameter.
  /// The argument is parameter number. Value is not provided; it is intended that the right mouse button
  /// randomizes or cycles the value.
  member val rightCallback = fun (i : int) -> () with get, set


  /// Resets the view.
  member this.reset() =
    // Save item expandedness information.
    structMap.iter(fun item hash -> expandMap.[hash] <- item.IsExpanded)
    structMap.reset()
    itemArray.reset()
    dna.reset()
    this.treeView.Items.Clear()


  /// Creates a UI element for the parameter.
  member private this.createItemPanel(i : int, parameter : Parameter, editable : bool) =
    let itemPanel = StackPanel(Orientation = Orientation.Horizontal)
    itemPanel.add(TextBlock(Documents.Span(Documents.Run(parameter.name))))
    // If we have editable choices, show them in a list.
    if editable && parameter.choices.isSome then
      let choices = !parameter.choices
      let valueBox = ComboBox(Margin = Thickness(10.0, 0.0, 0.0, 0.0))
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
          valueItem.Selected.Add(fun _ -> this.leftCallback i (uint j))
      valueBox.PreviewMouseRightButtonDown.Add(fun _ -> this.rightCallback i)
      valueBox.PreviewMouseWheel.Add(fun (args : Input.MouseWheelEventArgs) ->
        if args.Delta <> 0 then this.wheelCallback i args.Delta
        args.Handled <- true
        )
      itemPanel.add(valueBox)
    elif parameter.maxValue > 0u || parameter.valueString.Length > 0 then
      let vw = 140.0
      let vh = 22.0
      let vb = 1.0
      let vcanvas = Canvas(Margin = Thickness(10.0, 0.0, 0.0, 0.0), Width = vw, Height = vh, Background = Wpf.brush(0.1, 0.1, 0.2))
      vcanvas.PreviewMouseLeftButtonDown.Add(fun (args : Input.MouseButtonEventArgs) ->
        let x = args.GetPosition(vcanvas).X
        let value = uint <| round (lerp -0.49 (float parameter.maxValue + 0.49) (delerp01 4.0 (vw - 4.0) x))
        if i < dna.size && value <> dna.[i].value then this.leftCallback i value
        )
      vcanvas.PreviewMouseRightButtonDown.Add(fun _ -> this.rightCallback i)
      vcanvas.PreviewMouseWheel.Add(fun (args : Input.MouseWheelEventArgs) ->
        if args.Delta <> 0 then this.wheelCallback i args.Delta
        args.Handled <- true
        )
      if parameter.format = Ordered then
        let vrect = Rectangle(Fill = Wpf.brush(0.25, 0.35, 0.6), Width = max 1.0 ((vw - vb * 2.0) * (float parameter.value / float parameter.maxValue)), Height = vh - vb * 2.0)
        vcanvas.add(vrect, vb, vb)
      let vtext = TextBlock(Documents.Span(Documents.Run(parameter.valueString)), Foreground = Brushes.White)
      vcanvas.add(vtext, 2.0, 2.0)
      itemPanel.add(vcanvas)
    itemPanel


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
        dna.fingerprint <- dna'.fingerprint
        for i = 0 to dna'.last do
          let valueHasChanged = dna'.[i].value <> dna.[i].value || dna'.[i].valueString <> dna.[i].valueString
          dna.parameterArray.[i] <- dna'.[i]
          if displayArray.[i] <> Hidden && valueHasChanged then
            let item = !itemArray.[i].treeItem
            item.Header <- this.createItemPanel(i, dna.[i], displayArray.[i] = Editable)

    else

      this.reset()
      dna.copyFrom(dna')

      for i = 0 to dna.last do
        let parameter = dna.[i]
        let displayAction = displayArray.[i]
        if displayAction = Hidden then
          itemArray.add(DnaItem(Hidden, None))
        else
          let item = TreeViewItem(Margin = Thickness(1.0))
          itemArray.add(DnaItem(displayAction, Some item))
          structMap.[item] <- parameter.structuralId
          item.IsExpanded <- expandMap.find(parameter.structuralId) >? true
          item.Header <- this.createItemPanel(i, parameter, (displayAction = Editable))

          let rec addItem i item =
            match dna.[i].level, dna.[i].parent with
            | 0, _ ->
              // Item is at root level.
              this.treeView.add(item)
            | _, Someval j ->
              match itemArray.[j].treeItem with
              | Some parentItem -> parentItem.add(item)
              | None ->
                // Create a blank parent item.
                let blankItem = TreeViewItem(Margin = Thickness(1.0))
                itemArray.[j] <- DnaItem(itemArray.[j].displayMode, Some blankItem)
                addItem j blankItem
                blankItem.add(item)
            | _ ->
              // This is not supposed to happen - every parameter not at root should have a parent.
              Log.warnf "Dna parameter '%s' does not have a parent." dna.[i].name
              this.treeView.add(item)

          addItem i item


