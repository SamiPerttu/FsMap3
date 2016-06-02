namespace FsMap3

open Common


[<NoComparison; NoEquality>]
type EditorMapAction =
  {
    view : EditorView<RichMap3>
    title : string
    predicate : MutationPredicate
  }

  static member create(view, title, predicate) = { EditorMapAction.view = view; title = title; predicate = predicate }



[<NoComparison; NoEquality>]
type EditorMessage =
  | ApplyMapAction of action : EditorMapAction
  | ApplyMapRestart of views : EditorView<RichMap3>[] * title : string * predicate : MutationPredicate
  | ApplyMapMutation of views : EditorView<RichMap3>[] * source : EditorView<RichMap3> * title : string * predicates : MutationPredicate[]
  | ApplyMapCopy of view : EditorView<RichMap3> * source : EditorView<RichMap3> * title : string
  | ApplyMapLoad of view : EditorView<RichMap3> * title : string * filename : string * isPreset : bool
  | Undo
  | Redo
  | CloseAction
  | Quit



/// The editor controller serializes user actions and maintains the undo stack.
/// There is one editor controller per editor.
[<NoComparison; NoEquality>]
type EditorController =
  {
    undoStack : EditorUndoStack
    setFocus : EditorView<RichMap3> -> unit
    setViewMode : EditorViewMode -> unit
    updateGui : unit -> unit
    mutable agent : EditorMessage Agent
  }

  static member create(setFocus, setViewMode, updateGui) =
    let this =
      {
        EditorController.undoStack = EditorUndoStack.create()
        setFocus = setFocus
        setViewMode = setViewMode
        updateGui = updateGui
        EditorController.agent = nullRef
      }
    this.agent <- Agent.Start(this.agentFunction)
    this


  member this.stop() =
    this.agent.Post(Quit)


  member this.post(msg) =
    this.agent.Post(msg)


  member this.postAction(view, title, predicate) =
    this.agent.Post(ApplyMapAction(EditorMapAction.create(view, title, predicate)))


  member this.postUndo() = this.agent.Post(Undo)


  member this.postRedo() = this.agent.Post(Redo)


  /// Applies all map actions in the set in order.
  member this.applyActions(actionSet : EditorMapAction Darray) =
    let view = actionSet.firstItem.view
    let title = actionSet.firstItem.title

    let beforeState = view.editState
    // Here we combine Adjust01 and ModifyFloat predicates. This helps maintain responsiveness
    // and prevent lagging during rapid panning actions etc. If there are multiple alternating
    // runs, we pick the first and ignore the rest.
    view.controller.alter(fun rnd dna i ->
      let mutable modifyChain = []
      let mutable adjust01 = 0.0
      let mutable effective = None
      for j = actionSet.last downto 0 do
        match actionSet.[j].predicate rnd dna i with
        | Adjust01 delta ->
          modifyChain <- []
          adjust01 <- adjust01 + delta
        | Retain ->
          ()
        | ModifyFloat f ->
          modifyChain <- f :: modifyChain
          adjust01 <- 0.0
        | x ->
          effective <- Some(x)
      match effective with
      | Some(x) ->
        x
      | None ->
        if modifyChain.size > 0 then
          ModifyFloat(List.reduce (>>) modifyChain)
        elif adjust01 <> 0.0 then
          Adjust01(adjust01)
        else Retain
      )

    let edit =
      {
        EditorEditBundle.time = Common.timeNow()
        title = title
        closed = false
        edits = [| SetMap(view, beforeState, view.editState) |]
      }
    this.undoStack.add(edit)

    this.updateGui()


  member this.agentFunction(inbox) =
    async {
      let actionSet = Darray.create()
      let mutable alive = true

      let applyActions() =
        if actionSet.size > 0 then this.applyActions(actionSet)
        actionSet.reset()

      while alive do
        let! msg = inbox.Receive()
        match msg with

        | ApplyMapAction(action) ->
          if actionSet.size > 0 && action.view <>= actionSet.firstItem.view then applyActions()
          actionSet.add(action)
          if inbox.CurrentQueueLength = 0 then applyActions()

        | ApplyMapRestart(views, title, predicate) ->
          applyActions()
          let edits = views |> Array.map(fun view ->
            let beforeState = view.editState
            view.filename <- ""
            view.presetFilename <- ""
            view.controller.restart(predicate)
            SetMap(view, beforeState, view.editState)
            )
          let edit =
            {
              EditorEditBundle.time = Common.timeNow()
              title = title
              closed = true
              edits = edits
            }
          this.undoStack.add(edit)
          this.updateGui()

        | ApplyMapMutation(views, source, title, predicates) ->
          applyActions()
          let edits = Array.init views.size (fun i ->
            let view = views.[i]
            let beforeState = view.editState
            view.filename <- source.filename
            view.presetFilename <- source.presetFilename
            view.controller.mutateFrom(source.controller, predicates.[i])
            SetMap(view, beforeState, view.editState)
            )
          let edit =
            {
              EditorEditBundle.time = Common.timeNow()
              title = title
              closed = true
              edits = Array.append [| SetFocus(source, source) |] edits
            }
          this.undoStack.add(edit)
          this.updateGui()

        | ApplyMapCopy(view, source, title) ->
          applyActions()
          let beforeState = view.editState
          view.filename <- source.filename
          view.presetFilename <- source.presetFilename
          view.pixmapView.reset()
          view.controller.copyFrom(source.controller)
          let edit =
            {
              EditorEditBundle.time = Common.timeNow()
              title = title
              closed = true
              edits = [| SetMap(view, beforeState, view.editState); SetViewMode(source.mainMode, view.mainMode); SetFocus(source, view) |]
            }
          this.undoStack.add(edit)
          this.updateGui()

        | ApplyMapLoad(view, title, filename, isPreset) ->
          applyActions()
          try
            let beforeState = view.editState
            view.filename <- if isPreset then "" else filename
            view.presetFilename <- if isPreset then filename else ""
            use stream = new System.IO.StreamReader(filename)
            let readLine() = match stream.EndOfStream with | false -> Some(stream.ReadLine()) | true -> None
            let source = DeserializerSource(readLine)
            view.controller.generate(true, dnaSource = (source :> DnaSource))
            stream.Close()
            let edit =
              {
                EditorEditBundle.time = Common.timeNow()
                title = title
                closed = true
                edits = [| SetMap(view, beforeState, view.editState) |]
              }
            this.undoStack.add(edit)
            this.updateGui()
          with
            | ex -> System.Windows.MessageBox.Show(ex.ToString()) |> ignore

        | Quit ->
          alive <- false

        | Undo ->
          applyActions()
          match this.undoStack.undo() with
          | Some(bundle) ->
            for edit in bundle.edits do
              match edit with
              | SetMap(view, before, _) ->
                view.filename <- before.filename
                view.presetFilename <- before.presetFilename
                if before.genotype.size > 0 then
                  view.controller.generate(true, dnaSource = DnaData(before.genotype))
                else
                  view.controller.reset()
                this.setViewMode view.mainMode
              | SetFocus(before, _) ->
                this.setFocus before
              | SetViewMode(before, _) ->
                this.setViewMode before
            this.updateGui()
          | None -> ()

        | Redo ->
          applyActions()
          match this.undoStack.redo() with
          | Some(bundle) ->
            for edit in bundle.edits do
              match edit with
              | SetMap(view, _, after) ->
                view.filename <- after.filename
                view.presetFilename <- after.presetFilename
                if after.genotype.size > 0 then
                  view.controller.generate(true, dnaSource = DnaData(after.genotype))
                else
                  view.controller.reset()
                this.setViewMode view.mainMode
              | SetFocus(_, after) ->
                this.setFocus after
              | SetViewMode(_, after) ->
                this.setViewMode after
            this.updateGui()
          | None -> ()

        | CloseAction ->
          applyActions()
          this.undoStack.closeLatest()
    }

