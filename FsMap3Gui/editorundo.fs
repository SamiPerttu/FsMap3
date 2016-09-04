// Undo/redo.
namespace FsMap3

open Common


type [<NoComparison; NoEquality>] EditorEdit =
  | SetMap of view : EditorView<RichMap3> * before : ViewEditState * after : ViewEditState
  | SetFocus of before : EditorView<RichMap3> * after : EditorView<RichMap3>
  | SetViewMode of before : EditorViewMode * after : EditorViewMode

  member this.isCombinableWith(that : EditorEdit) =
    match this, that with
    | SetMap(view1, _, _), SetMap(view2, _, _) when view1 === view2 -> true
    | _ -> false



/// An edit bundle is a single undo/redoable action.
type [<NoComparison; NoEquality>] EditorEditBundle =
  {
    mutable time : float
    title : string
    mutable closed : bool
    edits : EditorEdit[]
  }

  member this.edit = this.edits.[0]



type [<NoComparison; NoEquality>] EditorUndoStack =
  {
    stack : EditorEditBundle Darray
    mutable position : int
  }

  static member create() = { EditorUndoStack.stack = Darray.create(); position = 0 }

  member this.undoRedoTitles =
    lock this (fun _ ->
      let undoTitle = if this.position > 0 then Some(this.stack.[this.position - 1].title) else None
      let redoTitle = if this.position < this.stack.size then Some(this.stack.[this.position].title) else None
      undoTitle, redoTitle
      )

  member this.undo() =
    lock this (fun _ ->
      if this.position > 0 then
        this.position <- this.position - 1
        Some(this.stack.[this.position])
      else None
      )

  member this.redo() =
    lock this (fun _ ->
      if this.position < this.stack.size then
        this.position <- this.position + 1
        Some(this.stack.[this.position - 1])
      else None
      )

  member this.add(bundle : EditorEditBundle) =
    lock this (fun _ ->
      this.stack.resize(this.position)
      if this.stack.size > 0
          && bundle.edits.size = 1
          && this.stack.lastItem.edits.size = 1
          && this.stack.lastItem.closed = false
          && bundle.title = this.stack.lastItem.title
          && this.stack.lastItem.edit.isCombinableWith(bundle.edit)
          && abs(bundle.time - this.stack.lastItem.time) < 2.0 then
          let action =
            match this.stack.lastItem.edit, bundle.edit with
            | SetMap(view, before, _), SetMap(_, _, after) ->
              SetMap(view, before, after)
            | _ -> failwith "EditorUndoStack.add: Bug."
          this.stack.[this.stack.last] <- { bundle with edits = [| action |]; closed = false }
      else
        match Fun.findArgBack 0 this.stack.last (fun i -> this.stack.[i].time < bundle.time) with
        | Someval i -> this.stack.insert(i + 1, bundle)
        | Noneval -> this.stack.insert(0, bundle)
        this.position <- this.position + 1
      )

  member this.closeLatest() =
    lock this (fun _ ->
      if this.stack.size > 0 then this.stack.lastItem.closed <- true
      )

