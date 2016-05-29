namespace FsMap3

open Common

open System
open System.Windows
open System.Windows.Media
open System.Windows.Shapes
open System.Windows.Controls
open System.Windows.Controls.Primitives


type LogWindow() =

  let grid = Grid()
  let window = Window(Width = 512.0, Height = 300.0, SizeToContent = SizeToContent.Manual, Title = "FsMap3 Editor Log", Content = grid)
  let text = TextBlock(Padding = Thickness(1.0), Foreground = Wpf.brush(0.8, 0.85, 1.0), Background = Wpf.brush(0.2, 0.4, 0.45))
  let viewer = ScrollViewer(Content = text, VerticalScrollBarVisibility = ScrollBarVisibility.Visible, HorizontalScrollBarVisibility = ScrollBarVisibility.Auto)
  let bottomPanel = DockPanel(Margin = Thickness(2.0), LastChildFill = false)
  let closeButton = Button(Padding = Thickness(15.0, 2.0, 15.0, 2.0), Content = "Close")
  let scrollButton = CheckBox(Content = "Follow Log", IsChecked = Nullable(true), VerticalAlignment = VerticalAlignment.Center)
  let mutable opened = true

  let addEntry (entry : LogEntry) =
    Wpf.dispatch(window, fun _ ->
      text.Inlines.Add(entry.message)
      if scrollButton.IsChecked.HasValue && !scrollButton.IsChecked = true then
        viewer.ScrollToBottom()
      text.Inlines.Add(Documents.LineBreak())
      )

  let listener (entry : LogEntry) =
    addEntry entry

  let logListener = LogListener(listener)

  do
    grid.RowDefinitions.Add(RowDefinition())
    grid.RowDefinitions.Add(RowDefinition(Height = GridLength.Auto))
    grid.ColumnDefinitions.Add(ColumnDefinition())
    grid.add(viewer, 0, 0)
    grid.add(bottomPanel, 0, 1)
    bottomPanel.add(closeButton, Dock.Right)
    bottomPanel.add(scrollButton, Dock.Left)
    closeButton.Click.Add(fun _ -> window.Close())
    window.Closed.Add(fun _ ->
      opened <- false
      Log.removeListener(logListener)
      )
    Log.iterMemory(addEntry)
    Log.addListener(logListener)
    window.Show()

  member this.isOpen =
    opened

  member this.show() =
    if opened then
      window.Activate() |> ignore

  member this.close() =
    if opened then
      window.Close()

