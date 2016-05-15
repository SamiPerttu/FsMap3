module FsMap3.DnaVisualizer

open System.Windows
open System.Windows.Media
open System.Windows.Shapes
open System.Windows.Controls

open Common


let fadeChoiceVisualizer width height (parameter : Parameter) (i : int) =
  match parameter.choices with
  | Some(:? Choices<float32 -> float32> as choices) ->
    let f = choices.value(i)

    let polygon = Polygon(Fill = Wpf.brush(0.25, 0.35, 0.6), StrokeThickness = 0.0)
    let points = PointCollection()
    if parameter.name.EndsWith("wave") then
      points.Add(Point(width, height))
      points.Add(Point(0.0, height))
      let n = int width - 2
      for i = 0 to n do
        points.Add(Point(1.0 + (width - 2.0) * float i / float n, height * 0.5 - (height - 2.0) * 0.5 * float (f (float32 i / float32 n))))
    else
      points.Add(Point(width, 0.0))
      points.Add(Point(width, height))
      points.Add(Point(0.0, height))
      let n = int width - 2
      for i = 0 to n do
        points.Add(Point(1.0 + (width - 2.0) * float i / float n, height - 1.0 - (height - 2.0) * float (f (float32 i / float32 n))))
    polygon.Points <- points

    let canvas = Canvas(Width = width, Height = height, Background = Brushes.Transparent)
    canvas.add(Rectangle(Width = width, Height = height, StrokeThickness = 1.0, Stroke = Wpf.brush(0.0)), 0.0, 0.0)
    canvas.add(polygon, 0.0, 0.0)

    let panel = StackPanel(Orientation = Orientation.Horizontal, HorizontalAlignment = HorizontalAlignment.Stretch)
    panel.add(canvas)
    panel.add(Label(Content = choices.name(i)))

    Some(box panel)

  | _ ->
    None



