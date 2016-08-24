/// DnaView parameter visualizers.
module FsMap3.DnaVisualizer

open System.Windows
open System.Windows.Media
open System.Windows.Shapes
open System.Windows.Controls

open Common


let colorSpaceChoiceVisualizer (width : int) (height : int) =

  let bitmapCache = HashMap.create(Mangle.mangleString)

  fun (parameter : DnaParameter) (i : int) ->
    match parameter.choices with

    | Some(:? Choices<ColorSpace> as choices) ->
      let image = Controls.Image(Width = float width, Height = float height, Margin = Thickness(1.0))
      let space = choices.value(i)
      let spaceKey = choices.name(i)

      match bitmapCache.find(spaceKey) with
      | Someval(bitmap) -> 
        image.Source <- bitmap

      | Noneval ->
        let pixmap = Pixmap.create(width, height)
        for x = 0 to pixmap.lastX do
          let h = float32 x / float32 pixmap.lastX
          for y = 0 to pixmap.lastY do
            let v = 1.0f - 0.9f * float32 y / float32 pixmap.lastY
            pixmap.[x, y] <- space.rgb(h, 1.0f, v)
        let bitmap = pixmap.bitmapSource()
        image.Source <- bitmap
        bitmapCache.[spaceKey] <- bitmap

      let panel = StackPanel(Orientation = Orientation.Horizontal, HorizontalAlignment = HorizontalAlignment.Stretch)
      panel.add(image)
      panel.add(Label(Content = choices.name(i)))

      Some(box panel)

    | _ -> None



let fadeChoiceVisualizer (width : int) (height : int) =

  let width = float width
  let height = float height

  fun (parameter : DnaParameter) (i : int) ->

    match parameter.choices with

    | Some(:? Choices<float32 -> float32> as choices) ->
      let f = choices.value(i)

      let polygon = Polygon(Fill = Wpf.brush(0.25, 0.35, 0.6), StrokeThickness = 0.0)

      if parameter.name.EndsWith("wave") then
        let points = PointCollection()
        points.Add(Point(width, height))
        points.Add(Point(0.0, height))
        let n = int width - 2
        for i = 0 to n do
          points.Add(Point(1.0 + (width - 2.0) * float i / float n, height * 0.5 - (height - 2.0) * 0.5 * float (f (float32 i / float32 n))))
        polygon.Points <- points
      else
        let points = PointCollection()
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

    | _ -> None

