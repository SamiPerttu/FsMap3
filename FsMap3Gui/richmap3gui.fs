namespace Fuse

open System
open System.Windows
open System.Windows.Media
open System.Windows.Shapes
open System.Windows.Controls
open System.Windows.Controls.Primitives

open Common


[<NoComparison; NoEquality>]
type AxisInfo =
  {
    label : string
    header : Label
    circle : Ellipse
    arrow : Polygon
    center : Label
    radius : Label
    isZ : bool
    headerColor : Brush
    tileColor : Brush
    zoomColor : Brush
  }

  member this.reset() =
    this.header.Foreground <- this.headerColor
    this.circle.Visibility <- Visibility.Hidden
    this.arrow.Visibility <- Visibility.Hidden
    this.center.Visibility <- Visibility.Hidden
    this.radius.Visibility <- Visibility.Hidden

  member this.update(tiles, center : float32, zoom, length : float32) =
    let zoomEpsilon = 1.0e-5f
    let zoomIsRight = this.isZ || (zoom <= 1.0f + zoomEpsilon && zoom >= 1.0f - zoomEpsilon)
    match tiles, zoomIsRight with
    | true, true ->
      this.header.Foreground <- this.tileColor
      this.header.ToolTip <- if this.isZ then "Z axis tiles." else this.label + " axis tiles in this view."
      this.circle.Visibility <- Visibility.Visible
      this.arrow.Visibility <- Visibility.Visible
      this.circle.Stroke <- this.tileColor
      this.arrow.Fill <- this.tileColor
    | true, false ->
      this.header.Foreground <- this.tileColor
      this.header.ToolTip <- this.label + " axis tiles at the default zoom level."
      this.circle.Visibility <- Visibility.Visible
      this.arrow.Visibility <- Visibility.Visible
      this.circle.Stroke <- this.zoomColor
      this.arrow.Fill <- this.zoomColor
    | false, _ ->
      this.header.Foreground <- this.headerColor
      this.header.ToolTip <- this.label + " axis does not tile."
      this.circle.Visibility <- Visibility.Hidden
      this.arrow.Visibility <- Visibility.Hidden
    this.center.Visibility <- Visibility.Visible
    this.center.Content <- Pretty.string center
    if not this.isZ then
      this.radius.Visibility <- Visibility.Visible
      this.radius.Content <- sprintf "±%s" (Pretty.string (length * 0.5f))

  static member create(canvas : Canvas, label, fontSize, x0, x1, x2, y, size, headerColor, tileColor, zoomColor) =
    let header = Label(Padding = Thickness(0.0), FontSize = fontSize, Content = label)
    canvas.add(header, x0, y)
    let circle = Ellipse(Width = size, Height = size, StrokeThickness = 0.8, Fill = Brushes.Transparent, IsHitTestVisible = false)
    canvas.add(circle, 1.0, y - 0.5)
    let arrow = Polygon(Points = Wpf.regularPolygon(3, 1.0f, float32 size * 0.5f, 2.0f, G tau * Q 3 4, 2.0f), IsHitTestVisible = false)
    canvas.add(arrow, 0.5, y)
    let center = Label(Padding = Thickness(0.0), FontSize = fontSize)
    canvas.add(center, x1, y)
    let radius = Label(Padding = Thickness(0.0), FontSize = fontSize)
    canvas.add(radius, x2, y)
    {
      label = label
      header = header
      circle = circle
      arrow = arrow
      center = center
      radius = radius
      isZ = label = "Z"
      headerColor = headerColor
      tileColor = tileColor
      zoomColor = zoomColor
    }
      


[<NoComparison; NoEquality>]
type RichMap3InfoBox =
  {
    canvas : Canvas
    detailLevel : Label
    xInfo : AxisInfo
    yInfo : AxisInfo
    zInfo : AxisInfo
    headerColor : Brush
    tileColor : Brush
  }

  static member create(width) =
    let fontSize    = 12.0
    let tileColor   = Wpf.brush(0.2, 0.65, 0.2)
    let headerColor = Wpf.brush(0.1, 0.35, 0.7)
    let zoomColor   = Wpf.brush(0.7, 0.9, 0.7)
    let bgColor     = Wpf.brush(0.92)
    let lineHeight  = 18.0

    let canvas = Canvas(Width = width, Height = 98.0, Margin = Thickness(0.0))

    let makeHeader content x y =
      let header = Label(Padding = Thickness(0.0), FontSize = fontSize, Foreground = headerColor, Content = content)
      canvas.add(header, x, y)
      header
    let makeText x y =
      let text = Label(Padding = Thickness(0.0), FontSize = fontSize)
      canvas.add(text, x, y)
      text

    let x0 = 6.5
    let x1 = 25.0
    let x2 = 88.0

    let detailLabel = makeHeader "Detail Level" x0 5.0
    detailLabel.ToolTip <- "Estimated resolution at which most of the detail is discernible."
    let detailLevel = makeText x2 5.0

    let yx = 30.0
    canvas.add(Rectangle(Width = width, Height = lineHeight + 2.0, Fill = bgColor), 0.0, yx - 1.0)
    let xInfo = AxisInfo.create(canvas, "X", fontSize, x0, x1, x2, yx, lineHeight, headerColor, tileColor, zoomColor)

    let yy = 52.0
    let yInfo = AxisInfo.create(canvas, "Y", fontSize, x0, x1, x2, yy, lineHeight, headerColor, tileColor, zoomColor)

    let yz = 74.0
    canvas.add(Rectangle(Width = width, Height = lineHeight + 2.0, Fill = bgColor), 0.0, yz - 1.0)
    let zInfo = AxisInfo.create(canvas, "Z", fontSize, x0, x1, x2, yz, lineHeight, headerColor, tileColor, zoomColor)

    {
      canvas = canvas
      detailLevel = detailLevel
      xInfo = xInfo
      yInfo = yInfo
      zInfo = zInfo
      headerColor = headerColor
      tileColor = tileColor
    }

  member this.reset() =
    this.detailLevel.Visibility <- Visibility.Hidden
    this.xInfo.reset()
    this.yInfo.reset()
    this.zInfo.reset()

  member this.update(map : RichMap3) =
    let tileX, tileY, tileZ = Map3Dna.doesItTile map.dna
    this.detailLevel.Content <- sprintf "%d px" (int map.detailLevel)
    this.xInfo.update(tileX, map.center.x, map.zoom, map.viewWidth)
    this.yInfo.update(tileY, map.center.y, map.zoom, map.viewHeight)
    this.zInfo.update(tileZ, map.center.z, map.zoom, 0.0f)
    this.detailLevel.Visibility <- Visibility.Visible

