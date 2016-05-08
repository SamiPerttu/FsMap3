namespace FsMap3

open System
open System.Windows
open System.Windows.Media
open System.Windows.Shapes
open System.Windows.Controls
open System.Windows.Controls.Primitives

open Common


[<NoComparison; NoEquality>]
type RichMap3InfoBox =
  {
    container : Border
    panel : StackPanel
    xCenter : Label
    xRadius : Label
    yCenter : Label
    yRadius : Label
    zText : Label
  }

  static member create(width) =
    let headerFontSize = 13.0
    let textFontSize = 11.0
    let itemMargin = Thickness(0.0, -3.0, 0.0, -3.0)
    let textMargin = Thickness(-4.0, -3.0, 0.0, -3.0)
    let headerColor = Wpf.brush(0.1, 0.35, 0.6)

    let makeHeader content =
      Label(Margin = Thickness(0.0), FontSize = headerFontSize, FontWeight = FontWeights.Bold, Content = content, Foreground = headerColor)
    let makeText() =
      Label(Margin = textMargin, FontSize = textFontSize, VerticalAlignment = VerticalAlignment.Center)
    let makePanel() =
      StackPanel(Margin = itemMargin, Orientation = Orientation.Horizontal, HorizontalAlignment = HorizontalAlignment.Left)

    let panel = StackPanel(Width = width, Margin = Thickness(0.0), Orientation = Orientation.Vertical, HorizontalAlignment = HorizontalAlignment.Center, Visibility = Visibility.Hidden)
    let xPanel = makePanel()
    xPanel.add(makeHeader "X")
    let xCenter = makeText()
    xCenter.Width <- width * 0.4
    xPanel.add(xCenter)
    let xRadius = makeText()
    xPanel.add(xRadius)
    let yPanel = makePanel()
    yPanel.add(makeHeader "Y")
    let yCenter = makeText()
    yCenter.Width <- width * 0.4
    yPanel.add(yCenter)
    let yRadius = makeText()
    yPanel.add(yRadius)
    let zPanel = makePanel()
    zPanel.add(makeHeader "Z")
    let zText = makeText()
    zPanel.add(zText)
    panel.add(xPanel)
    panel.add(yPanel)
    panel.add(zPanel)

    let container = Border(Margin = Thickness(0.0, 0.0, 1.0, 1.0),
                           Background = Wpf.verticalBrush(Wpf.color(1.0), Wpf.color(0.85), 0.3),
                           Padding = Thickness(1.0),
                           BorderBrush = Wpf.verticalBrush(Wpf.color(0.8), Wpf.color(0.4)),
                           BorderThickness = Thickness(1.0),
                           CornerRadius = CornerRadius(4.0),
                           VerticalAlignment = VerticalAlignment.Bottom)
    container.Child <- panel

    {
      RichMap3InfoBox.container = container
      panel = panel
      xCenter = xCenter
      xRadius = xRadius
      yCenter = yCenter
      yRadius = yRadius
      zText = zText
    }

  member this.reset() =
    this.panel.Visibility <- Visibility.Hidden

  member this.update(map : RichMap3) =
    this.xCenter.Content <- Pretty.string map.center.x
    this.xRadius.Content <- sprintf "±%s" (Pretty.string (map.viewWidth * 0.5f))
    this.yCenter.Content <- Pretty.string map.center.y
    this.yRadius.Content <- sprintf "±%s" (Pretty.string (map.viewHeight * 0.5f))
    this.zText.Content <- sprintf "%s" (Pretty.string map.center.z)
    this.panel.Visibility <- Visibility.Visible
