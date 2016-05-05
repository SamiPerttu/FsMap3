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
    xPanel : StackPanel
    xText : Label
    yPanel : StackPanel
    yText : Label
    zPanel : StackPanel
    zText : Label
  }

  static member create(width) =
    let font = FontFamily("Lucida Console")
    let headerFontSize = 13.0
    let textFontSize = 11.0
    let itemMargin = Thickness(0.0, -3.0, 0.0, -3.0)
    let textMargin = Thickness(-4.0, -3.0, 0.0, -3.0)
    let headerColor = Wpf.brush(0.1, 0.35, 0.6)

    let makeHeader content =
      Label(Margin = Thickness(0.0), FontFamily = font, FontSize = headerFontSize, FontWeight = FontWeights.Bold, Content = content, Foreground = headerColor)

    let panel = StackPanel(Width = width, Margin = Thickness(0.0), Orientation = Orientation.Vertical, HorizontalAlignment = HorizontalAlignment.Center, Visibility = Visibility.Hidden)
    let xPanel = StackPanel(Margin = itemMargin, Orientation = Orientation.Horizontal, HorizontalAlignment = HorizontalAlignment.Left)
    xPanel.add(makeHeader "X")
    let xText = Label(Margin = textMargin, FontFamily = font, FontSize = textFontSize, VerticalAlignment = VerticalAlignment.Center)
    xPanel.add(xText)
    let yPanel = StackPanel(Margin = itemMargin, Orientation = Orientation.Horizontal, HorizontalAlignment = HorizontalAlignment.Left)
    yPanel.add(makeHeader "Y")
    let yText = Label(Margin = textMargin, FontFamily = font, FontSize = textFontSize, VerticalAlignment = VerticalAlignment.Center)
    yPanel.add(yText)
    let zPanel = StackPanel(Margin = itemMargin, Orientation = Orientation.Horizontal, HorizontalAlignment = HorizontalAlignment.Left)
    zPanel.add(makeHeader "Z")
    let zText = Label(Margin = textMargin, FontFamily = font, FontSize = textFontSize, VerticalAlignment = VerticalAlignment.Center)
    zPanel.add(zText)
    panel.add(xPanel)
    panel.add(yPanel)
    panel.add(zPanel)
    let container = Border(Margin = Thickness(0.0, 0.0, 1.0, 1.0), Background = Wpf.verticalBrush(Wpf.color(1.0), Wpf.color(0.85), 0.3), Padding = Thickness(1.0), BorderBrush = Wpf.verticalBrush(Wpf.color(0.8), Wpf.color(0.4)), BorderThickness = Thickness(1.0), CornerRadius = CornerRadius(4.0), VerticalAlignment = VerticalAlignment.Bottom)
    container.Child <- panel
    {
      RichMap3InfoBox.container = container
      panel = panel
      xPanel = xPanel
      xText = xText
      yPanel = yPanel
      yText = yText
      zPanel = zPanel
      zText = zText
    }

  member this.reset() =
    this.panel.Visibility <- Visibility.Hidden

  member this.update(map : RichMap3) =

    let w = map.viewWidth
    let h = map.viewHeight

    this.xText.Content <- sprintf "%-9s ±%s" (Pretty.string map.center.x) (Pretty.string (w * 0.5f))
    this.yText.Content <- sprintf "%-9s ±%s" (Pretty.string map.center.y) (Pretty.string (h * 0.5f))
    this.zText.Content <- sprintf "%s" (Pretty.string map.center.z)

    this.panel.Visibility <- Visibility.Visible
