module FsMap3.EditorAbout

open Common

open System
open System.Windows
open System.Windows.Media
open System.Windows.Shapes
open System.Windows.Controls
open System.Windows.Controls.Primitives


let showAboutWindow() =
  let bold = FontWeight.FromOpenTypeWeight(900)
  let medium = FontWeight.FromOpenTypeWeight(500)
  let effect = Effects.DropShadowEffect(BlurRadius = 3.0, Color = Wpf.color(1.0), Opacity = 1.0, ShadowDepth = 0.0)
  let map =
    let data = [|
      DnaData("ZSjV4+ZJwJ10-1lcK0ZRQ10+03-ac3a0+-KSC0+nwml++VRah+-beny0-QY660-OMGC0001YFuyXvZGbHq+YUAoyv2ZHJuV0Zp0lt00ZvOAd0Y5KfMA0ZtGet010dZFMGb0-vMky008701ZK8nC0YENchw-Pvmq04+cFxm0ZaTNW+ZQlHo++k3lO0-nxay0ZOl+h0-InEZ+ZjlbV0008302-OXJ7m+mkAi0YKlGOw4YvVTOfYQOdavZV-l6++s3ae0-nxay0ZySyp0-gthW0ZjlbV001+ZMO20+haza0"B)
      |]
    data.[Common.timeSeed() |> Mangle.mangle32 |> flip emod data.size].generate(Map3Dna.generateEditorMap)
  let w = 600.0
  let h = 300.0
  let bgImage = Image(Width = w, Height = h, SnapsToDevicePixels = true)
  let bgView = PixmapView(bgImage, int w, int h)
  bgView.start(RichMap3.pixmapSource(map))
  let aboutWindow = Window(Title = "About FsMap3 Editor", SizeToContent = SizeToContent.WidthAndHeight, ResizeMode = ResizeMode.NoResize)
  let aboutCanvas = Canvas(Width = w, Height = h)
  aboutCanvas.add(bgImage, 0.0, 0.0)
  let title = Label(Content = "FsMap3 Editor", FontSize = 40.0, FontWeight = bold, Effect = effect)
  let version = Label(Content = "Version " + Map3Dna.EditorVersion, FontSize = 16.0, FontWeight = bold, Effect = effect)
  aboutCanvas.add(title, 10.0, 10.0)
  aboutCanvas.add(version, 12.0, 60.0)
  let copyright = Label(Content = "© Copyright 2016 Sami Perttu", FontSize = 20.0, FontWeight = medium, Effect = effect)
  aboutCanvas.add(copyright, 30.0, 120.0)
  let license = Label(Content = "This program is distributed under the MIT license.", FontSize = 16.0, FontWeight = medium, Effect = effect)
  aboutCanvas.add(license, 30.0, 150.0)
  let license2 = Label(Content = "See the file LICENSE.md for more details.", FontSize = 16.0, FontWeight = medium, Effect = effect)
  aboutCanvas.add(license2, 30.0, 172.0)
  let closeButton = Button(Content = "Close", Width = 100.0, Height = 25.0, Background = Wpf.brush(1.0, 1.0, 1.0, 0.3), withClick = fun _ -> aboutWindow.Close())
  aboutCanvas.add(closeButton, 490.0, 265.0)
  aboutWindow.Content <- aboutCanvas
  aboutWindow.ShowDialog() |> ignore
  bgView.stop()

