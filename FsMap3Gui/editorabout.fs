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
      DnaData("-147G0ZW04r0-1lcK0ZPIoj003-ac3a0+-KSC0+nwml++VRah+-beny0-QY660-vgnQ5001YFuyXvZGbHq+YUAoyv2ZHJuV0Z4tN7s0-5ifTsYLEbW2001YFHaqOZ4tN7sZcCsB0hZFMGb0-vMky0099702-OXJ74YuAVeg-Pvmq02+cFxm0ZaTNW+ZV-l6++k3lO0-nxay0ZOl+h0-InEZ+ZjlbV00095302-OXJ74-wLre0ZyeUzK4YvVTOfYQOdavZ+QXLo+s3ae0-nxay0ZySyp0-gthW0ZH3Rn001+FHaqO+haza0"B)
      |]
    data.[Common.timeSeed() |> Mangle.mangle32 |> flip emod data.size].generate(Map3Dna.loadEditorMap)
  let w = 600.0
  let h = 300.0
  let bgImage = Image(Width = w, Height = h, SnapsToDevicePixels = true)
  let bgView = PixmapView(bgImage, int w, int h)
  bgView.start(RichMap3.pixmapSource(map))
  let aboutWindow = Window(Title = "About FsMap3 Editor", SizeToContent = SizeToContent.WidthAndHeight, ResizeMode = ResizeMode.NoResize)
  let aboutCanvas = Canvas(Width = w, Height = h)
  aboutCanvas.add(bgImage, 0.0, 0.0)
  let title = Label(Content = "FsMap3 Editor", FontSize = 40.0, FontWeight = bold, Effect = effect)
  let alpha = Label(Content = "alpha", FontSize = 20.0, FontWeight = bold, Effect = effect, Foreground = Wpf.brush(0.8, 0.0, 0.0))
  let version = Label(Content = "Version " + Map3Dna.EditorVersion, FontSize = 16.0, FontWeight = bold, Effect = effect)
  aboutCanvas.add(title, 10.0, 5.0)
  aboutCanvas.add(alpha, 12.0, 50.0)
  aboutCanvas.add(version, 12.0, 75.0)
  let copyright = Label(Content = "© Copyright 2016 Sami Perttu", FontSize = 20.0, FontWeight = medium, Effect = effect)
  aboutCanvas.add(copyright, 30.0, 120.0)
  let license = Label(Content = "This program is distributed under the MIT license.", FontSize = 16.0, FontWeight = medium, Effect = effect)
  aboutCanvas.add(license, 33.0, 150.0)
  //let license2 = Label(Content = "See the file LICENSE.md for more details.", FontSize = 16.0, FontWeight = medium, Effect = effect)
  //aboutCanvas.add(license2, 30.0, 172.0)
  let closeButton = Button(Content = "Close", Width = 100.0, Height = 25.0, Background = Wpf.brush(1.0, 1.0, 1.0, 0.3), withClick = fun _ -> aboutWindow.Close())
  aboutCanvas.add(closeButton, 490.0, 265.0)

  let makeInfoLabel text = Label(Content = text, FontSize = 13.0, Effect = effect, Foreground = Wpf.brush(0.1, 0.0, 0.8))

  let ix1, ix2 = 40.0, 140.0
  let iy = 205.0
  aboutCanvas.add(makeInfoLabel "OS version", ix1, iy)
  aboutCanvas.add(makeInfoLabel (string System.Environment.OSVersion), ix2, iy)
  aboutCanvas.add(makeInfoLabel "CLR version", ix1, iy + 15.0)
  aboutCanvas.add(makeInfoLabel (string System.Environment.Version), ix2, iy + 15.0)
  aboutCanvas.add(makeInfoLabel "Address space", ix1, iy + 30.0)
  aboutCanvas.add(makeInfoLabel (if sizeof<System.IntPtr> = 4 then "32-bit" else "64-bit"), ix2, iy + 30.0)
  aboutCanvas.add(makeInfoLabel "SIMD enabled", ix1, iy + 45.0)
  aboutCanvas.add(makeInfoLabel (if System.Numerics.Vector.IsHardwareAccelerated then "yes" else "no"), ix2, iy + 45.0)
  aboutCanvas.add(makeInfoLabel "Processors", ix1, iy + 60.0)
  aboutCanvas.add(makeInfoLabel (string System.Environment.ProcessorCount), ix2, iy + 60.0)

  aboutWindow.Content <- aboutCanvas
  aboutWindow.ShowDialog() |> ignore
  bgView.stop()

