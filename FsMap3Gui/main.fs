/// Startup code.
module FsMap3.Main

open Common

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Markup

type App() as app =
  inherit System.Windows.Application()  
  do app.Startup.Add(fun _ ->
    //Log.createLogFile("fsmap3.log")
    Editor.start()
    )

[<STAThread>]
[<EntryPoint>]
let main argv = 
    let application = App()
    application.Run()

