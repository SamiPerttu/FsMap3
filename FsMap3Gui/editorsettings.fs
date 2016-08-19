namespace FsMap3

open System
open System.Xml

open Common

/// FsMap3 editor settings.
type EditorSettings() =

  member val recentFiles : string[] = Array.empty with get, set

  /// Adds a file to the list of recent files.
  member this.addRecent(file) =
    this.recentFiles <- this.recentFiles |> Array.filter ((<>) file) |> Array.append [| file |]

  static member directory = 
    let appDataPath = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData)
    System.IO.Path.Combine(appDataPath, "FsMap3Editor")

  static member path =
    System.IO.Path.Combine(EditorSettings.directory, "settings.xml")

  /// Reads settings.
  static member read() =
    try
      Log.infof "Loading editor settings from '%s'." EditorSettings.path
      let doc = XmlDocument()
      doc.Load(EditorSettings.path)
      let recentFiles =
        let nodes = doc.SelectNodes("recentFiles/file")
        Array.init nodes.Count (fun i -> nodes.[i].InnerText)
      EditorSettings(recentFiles = recentFiles)
    with
      | _ -> EditorSettings()

  /// Writes these settings.
  member this.write() =
    try
      System.IO.Directory.CreateDirectory(EditorSettings.directory) |> ignore
      let doc = XmlDocument()
      let recentFilesNode = doc.CreateElement("recentFiles")
      for file in this.recentFiles do
        let fileNode = doc.CreateElement("file", InnerText = file)
        recentFilesNode.AppendChild(fileNode) |> ignore
      doc.AppendChild(recentFilesNode) |> ignore
      doc.Save(EditorSettings.path)
    with
      | _ -> ()

