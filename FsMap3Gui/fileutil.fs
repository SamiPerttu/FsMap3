module FsMap3.FileUtil

open System
open System.Windows
open System.Windows.Media
open System.Windows.Shapes
open System.Windows.Controls
open System.Windows.Controls.Primitives

open Common


/// Retrieves the contents of a directory and its subdirectories, recursively.
/// Returns a pair of lists of directories and files where each entry is a pair (relative path, absolute path).
let getDirectoryContents (path : string) =
  let directories = Darray.create()
  let files = Darray.create()
  let rec addContents (relativePath : string) (absolutePath : string) =
    for directory in System.IO.Directory.GetDirectories(absolutePath) do
      let relativePath' = relativePath + System.IO.Path.GetFileName(directory) + "/"
      directories.add(relativePath', directory)
      addContents relativePath' directory
    for file in System.IO.Directory.GetFiles(absolutePath) do
      files.add(relativePath + System.IO.Path.GetFileName(file), file)
  try
    addContents "" path
  with
    | _ -> ()
  directories, files


/// Asks the user if deletion of file is really wanted. Returns true if the file is gone.
let confirmDelete deleteVerb (file : string) =
  if IO.File.Exists(file) then
    match MessageBox.Show("Are you sure you want to " + deleteVerb + " '" + System.IO.Path.GetFileNameWithoutExtension(file) + "'?", "Please confirm", MessageBoxButton.YesNo, MessageBoxImage.Question) with
    | MessageBoxResult.Yes ->
      try
        System.IO.File.Delete(file)
        true
      with
        | _ -> false
    | _ ->
      false
  else
    true
