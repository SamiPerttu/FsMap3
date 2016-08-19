// Logging services.
namespace FsMap3

open Common


type LogCategory = Info | Warning | Error



[<NoComparison; NoEquality>]
type LogEntry =
  {
    time : float
    category : LogCategory
    message : string
  }

  static member create(category, message) = {
    LogEntry.time = Common.timeNow()
    category = category
    message = message
  }

  override this.ToString() =
    match this.category with
    | Warning ->
      sprintf "Warning: %s" this.message
    | Error ->
      sprintf "Error: %s" this.message
    | Info ->
      sprintf "%s" this.message



[<NoComparison; NoEquality>]
type LogListener = LogListener of (LogEntry -> unit)



type Log() =

  /// Log entries retained in memory. Also used as a synchronization object.
  static let memory = System.Collections.Generic.Queue<LogEntry>()

  /// Maximum number of entries retained in memory.
  static let mutable memorySize = 16384

  /// Log listeners.
  static let listenerList = System.Collections.Generic.List<LogListener>()

  /// Filter that determines which entries are printed to standard streams.
  static let mutable consoleFilter = (fun (entry : LogEntry) -> true)


  static member private send(entry : LogEntry) =
    lock memory (fun _ ->
      while memory.Count >= memorySize do
        memory.Dequeue() |> ignore
      memory.Enqueue(entry)
      for i = 0 to listenerList.Count - 1 do
        let (LogListener(f)) = listenerList.[i]
        f entry
      if consoleFilter entry then
        match entry.category with
        | Warning | Error ->
          eprintfn "Log %s" (entry.ToString())
        | Info ->
          printfn "Log Info: %s" (entry.ToString())
    )


  // PUBLIC INTERFACE


  static member setMemorySize(size) =
    enforce (size > 0) "Log.setMemorySize: Size must be greater than zero."
    lock memory (fun _ ->
      memorySize <- size
      while memory.Count >= memorySize do
        memory.Dequeue() |> ignore
    )


  static member iterMemory(f) =
    lock memory (fun _ ->
      for entry in memory do f entry
      )


  static member createLogFile(filename : string) =
    let stream = new System.IO.StreamWriter(filename, AutoFlush = true)
    Log.addListener(LogListener(fun (entry : LogEntry) ->
      stream.WriteLine(string entry)
      ))


  static member setConsoleFilter(filter) =
    lock memory (fun _ ->
      consoleFilter <- filter
      )


  static member addListener(listener) =
    lock memory (fun _ ->
      listenerList.Add(listener)
      )


  static member removeListener(listener) =
    lock memory (fun _ ->
      enforce (listenerList.Remove(listener)) "Log.removeListener: Cannot find listener."
      )


  static member info(message : string) =
    let entry = LogEntry.create(Info, message)
    Log.send(entry)


  static member infof(format) =
    Printf.kprintf Log.info format


  static member warn(message : string) =
    let entry = LogEntry.create(Warning, message)
    Log.send(entry)


  static member warnf(format) =
    Printf.kprintf Log.warn format


  