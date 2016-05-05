namespace FsMap3

open Common


type Pretty =

  /// Pretty-converts a float to a string. The result is at most 9 characters long.
  /// Note: output is not meant as an F# float constant.
  static member string(x : float) =
    let a = abs x
    if a < 1.0e-7 then
      if x < 0.0 then "-0" elif x > 0.0 then "+0" else "0"
    elif a < 1.0e-4 then
      let s = sprintf "%.7f" x
      let rec trail (s : string) i = if s.[i] = '0' then trail s (i - 1) else (if s.[i] = '.' then i + 1 else i)
      s.[0 .. trail s (s.Length - 1)]
    elif a < 1.0e1 then
      let x = (round (x * 1.0e6)) / 1.0e6
      string x
    elif a < 1.0e6 then
      let e = log10 a |> floor |> max 1.0 |> (-) 6.0 |> exp10 |> round
      let x = (round (x * e)) / e
      string x
    elif a < 1.0e9 then
      string (round x)
    elif x = infinity then
      "infinity"
    elif x = -infinity then
      "-infinity"
    elif isNaN x then
      "NaN"
    else
      let e = floor (log10 a)
      let m = x / exp10 e
      let m = (round (m * 1.0e3)) / 1.0e3
      (string m) + "e" + (string e)

  /// Pretty-converts a float32 to a string. Note: output is not meant as an F# float32 constant.
  static member string(x : float32) = Pretty.string(float x)

