/// String utilities.
module Fuse.Tome

open Common


type TomeBuilder() =
  member __.Yield(s : string) = [s]
  member __.Yield(c : char) = [string c]
  // /// Yields the byte as an ASCII character.
  // member __.Yield(c : byte) = [string (char c)]
  member __.Yield(f : float) = [Pretty.string(f)]
  member __.Yield(f : float32) = [Pretty.string(f)]
  member __.Yield(i : int) = [string i]
  member __.Yield(i : int64) = [string i]
  member __.Yield(u : uint) = [string u]
  member __.Yield(u : uint64) = [string u]
  member __.YieldFrom(s) = s
  member __.Combine(a, b) = a @ b
  member __.Delay(f) = f()
  member __.Zero() = []
  member __.For(xs : 'a seq, f : 'a -> string list) = xs |> Seq.fold (fun s x -> s @ f x) []


/// A workflow for building strings. Yielded strings are placed in a list.
let tome = TomeBuilder()


/// Concatenates strings from the tome workflow into a single string.
let buildTome t = String.concat "" t

