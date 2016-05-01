#r @"bin\Release\System.Numerics.Vectors.dll"
#r @"bin\Release\FsMap3.dll"
#r @"bin\Release\FsMap3Gui.exe"

open FsMap3
open Common
open Map3
open Basis3
open Perlin
open Cubex
open Leopard
open Worley
open Map3Gui

show 800 (fractal 0.5f 0.5f 2.0f 9 (cubexf Fade.sine) |> color 1)
show 800 (leopardd 1.0f 3.2f >> leopardd 1.0f 2.5f >> leopardd 1.0f 3.0f >> leopardd 1.0f 2.0f |> color 15)
show 800 (fractald 0.6f 0.5f 0.0f Mix.sum (Walk.march 1.5f Fade.power2) 2.5f 8 perlind |> color 14)
show 800 (worleyd 3 cellNorm4 Fade.power2 12.0f |> color 44)

