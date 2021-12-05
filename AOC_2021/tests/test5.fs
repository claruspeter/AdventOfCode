module Tests.Day5

open System
open Xunit
open FsUnit.Xunit
open AOC2021.Common
open AOC2021.Day5

let sample = [
  "0,9 -> 5,9"
  "8,0 -> 0,8"
  "9,4 -> 3,4"
  "2,2 -> 2,1"
  "7,0 -> 7,4"
  "6,4 -> 2,0"
  "0,9 -> 2,9"
  "3,4 -> 1,4"
  "0,0 -> 8,8"
  "5,5 -> 8,2"
]

[<Fact>]
let A_Sample () =
  sample
  |> List.map parseVent
  |> List.filter (fun (v:Vent) -> v.a.x = v.b.x || v.a.y = v.b.y)
  |> mapVents
  |> List.filter (fun x -> x.score > 1)
  |> List.length
  |> should equal 5
  
[<Fact>]
let A () =
  lines 5 |> Array.toList
  |> List.map parseVent
  |> List.filter (fun (v:Vent) -> v.a.x = v.b.x || v.a.y = v.b.y)
  |> mapVents
  |> List.filter (fun x -> x.score > 1)
  |> List.length
  |> should equal 6267

[<Fact>]
let B_Sample () =
  sample
  |> List.map parseVent
  |> mapVents
  |> List.filter (fun x -> x.score > 1)
  |> List.length
  |> should equal 12

[<Fact>]
let B () =
  lines 5 |> Array.toList
  |> List.map parseVent
  |> mapVents
  |> List.filter (fun x -> x.score > 1)
  |> List.length
  |> should equal 20196  