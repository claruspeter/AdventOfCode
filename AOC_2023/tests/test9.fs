module Tests.Day9

open System
open Xunit
open FsUnit.Xunit
open AOC2023.Common
open AOC2023.Day9

let sample = [
  "0 3 6 9 12 15"
  "1 3 6 10 15 21"
  "10 13 16 21 30 45"
]

[<Fact>]
let A_Sample () =
  sample
  |> predictNext
  |> should equal [18;28;68]

[<Fact>]
let A () =
  lines 9
  |> predictNext
  |> List.sum
  |> should equal 1887980197

[<Fact>]
let B_Sample () =
  sample
  |> predictPrev
  |> should equal [-3;0;5]

[<Fact>]
let B () =
  lines 9
  |> predictPrev
  |> List.sum
  |> should equal 990