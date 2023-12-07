module Tests.Day6

open System
open Xunit
open FsUnit.Xunit
open AOC2023.Common
open AOC2023.Day6

let sample = [
  "Time:      7  15   30"
  "Distance:  9  40  200"
]

[<Fact>]
let A_Sample () =
  sample
  |> numberWaysToWin
  |> should equal [4;8;9]

[<Fact>]
let A () =
  lines 6
  |> numberWaysToWin
  |> List.reduce (*)
  |> should equal 1195150

[<Fact>]
let B_Sample () =
  sample
  |> numberWaysToWinLong
  |> should equal [71503]

// [<Fact>]
// let B () =
//   lines 6
//   |> numberWaysToWinLong
//   |> should equal [42550411]