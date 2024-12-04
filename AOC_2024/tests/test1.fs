module Tests.Day1

open System
open Xunit
open FsUnit.Xunit
open AOC2023.Common
open AOC2023.Day1

let sample =
  [
    "3   4"
    "4   3"
    "2   5"
    "1   3"
    "3   9"
    "3   3"
  ]

[<Fact>]
let A_Sample () =
  sample
  |> calcDistances
  |> should matchList [2; 1; 0; 1; 2; 5]
  

[<Fact>]
let A () =
  lines 1
  |> calcDistances
  |> List.sum
  |> should equal 2057374

[<Fact>]
let B_Sample () =
  sample
  |> calcSimilarity
  |> should equal [9; 4; 0; 0; 9; 9]

[<Fact>]
let B () =
  lines 1
  |> calcSimilarity
  |> List.sum
  |> should equal 23177084