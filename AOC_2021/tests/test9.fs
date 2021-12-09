module Tests.Day9

open System
open Xunit
open FsUnit.Xunit
open AOC2021.Common
open AOC2021.Day9

let sample = [|
  "2199943210"
  "3987894921"
  "9856789892"
  "8767896789"
  "9899965678"
|]

[<Fact>]
let A_Sample () =
  let result = 
    sample
    |> parseHeightmap
    |> findLowPoints
  result |> should equal [1; 0; 5; 5]
  result |> riskLevel |> should equal 15

[<Fact>]
let A () =
  lines 9
  |> parseHeightmap
  |> findLowPoints
  |> riskLevel
  |> should equal 496


