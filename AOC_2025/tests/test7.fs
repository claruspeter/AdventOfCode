module Tests.Day7

open System
open Xunit
open FsUnit.Xunit
open AOC2023.Common
open AOC2023.Day7

let sample = [
  ".......S......."
  "..............."
  ".......^......."
  "..............."
  "......^.^......"
  "..............."
  ".....^.^.^....."
  "..............."
  "....^.^...^...."
  "..............."
  "...^.^...^.^..."
  "..............."
  "..^...^.....^.."
  "..............."
  ".^.^.^.^.^...^."
  "..............."
]

[<Fact>]
let A_Sample () =
  let result =
    sample
    |> parse
    |> traceTachyons
  result
  |> _.numSplit |> should equal 21

[<Fact>]
let A () =
  let result =
    lines 7
    |> parse
    |> traceTachyons
  result
  |> _.numSplit |> should equal 1615


[<Fact>]
let B_Sample () =
  let result =
    sample
    |> parse
    |> traceTachyons
  // result.lines |> logseqf id |> ignore
  let extra = countExtraLifetimes result
  result.numSplit + extra |> should equal 40

[<Fact>]
let B () =
  let result =
    lines 7
    |> parse
    |> traceTachyons
  // result.lines |> logseqf id |> ignore
  let extra = countExtraLifetimes result
  result.numSplit + extra |> should equal 1899 // to low
