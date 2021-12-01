module Tests

open System
open Xunit
open FsUnit.Xunit
open AOC2021.Common
open AOC2021.Day1

[<Fact>]
let aoc1a_Sample () =
  [
    199
    200
    208
    210
    200
    207
    240
    269
    260
    263
  ]
  |> countIncreasingDepths 
  |> should equal 7

[<Fact>]
let aoc1a () =
  lines 1
  |> Seq.choose parseInts
  |> countIncreasingDepths 
  |> should equal 1616

[<Fact>]
let aoc1b_Sample () =
  [
    199
    200
    208
    210
    200
    207
    240
    269
    260
    263
  ]
  |> sumWindow 3
  |> countIncreasingDepths 
  |> should equal 5

[<Fact>]
let aoc1b () =
  lines 1
  |> Seq.choose parseInts
  |> sumWindow 3
  |> countIncreasingDepths 
  |> should equal 1645