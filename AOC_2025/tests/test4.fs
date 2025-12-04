module Tests.Day4

open System
open Xunit
open FsUnit.Xunit
open AOC2023.Common
open AOC2023.Day4

let sample = [
  "..@@.@@@@."
  "@@@.@.@.@@"
  "@@@@@.@.@@"
  "@.@@@@..@."
  "@@.@@@@.@@"
  ".@@@@@@@.@"
  ".@.@.@.@@@"
  "@.@@@.@@@@"
  ".@@@@@@@@."
  "@.@.@@@.@."
]

[<Fact>]
let A_Sample () =
  let cells =
    sample
    |> parse
    |> countAdjacentRolls
  cells |> should haveLength 100
  // cells |> asGridString 10 10 |> log |> ignore
  cells
    |> List.filter _.isRemovable
    |> should haveLength 13

[<Fact>]
let A () =
  let cells =
    lines 4
    |> parse
    |> countAdjacentRolls
  cells
    |> List.filter _.isRemovable
    |> List.length |> should equal 1395

[<Fact>]
let B_Sample () =
  let cells =
    sample
    |> parse

  let removed = removeUntilImpossible cells
  removed |> should equal [13; 12; 7; 5; 2; 1; 1; 1; 1;]
  removed |> List.sum |> should equal 43

[<Fact>]
let B () =
  let cells =
    lines 4
    |> parse

  let removed = removeUntilImpossible cells
  removed |> List.sum |> should equal 8451
