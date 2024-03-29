module Tests.Day4

open System
open Xunit
open FsUnit.Xunit
open AOC2023.Common
open AOC2023.Day4

let sample = [
  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
  "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
  "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
  "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
  "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
  "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
]

[<Fact>]
let A_Sample () =
  sample
  |> cardPoints
  |> should equal [8;2;2;1;0;0]

[<Fact>]
let A () =
  lines 4
  |> cardPoints
  |> List.sum
  |> should equal 20855


[<Fact>]
let B_Sample () =
  sample
  |> totalCardCounts
  |> should equal [1; 2; 4; 8; 14; 1]
  
[<Fact>]
let B () =
  lines 4
  |> totalCardCounts
  |> List.sum
  |> should equal 5489600