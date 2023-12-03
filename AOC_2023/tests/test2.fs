module Tests.Day2

open System
open Xunit
open FsUnit.Xunit
open AOC2023.Common
open AOC2023.Day2

let sample = [
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
  "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
  "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
  "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
  "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
]

[<Fact>]
let A_Sample () =
  sample
  |> findPossibleGames {n=0; red=12; green=13; blue=14}
  |> should equal [1;2;5]

[<Fact>]
let A () =
  lines 2
  |> findPossibleGames {n=0; red=12; green=13; blue=14}
  |> List.sum
  |> should equal 2913

[<Fact>]
let B_Sample () =
  sample
  |> findMinimumGames
  |> List.map gamePower
  |> should equal [48; 12; 1560; 630; 36]

[<Fact>]
let B () =
  lines 2
  |> findMinimumGames
  |> List.map gamePower
  |> List.sum
  |> should equal 55593