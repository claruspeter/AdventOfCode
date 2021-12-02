module Tests2

open System
open Xunit
open FsUnit.Xunit
open AOC2021.Common
open AOC2021.Day2

[<Fact>]
let aoc2a_Sample () =
  let position = 
    [
      "forward 5"
      "down 5"
      "forward 8"
      "up 3"
      "down 8"
      "forward 2"
    ]
    |> Seq.choose parseDirection
    |> Seq.fold moveInDirection {x = 0; depth = 0}
  position |> should equal {x = 15; depth = 10}


[<Fact>]
let aoc2a () =
  let position = 
    lines 2
    |> Seq.choose parseDirection
    |> Seq.fold moveInDirection {x = 0; depth = 0}
  let vector = position.x * position.depth
  vector |> should equal 1690020

[<Fact>]
let aoc2b_Sample () =
  let position = 
    [
      "forward 5"
      "down 5"
      "forward 8"
      "up 3"
      "down 8"
      "forward 2"
    ]
    |> Seq.choose parseDirection
    |> Seq.fold moveInAimedDirection {posn={x = 0; depth = 0}; aim = 0}
  position.posn |> should equal {x = 15; depth = 60}

[<Fact>]
let aoc2b () =
  let position = 
    lines 2
    |> Seq.choose parseDirection
    |> Seq.fold moveInAimedDirection {posn={x = 0; depth = 0}; aim = 0}
  position.posn |> should equal {x = 1845; depth = 763408}
  (position.posn.x * position.posn.depth) |> should equal 1408487760