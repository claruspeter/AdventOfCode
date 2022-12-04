module Tests.Day2

open System
open Xunit
open FsUnit.Xunit
open AOC2022.Common
open AOC2022.Day2

[<Fact>]
let A_Sample () =
  let data = [
    "A Y"
    "B X"
    "C Z"
  ]
  let rounds = data |> List.map parseRpsRound
  rounds |> Seq.length |> should equal 3
  rounds |> List.map (fun x -> x.us.value) |> should equal [ 2; 1; 3]
  rounds |> List.map (fun x -> x.score) |> should equal [ 8; 1; 6]

[<Fact>]
let A () =
  lines 2
  |> List.map parseRpsRound
  |> List.map (fun x -> x.score) 
  |> List.sum
  |> should equal 10595

[<Fact>]
let B_Sample () =
  let data = [
    "A Y"
    "B X"
    "C Z"
  ]
  let rounds = data |> List.map parseRpsRoundStrategically
  rounds |> Seq.length |> should equal 3
  rounds |> List.map (fun x -> x.us.value) |> should equal [ 1; 1; 1]
  rounds |> List.map (fun x -> x.score) |> should equal [ 4; 1; 7]

[<Fact>]
let B () =
  lines 2
  |> List.map parseRpsRoundStrategically
  |> List.map (fun x -> x.score) 
  |> List.sum
  |> should equal 9541