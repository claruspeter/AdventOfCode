module Tests.Day8

open System
open Xunit
open FsUnit.Xunit
open AOC2023.Common
open AOC2023.Day8

let sample =[
  "RL"
  ""
  "AAA = (BBB, CCC)"
  "BBB = (DDD, EEE)"
  "CCC = (ZZZ, GGG)"
  "DDD = (DDD, DDD)"
  "EEE = (EEE, EEE)"
  "GGG = (GGG, GGG)"
  "ZZZ = (ZZZ, ZZZ)"
]

let sample2 = [
  "LLR"
  ""
  "AAA = (BBB, BBB)"
  "BBB = (AAA, ZZZ)"
  "ZZZ = (ZZZ, ZZZ)"
]

[<Fact>]
let A_Sample () =
  sample
  |> walkThePath starterSimple
  |> List.collect id
  |> should equal ["AAA"; "CCC"; "ZZZ"]

[<Fact>]
let A_Sample2 () =
  sample2
  |> walkThePath starterSimple
  |> List.collect id
  |> should equal ["AAA"; "BBB"; "AAA"; "BBB"; "AAA"; "BBB"; "ZZZ"]

[<Fact>]
let A () =
  lines 8
  |> walkThePath starterSimple
  |> List.collect id
  |> Seq.length
  |> (+) -1
  |> should equal 11567

let sampleB = [
  "LR"
  ""
  "11A = (11B, XXX)"
  "11B = (XXX, 11Z)"
  "11Z = (11B, XXX)"
  "22A = (22B, XXX)"
  "22B = (22C, 22C)"
  "22C = (22Z, 22Z)"
  "22Z = (22B, 22B)"
  "XXX = (XXX, XXX)"
]

[<Fact>]
let B_Sample () =
  sampleB
  |> walkThePath starterGhost
  |> should equal [
      ["11A"; "22A"]
      ["11B"; "22B"]
      ["11Z"; "22C"]
      ["11B"; "22Z"]
      ["11Z"; "22B"]
      ["11B"; "22C"]
      ["11Z"; "22Z"]
    ]


// [<Fact>]
// let B () =
//   lines 8
//   |> walkThePath starterGhost
//   |> Seq.length
//   |> (+) -1
//   |> should equal 1