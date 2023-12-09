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
  |> walkThePath
  |> should equal ["AAA"; "CCC"; "ZZZ"]

[<Fact>]
let A_Sample2 () =
  sample2
  |> walkThePath
  |> should equal ["AAA"; "BBB"; "AAA"; "BBB"; "AAA"; "BBB"; "ZZZ"]

[<Fact>]
let A () =
  lines 8
  |> walkThePath
  |> Seq.length
  |> (+) -1
  |> should equal 11567




