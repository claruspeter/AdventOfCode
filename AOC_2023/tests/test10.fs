module Tests.Day10

open System
open Xunit
open FsUnit.Xunit
open AOC2023.Common
open AOC2023.Day10

let sample1 = [
  "-L|F7"
  "7S-7|"
  "L|7||"
  "-L-J|"
  "L|-JF"
]

[<Fact>]
let A_Sample () =
  sample1
  |> findDistanceFromStart
  |> sprintf "%O"
  |> should equal (
      Join "\n"
        [
          "....."
          ".012."
          ".1.3."
          ".234."
          "....."
        ]
  )




