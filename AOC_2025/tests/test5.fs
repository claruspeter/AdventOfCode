module Tests.Day5

open System
open Xunit
open FsUnit.Xunit
open AOC2023.Common
open AOC2023.Day5

let sample = [
  "3-5"
  "10-14"
  "16-20"
  "12-18"
  ""
  "1"
  "5"
  "8"
  "11"
  "17"
  "32"
]

[<Fact>]
let A_Sample () =
  sample
  |> parse
  |> findSpoiled
  |> should equal [ 1L; 8L; 32L]

[<Fact>]
let A () =
  lines 5
  |> parse
  |> findFresh
  |> _.Length |> should equal 737


[<Fact>]
let B_Sample () =
  let result =
    sample
    |> parse
    |> countFresh
  result |> should equal 14L

[<Fact>]
let B () =
  let result =
    lines 5
    |> parse
    |> countFresh
  result |> should equal 304637184388846L