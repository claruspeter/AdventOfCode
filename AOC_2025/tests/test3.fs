module Tests.Day3

open System
open Xunit
open FsUnit.Xunit
open AOC2023.Common
open AOC2023.Day3

let sample = [
  "987654321111111"
  "811111111111119"
  "234234234234278"
  "818181911112111"
]

[<Fact>]
let A_Sample () =
  let result = 
    sample
    |> List.map findTwoBatteryJoltage
  result |> should equal [ 98; 89; 78; 92  ]
  result |> List.sum |> should equal 357

[<Fact>]
let A () =
  let result = 
    lines 3
    |> List.map findTwoBatteryJoltage
  result |> List.sum |> should equal 17452


[<Fact>]
let B_Sample () =
  let result = 
    sample
    |> List.map findTwelveBatteryJoltage
  result |> should equal [ 987654321111L; 811111111119L; 434234234278L; 888911112111L  ]
  result |> List.sum |> should equal 3121910778619L

[<Fact>]
let B () =
  let result = 
    lines 3
    |> List.map findTwelveBatteryJoltage
  result |> List.sum |> should equal 173300819005913L