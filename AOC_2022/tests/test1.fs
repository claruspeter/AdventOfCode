module Tests.Day1

open System
open Xunit
open FsUnit.Xunit
open AOC2022.Common
open AOC2022.Day1

[<Fact>]
let A_Sample () =
  let lines = [
    "1000"
    "2000"
    "3000"
    ""
    "4000"
    ""
    "5000"
    "6000"
    ""
    "7000"
    "8000"
    "9000"
    ""
    "10000"
  ]

  let byElf = lines |> groupCalories
  byElf |> List.length |> should equal 5
  let sums = byElf |> List.map (fun elf -> elf |> List.sum) 
  sums |> should equal [6000L; 4000L; 11000L; 24000L; 10000L]




