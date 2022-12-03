module Tests.Day1

open System
open Xunit
open FsUnit.Xunit
open AOC2022.Common
open AOC2022.Day1

[<Fact>]
let A_Sample () =
  let input = [
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
  let byElf = input |> groupCalories
  byElf |> List.length |> should equal 5
  let sums = byElf |> List.map (fun elf -> elf |> List.sum) 
  sums |> should equal [6000L; 4000L; 11000L; 24000L; 10000L]

[<Fact>]
let A () =
  let input = lines 1
  let byElf = input |> groupCalories
  let sums = byElf |> Seq.map (fun elf -> elf |> Seq.sum) 
  sums |> Seq.max |> should equal 70116L


[<Fact>]
let B_Sample () =
  let input = [
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
  let byElf = input |> groupCalories
  byElf |> List.length |> should equal 5
  let sums = byElf |> List.map (fun elf -> elf |> List.sum) |> List.sort
  sums |> should equal [4000L; 6000L; 10000L; 11000L; 24000L]

[<Fact>]
let B () =
  let input = lines 1
  let byElf = input |> groupCalories
  let sums = byElf |> Seq.map (fun elf -> elf |> Seq.sum) 
  sums |> Seq.sortDescending |> Seq.take 3 |> Seq.sum |> should equal 206582L
