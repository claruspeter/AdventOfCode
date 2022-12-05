module Tests.Day3

open System
open Xunit
open FsUnit.Xunit
open AOC2022.Common
open AOC2022.Day3

let sample = [
  "vJrwpWtwJgWrhcsFMMfFFhFp"
  "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
  "PmmdzqPrVvPwwTWBwg"
  "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
  "ttgJtRGJQctTZtZT"
  "CrZsJsPPZsGzwwsLwLmpwMDw"
]

[<Fact>]
let A_Sample () =
  let inBoth = sample |> List.map inBothCompartments
  inBoth |> should equal ['p'; 'L'; 'P'; 'v'; 't'; 's']
  inBoth |> List.map packItemPriority |> List.sum |> should equal 157

[<Fact>]
let A () =
  lines 3
    |> List.map inBothCompartments
    |> List.map packItemPriority 
    |> List.sum 
    |> should equal 7793

[<Fact>]
let B_Sample () =
  let inBoth = sample |> inElfGroups |> List.map groupBadge
  inBoth |> should equal ['r'; 'Z']
  inBoth |> List.map packItemPriority |> List.sum |> should equal 70

[<Fact>]
let B () =
  lines 3 
    |> inElfGroups 
    |> List.map groupBadge
    |> List.map packItemPriority 
    |> List.sum 
    |> should equal 2499