module Tests.Day5

open System
open Xunit
open FsUnit.Xunit
open AOC2023.Common
open AOC2023.Day5

let sample = [
  "47|53"
  "97|13"
  "97|61"
  "97|47"
  "75|29"
  "61|13"
  "75|53"
  "29|13"
  "97|29"
  "53|29"
  "61|53"
  "97|53"
  "61|29"
  "47|13"
  "75|47"
  "97|75"
  "47|61"
  "75|61"
  "47|29"
  "75|13"
  "53|13"
  ""
  "75,47,61,53,29"
  "97,61,53,29,13"
  "75,29,13"
  "75,97,47,61,53"
  "61,13,29"
  "97,13,75,29,47"
]

[<Fact>]
let A_Sample () =
  let rules, updates = parse sample
  rules |> should haveLength 21
  updates |> should haveLength 6
  let validUpdates = updates |> List.filter (obeysRules rules)
  validUpdates |> List.map middle |> should equal [61; 53; 29]

[<Fact>]
let A () =
  let rules, updates = lines 5 |> parse
  updates
  |> List.filter (obeysRules rules)
  |> List.map middle
  |> List.sum
  |> should equal 5064

[<Fact>]
let B_Sample () =
  let rules, updates = parse sample
  rules |> should haveLength 21
  updates |> should haveLength 6
  let invalidUpdates = updates |> List.filter (fun x -> (obeysRules rules x) |> not)
  let reordered = invalidUpdates |> List.map (reorder 0 rules)
  reordered |> List.map middle |> should equal [47; 29; 47]

[<Fact>]
let B () =
  let rules, updates = lines 5 |> parse
  updates
  |> List.filter (fun x -> (obeysRules rules x) |> not)
  |> List.map (reorder 0 rules)
  |> List.map middle
  |> List.sum
  |> should equal 5064