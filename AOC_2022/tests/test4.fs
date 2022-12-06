module Tests.Day4

open System
open Xunit
open FsUnit.Xunit
open AOC2022.Common
open AOC2022.Day4

let sample = [
  "2-4,6-8"
  "2-3,4-5"
  "5-7,7-9"
  "2-8,3-7"
  "6-6,4-6"
  "2-6,4-8"
]

[<Fact>]
let A_Sample () =
  let assignmentPairs = sample |> List.map parseAssignmentPair
  assignmentPairs |> Seq.length |> should equal 6
  assignmentPairs |> Seq.filter fullyContained |> Seq.length |> should equal 2

[<Fact>]
let A () =
  lines 4 
  |> List.map parseAssignmentPair
  |> Seq.filter fullyContained 
  |> Seq.length 
  |> should equal 498

[<Fact>]
let B_Sample () =
  let assignmentPairs = sample |> List.map parseAssignmentPair
  assignmentPairs |> Seq.length |> should equal 6
  assignmentPairs |> Seq.filter overlaps |> Seq.length |> should equal 4

[<Fact>]
let B () =
  lines 4 
  |> List.map parseAssignmentPair
  |> Seq.filter overlaps 
  |> Seq.length 
  |> should equal 859
