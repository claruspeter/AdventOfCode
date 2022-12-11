module Tests.Day5

open System
open Xunit
open FsUnit.Xunit
open AOC2022.Common
open AOC2022.Day5

let sample = [
  "    [D]    "
  "[N] [C]    "
  "[Z] [M] [P]"
  " 1   2   3 "
  ""
  "move 1 from 2 to 1"
  "move 3 from 1 to 3"
  "move 2 from 2 to 1"
  "move 1 from 1 to 2"
]

[<Fact>]
let A_Sample_parsing () =
  let {stacks=stacks; instructions=instructions} = sample |> parseCratesAndInstructions
  instructions |> Seq.length |> should equal 4
  stacks |> Seq.length |> should equal 3
  //stacks |> Seq.sortBy (fun x -> x.Key) |> Seq.iter displayIndexedStack
  stacks.[1].Peek() |> should equal 'N'

[<Fact>]
let A_Sample_moving () =
  let {stacks=stacks; instructions=instructions} = sample |> parseCratesAndInstructions
  stacks[1] |> Seq.toList |> should equal ['N'; 'Z']
  stacks[2] |> Seq.toList |> should equal ['D'; 'C'; 'M']
  stacks[3] |> Seq.toList |> should equal ['P']
  let a = move stacks (instructions.[0])
  a[1] |> Seq.toList |> should equal ['D'; 'N'; 'Z']
  a[2] |> Seq.toList |> should equal ['C'; 'M']
  a[3] |> Seq.toList |> should equal ['P']
  let b = move a (instructions.[1])
  b[1] |> should be Empty
  b[2] |> Seq.toList |> should equal ['C'; 'M']
  b[3] |> Seq.toList |> should equal ['Z'; 'N'; 'D'; 'P']

[<Fact>]
let A_Sample_after_moved () =
  let {stacks=stacks; instructions=instructions} = sample |> parseCratesAndInstructions
  let moved = instructions |> Seq.fold move stacks
  moved[1] |> Seq.toList |> should equal ['C']
  moved[2] |> Seq.toList |> should equal ['M']
  moved[3] |> Seq.toList |> should equal ['Z'; 'N'; 'D'; 'P']
  moved |> topCrates |> should equal "CMZ"

[<Fact>]
let A () =
  let {stacks=stacks; instructions=instructions} = lines 5 |> parseCratesAndInstructions
  let moved = instructions |> Seq.fold move stacks
  moved |> topCrates |> should equal "CVCWCRTVQ"
