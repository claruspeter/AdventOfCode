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
let A_Sample () =
  let data = sample |> parseCratesAndInstructions
  data.stacks |> Seq.sortBy (fun x -> x.Key) |> Seq.iter displayIndexedStack
  data.instructions |> Seq.length |> should equal 4
  data.stacks |> Seq.length |> should equal 3
  data.stacks.[1].Peek() |> should equal 'N'




