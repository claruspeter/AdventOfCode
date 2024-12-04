module Tests.Day2

open System
open Xunit
open FsUnit.Xunit
open AOC2023.Common
open AOC2023.Day2

let sample = [
  "7 6 4 2 1"
  "1 2 7 8 9"
  "9 7 6 2 1"
  "1 3 2 4 5"
  "8 6 4 4 1"
  "1 3 6 7 9"
]

[<Fact>]
let A_Sample () =
  let safe = 
    sample
    |> safety
  safe 
    |> List.map ( fun x -> x.safe) 
    |> should equal [true; false; false;false;false; true;]
  
[<Fact>]
let A () =
  let safe = 
    lines 2
    |> safety
  safe 
    |> List.filter (fun x -> x.safe) 
    |> List.length
    |> should equal 490



