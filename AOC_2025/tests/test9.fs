module Tests.Day9

open System
open Xunit
open FsUnit.Xunit
open AOC2023.Common
open AOC2023.Day9

let sample = [
  "7,1"
  "11,1"
  "11,7"
  "9,7"
  "9,5"
  "2,5"
  "2,3"
  "7,3"
]

[<Fact>]
let A_Sample () =
  let result = 
    sample
    |> parse
  // result |> printGrid |> log |> ignore
  result |> should haveLength 8
  let found = result |> findLargestRectangle
  found |> should equal { a = {x=2; y=5}; b = {x=11; y=1} }
  found.area |> should equal 50L

[<Fact>]
let A () =
  let result = 
    lines 9
    |> parse
  result |> should haveLength 496
  let found = result |> findLargestRectangle
  found.area |> should equal 4782151432L //too low
