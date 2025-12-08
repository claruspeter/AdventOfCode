module Tests.Day6

open System
open Xunit
open FsUnit.Xunit
open AOC2023.Common
open AOC2023.Day6

let sample = [
  "123 328  51 64 "
  " 45 64  387 23 "
  "  6 98  215 314"
  "*   +   *   +  "
]

[<Fact>]
let A_Sample () =
  let result = 
    sample
    |> parse
    |> List.map calculate
  result |> should equal [33210L ; 490L ; 4243455L ; 401L ]
  result
    |> List.sum
    |> should equal 4277556L

[<Fact>]
let A () =
  let result = 
    lines 6
    |> parse
    |> List.map calculate
  result
    |> List.sum
    |> should equal 3785892992137L

[<Fact>]
let B_Sample () =
  let result = 
    sample
    |> parseVertical
    |> List.map calculate
  result |> should equal [1058L ; 3253600L ; 625L ; 8544L ]
  result
    |> List.sum
    |> should equal 3263827L

[<Fact>]
let B () =
  let result = 
    lines 6
    |> parseVertical
    |> List.map calculate
  result
    |> List.sum
    |> should equal 7669802156452L