module Tests.Day9

open System
open Xunit
open FsUnit.Xunit
open AOC2021.Common
open AOC2021.Day9

let sample = [|
  "2199943210"
  "3987894921"
  "9856789892"
  "8767896789"
  "9899965678"
|]

[<Fact>]
let A_Sample () =
  let result = 
    sample
    |> parseHeightmap
    |> findLowPoints
    |> List.map (fun x -> x.value)
  result |> should equal [1; 0; 5; 5]
  result |> riskLevel |> should equal 15

// [<Fact>]
let A () =
  lines 9
  |> parseHeightmap
  |> findLowPoints
  |> List.map (fun x -> x.value)
  |> riskLevel
  |> should equal 496


[<Fact>]
let B_Sample () =
  let map = 
    sample
    |> parseHeightmap
  let result =
    map
    |> findLowPoints
    |> infectAdjacent map
  result.values 
  |> Array.filter (fun a -> (a |> snd).basin.IsSome)
  |> Array.countBy (fun a -> (a |> snd).basin )
  |> Array.map snd
  |> Array.sortDescending
  |> Array.take 3
  |> Array.reduce (*)
  |> should equal 1134

// [<Fact>]  // really quite slow!
let B () =
  let map = 
    lines 9
    |> parseHeightmap
  let result =
    map
    |> findLowPoints
    |> infectAdjacent map
  result.values 
  |> Array.filter (fun a -> (a |> snd).basin.IsSome)
  |> Array.countBy (fun a -> (a |> snd).basin )
  |> Array.map snd
  |> Array.sortDescending
  |> Array.take 3
  |> Array.reduce (*)
  |> should equal 902880