module Tests.Day15

open System
open Xunit
open FsUnit.Xunit
open AOC2021.Common
open AOC2021.Grids
open AOC2021.Day15

let sample = [|
  "1163751742"
  "1381373672"
  "2136511328"
  "3694931569"
  "7463417111"
  "1319128137"
  "1359912421"
  "3125421639"
  "1293138521"
  "2311944581"
|]

[<Fact>]
let A_Sample () =
  let result = 
    sample
    |> parseGrid (fun p v -> v)
    |> euler
  // result 
  //   |> printHeightMap (fun a -> sprintf "%02d " a)
  //   |> printfn "%s"
  result.get (0,0) |> should equal 40

[<Fact>]
let A () =
  let result = 
    lines 15
    |> parseGrid (fun p v -> v)
    |> euler
  result 
    |> printHeightMap (fun a -> sprintf "%03d " a)
    |> printfn "%s"
  result.get (0,0) |> should equal 699



