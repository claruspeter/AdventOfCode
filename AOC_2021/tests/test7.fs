module Tests.Day7

open System
open Xunit
open FsUnit.Xunit
open AOC2021.Common
open AOC2021.Day7

let sample = "16,1,2,0,4,2,7,1,2,14"

[<Fact>]
let A_Sample () =
  let data = sample.Split([|','|]) |> Array.map Int32.Parse |> Array.sort
  let center = cheapestSpot data
  movementCost center data |> should equal 37
  movementCost 1 data |> should equal 41
  movementCost 3 data |> should equal 39
  movementCost 10 data |> should equal 71

[<Fact>]
let A () =
  let data = (raw 7).Split([|','|]) |> Array.map Int32.Parse |> Array.sort
  let center = cheapestSpot data
  movementCost center data |> should equal 349357

[<Fact>]
let B_Sample () =
  let data = sample.Split([|','|]) |> Array.map Int32.Parse |> Array.sort
  increasingMovementCost 5 3 |> should equal (1 + 2)
  increasingMovementCost 5 4 |> should equal 1
  increasingMovementCost 5 5 |> should equal 0
  increasingMovementCost 5 6 |> should equal 1
  increasingMovementCost 5 7 |> should equal (1 + 2)
  increasingMovementCost 5 8 |> should equal (1 + 2 + 3)
  increasingMovementCost 5 9 |> should equal (1 + 2 + 3 + 4)

  calcIncreasingCost 5 data |> should equal 168
  calcIncreasingCost 2 data |> should equal 206

  findIncreasingCenter data |> should equal { center = 5; cost = 168 }

[<Fact>]
let B () =
  let data = (raw 7).Split([|','|]) |> Array.map Int32.Parse |> Array.sort
  findIncreasingCenter data |> should equal  { center = 480; cost = 96708205 }
