module Tests.Day10

open System
open Xunit
open FsUnit.Xunit
open AOC2023.Common
open AOC2023.Day10

let sample = [
  "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
  "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
  "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
]

[<Fact>]
let A_Sample () =
  let result = 
    sample
    |> parse
  result.Length |> should equal 3
  result.[0].lights |> should equal [ false; true; true; false]
  result.[1].buttons |> should equal [ [0;2;3;4]; [2;3]; [0;4]; [0;1;2]; [1;2;3;4] ]
  result.[2].joltage |> should equal [10; 11; 11; 5; 10; 5]

  let leastButtons = 
    result
    |> List.map (findLeastButtons >> Seq.head >> Seq.length)
  leastButtons |> should equal [2; 3; 2]
  leastButtons |> List.sum |> should equal 7

[<Fact>]
let A () =
  let result = 
    lines 10
    |> parse
  let leastButtons = 
    result
    |> List.map (findLeastButtons >> Seq.head >> Seq.length)
  leastButtons |> List.sum |> should equal 434




