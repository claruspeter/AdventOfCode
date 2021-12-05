module Tests.Day4

open System
open Xunit
open FsUnit.Xunit
open AOC2021.Common
open AOC2021.Day4

let sample = 
  [
    "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
    ""
    "22 13 17 11  0"
    " 8  2 23  4 24"
    "21  9 14 16  7"
    " 6 10  3 18  5"
    " 1 12 20 15 19"
    ""
    " 3 15  0  2 22"
    " 9 18 13 17  5"
    "19  8  7 25 23"
    "20 11 10 24  4"
    "14 21 16 12  6"
    ""
    "14 21 17 24  4"
    "10 16 15  9 19"
    "18  8 23 26 20"
    "22 11 13  6  5"
    " 2  0 12  3  7"
  ]

let uncalled (bingo: BingoResult option) = 
    bingo.Value.board 
    |> Seq.map (fun b -> 
      b
      |> filterBingo (fun cell -> not cell.isCalled)
      |> Array.sumBy (fun x -> x.value)
    )
    |> Seq.toArray

[<Fact>]
let A_Sample () =
  let data = sample |> Seq.toList
  let caller = data.[0].Split([|','|]) |> Array.map Int32.Parse
  let boards = data |> List.skip 1 |> parseBoards
  boards |> Seq.length |> should equal 3

  let bingo = callTillBingo boards caller
  bingo |> uncalled |> should equal [| 188 |]


// [<Fact>]
let A () =
  let data = lines 4 |> Seq.toList
  let caller = data.[0].Split([|','|]) |> Array.map Int32.Parse
  let boards = data |> List.skip 1 |> parseBoards
  boards |> Seq.length |> should equal 100

  let bingo = callTillBingo boards caller
  bingo |> uncalled |> should equal [| 1137 |]
  bingo |> uncalled |> Array.head |>  (*) bingo.Value.lastCalled |> should equal 5685

[<Fact>]
let B_Sample () =
  let data = sample |> Seq.toList
  let caller = data.[0].Split([|','|]) |> Array.map Int32.Parse
  let boards = data |> List.skip 1 |> parseBoards |> Seq.toList
  boards.Length |> should equal 3

  let bingo = callTillLastBingo boards caller
  bingo.Value.lastCalled |> should equal 13
  bingo |> uncalled |> should equal [| 148 |]

[<Fact>]
let B () =
  let data = lines 4 |> Seq.toList
  let caller = data.[0].Split([|','|]) |> Array.map Int32.Parse
  let boards = data |> List.skip 1 |> parseBoards |> Seq.toList
  boards.Length |> should equal 100

  let bingo = callTillLastBingo boards caller
  bingo |> uncalled |> should equal [| 430 |]
  bingo 
  |> uncalled 
  |> Array.head 
  |>  (*) bingo.Value.lastCalled 
  |> should equal 21070
