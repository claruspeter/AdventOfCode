module Tests.Day4

open System
open Xunit
open FsUnit.Xunit
open AOC2023.Common
open AOC2023.Day4

let sample =[
  "MMMSXXMASM"
  "MSAMXMSMSA"
  "AMXSXMAAMM"
  "MSAMASMSMX"
  "XMASAMXAMM"
  "XXAMMXXAMA"
  "SMSMSASXSS"
  "SAXAMASAAA"
  "MAMMMXMMMM"
  "MXMXAXMASX"
]

[<Fact>]
let A_Sample () =
  let board = sample |> toBoard
  board
    |> indexStarters 
    |> makeWords board
    |> allXmas
    |> should haveLength 18

[<Fact>]
let A () =
  let board = 
    lines 4
    |> toBoard
  let found =
    board
    |> indexStarters 
    |> makeWords board
    |> allXmas
  found.Length |> should equal 2654


