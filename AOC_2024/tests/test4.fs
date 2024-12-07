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
    |> indexStarters 'X'
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
    |> indexStarters 'X'
    |> makeWords board
    |> allXmas
  found.Length |> should equal 2654


[<Fact>]
let B_Sample () =
  let board = sample |> toBoard
  board
    |> indexStarters 'A'
    |> makeDiagonalWords board
    |> allCrossedMasses
    |> should haveLength 9

[<Fact>]
let B () =
  let board = lines 4 |> toBoard
  let found =
    board
    |> indexStarters 'A'
    |> makeDiagonalWords board
    |> allCrossedMasses
  found.Length|> should equal 1990