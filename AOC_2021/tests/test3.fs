module Tests.Day3

open System
open System.Linq
open Xunit
open FsUnit.Xunit
open AOC2021.Common
open AOC2021.Day3

[<Fact>]
let A_Sample () =
  let transposed = 
    [
      "00100"
      "11110"
      "10110"
      "10111"
      "10101"
      "01111"
      "00111"
      "11100"
      "10000"
      "11001"
      "00010"
      "01010"
    ]
    |> transposeBits

  let gamma = 
    transposed
    |> Seq.map mostCommonBit
    |> toDec

  let epsilon = 
    transposed
    |> Seq.map leastCommonBit
    |> toDec
  gamma * epsilon |> should equal 198

[<Fact>]
let A () =
  let transposed = 
    lines 3
    |> transposeBits

  let gamma = 
    transposed
    |> Seq.map mostCommonBit
    |> toDec

  let epsilon = 
    transposed
    |> Seq.map leastCommonBit
    |> toDec
  gamma * epsilon |> should equal 3242606







