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

let scrubberRating discriminator (data:string seq) = 
  let strlen = data.First().Length
  [0..strlen]
  |> Seq.fold (fun acc i -> matchingCommonBitAt discriminator i acc) data
  |> Seq.head
  |> toDec

[<Fact>]
let B_Sample () =
  let data = 
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

  data 
  |> scrubberRating mostCommonBit
  |> should equal 23

  data 
  |> scrubberRating leastCommonBit
  |> should equal 10


[<Fact>]
let B () =
  let data = lines 3
  let oxygen = data |> scrubberRating mostCommonBit
  let co2 = data |> scrubberRating leastCommonBit
  co2 * oxygen |> should equal 4856080