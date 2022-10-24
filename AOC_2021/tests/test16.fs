module Tests.Day16

open System
open Xunit
open FsUnit.Xunit
open AOC2021.Common
open AOC2021.Day16

let bin_sample =[|
  "110100101111111000101000"
  "00111000000000000110111101000101001010010001001000000000"
  "11101110000000001101010000001100100000100011000001100000"
|]

let hex_sample = [
  "8A004A801A8002F478"
  "620080001611562C8802118E34"
  "C0015000016115A2E0802F182340"
  "A0016C880162017C3686B18A3D4780"
]

[<Fact>]
let DecodeHex () =
  let raw = "D2FE28"
  let bin = hexToBin raw
  bin |> should equal "110100101111111000101000"


[<Fact>]
let SimpleNumber () =
  let raw = "D2FE28" |> decode
  raw |> should equal {version = 6; typeId = 4; values = ["0111"; "1110"; "0101"] }
  raw.total |> should equal 2021

