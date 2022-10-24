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

let private packetValue (packet:Packet) =
  match packet.body with 
  | Operator _ -> failwith "Not a value"
  | Value p -> p.total

[<Fact>]
let DecodeHex () =
  let raw = "D2FE28"
  let bin = hexToBin raw
  bin |> should equal "110100101111111000101000"


[<Fact>]
let SimpleNumber () =
  let raw = "D2FE28" |> decodeHex
  raw.version |> should equal 6
  raw.typeId |> should equal 4
  match raw.body with 
  | Value b ->
    b.values |> should equal ["0111"; "1110"; "0101"]
    b.total |> should equal 2021
  | _ -> failwith "Not a value packet"

[<Fact>]
let OperatorPacketWithBitsLength () =
  let raw = "38006F45291200" |> decodeHex
  raw.version |> should equal 1
  raw.typeId |> should equal 6
  match raw.body with 
  | Operator b ->
    b.length |> should equal (Bits 27)
    b.subPackets.Length |> should equal 2
    b.subPackets.[0] |> packetValue |> should equal 10
    b.subPackets.[1] |> packetValue |> should equal 20
  | _ -> failwith "Not an operator packet"

[<Fact>]
let OperatorPacketWithPacketsLength () =
  let raw = "EE00D40C823060" |> decodeHex
  raw.version |> should equal 7
  raw.typeId |> should equal 3
  match raw.body with 
  | Operator b ->
    b.length |> should equal (Packets 3)
    b.subPackets.Length |> should equal 3
    b.subPackets.[0] |> packetValue |> should equal 1
    b.subPackets.[1] |> packetValue |> should equal 2
    b.subPackets.[2] |> packetValue |> should equal 3
  | _ -> failwith "Not an operator packet"

[<Fact>]
let VersionTrace () =
  let raw = "8A004A801A8002F478" |> decodeHex
  raw |> traceVersions |> Seq.toList |> should equal [(false, 4); (false, 1); (false, 5); (true, 6)]

[<Theory>]
[<InlineData("8A004A801A8002F478", 16)>]
[<InlineData("620080001611562C8802118E34", 12)>]
[<InlineData("C0015000016115A2E0802F182340", 23)>]
[<InlineData("A0016C880162017C3686B18A3D4780", 31)>]
let VersionSum (hex, expectedSum) =
  hex 
  |> decodeHex 
  |> sumVersions 
  |> should equal expectedSum

[<Fact>]
let A () =
  raw 16
    |> decodeHex 
    |> sumVersions 
    |> should equal 908