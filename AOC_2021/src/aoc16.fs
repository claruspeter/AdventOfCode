module AOC2021.Day16

open System
open AOC2021.Common

let private hexbin = 
  [
    ('0', "0000")
    ('1', "0001")
    ('2', "0010")
    ('3', "0011")
    ('4', "0100")
    ('5', "0101")
    ('6', "0110")
    ('7', "0111")
    ('8', "1000")
    ('9', "1001")
    ('A', "1010")
    ('B', "1011")
    ('C', "1100")
    ('D', "1101")
    ('E', "1110")
    ('F', "1111")
  ] |> dict

let private binToNum (s: seq<char>) = Convert.ToInt32(String(s |> Seq.toArray), 2)

type Packet = {
  version: int
  typeId: int
  values: string list
} with 
  member this.total = 
    this.values
    |> String.concat ""
    |> fun x -> Convert.ToInt32(x, 2)

let hexToBin (hex:string) =
  hex
  |> Seq.map ( fun x -> hexbin.[x] )
  |> Seq.toArray
  |> fun x -> String.Join("", x)

let yieldPackets (binStream: seq<char>) =
  seq{
    let chunks =
      binStream
      |> Seq.chunkBySize 5
    yield! chunks |> Seq.takeWhile (fun x -> x.[0] = '1')
    yield! chunks |> Seq.skipWhile (fun x -> x.[0] = '1') |> Seq.take 1
  }

let stripPacket (packet: char[]) =
  packet
  |> Array.tail
  |> String



let decode (hex:string) : Packet=
  let bin = hex |> hexToBin
  {
    version = bin |> Seq.take 3 |> binToNum
    typeId = bin.Substring(3,3) |> binToNum
    values = bin |> Seq.skip 6 |> yieldPackets |> Seq.map stripPacket |> Seq.toList
  }
