module AOC2021.Day16

open System
open System.IO;
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

type ValuePacket = {
  values: string list
} with 
  member this.total = 
    this.values
    |> String.concat ""
    |> fun x -> Convert.ToInt32(x, 2)

type SubPacketLength = 
  | Bits of int
  | Packets of int

type OperatorPacket = {
  length: SubPacketLength
  subPackets: Packet list
}

and PacketBody =
  | Value of ValuePacket
  | Operator of OperatorPacket

and Packet = {
  version: int
  typeId: int
  body: PacketBody
}

let private take n (stream:Stream) =
  seq{
    for i in 0..(n-1) do
      (char) (stream.ReadByte())
  }

let hexToBin (hex:string) =
  hex
  |> Seq.map ( fun x -> hexbin.[x] )
  |> Seq.toArray
  |> fun x -> String.Join("", x)

let private yieldValues (binStream: Stream) =
  let mutable foundEnd = false
  seq{
    while not(foundEnd) do
      let chunk =
        binStream
        |> take 5
        |> Seq.toArray
        |> String
      yield chunk 
      foundEnd <- chunk |> Seq.head |> (=) '0'
  }

let private stripValue (value: String) = value.Substring(1)



let rec decode (bin: Stream) : Packet =
  let version = bin |> take 3 |> binToNum
  let typeId = bin |> take 3 |> binToNum
  match typeId with 
  | 4 -> 
    let values = bin |> yieldValues |> Seq.map stripValue |> Seq.toList
    {
      version = version
      typeId = typeId
      body = Value {values = values }
    }
  | _ -> 
    let i = bin |> take 1 |> Seq.head
    let length = 
      match i with 
      | '0' -> 
        bin 
        |> take 15 
        |> binToNum
        |> Bits
      | '1' -> 
        bin 
        |> take 11 
        |> binToNum
        |> Packets
      | _ -> failwithf "Unknown length mode: %A" i
    let subPackets : Packet list = 
      match length with 
      | Bits n -> 
          let range = bin |> take n |> Seq.map byte |> Seq.toArray
          use stream = new MemoryStream(range)
          seq{
            while stream.Position < stream.Length do
              yield (decode stream)
          } |> Seq.toList

      | Packets n ->
        [1..n]
        |> List.map (fun _ -> 
          decode bin
        )

    {
      version = version
      typeId = typeId
      body = Operator { length = length; subPackets = subPackets }
    }

let rec traceVersions (p: Packet) =
  seq{
    match p.body with 
    | Operator body ->
      yield (p.typeId = 4, p.version)
      for sub in body.subPackets do
        yield! (traceVersions sub)
    | _ -> 
      yield (p.typeId = 4, p.version)
  }

let sumVersions p = 
  p
  |> traceVersions
  |> Seq.sumBy snd

let decodeHex (hex: string) =
  let bin = hex |> hexToBin
  use stream = new MemoryStream(System.Text.ASCIIEncoding.ASCII.GetBytes(bin))
  decode stream