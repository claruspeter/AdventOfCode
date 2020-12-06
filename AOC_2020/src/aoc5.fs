module AOC5

open System
open System.Numerics
open AOC_Common


let inputs = 
    lines 5

let n = inputs.Length

type Seat = {
  row:int
  column: int
} with
  member this.id = 
    this.row * 8 + this.column

let rec departition (fb:string) =
  match fb with 
  | "" -> 0
  | x -> 
    let inc = x.Substring(1) |> departition
    let nRows = BigInteger.Pow(2 |> BigInteger, fb.Length) |> int
    let thisPartition =
      match fb.[0] with 
      | 'F' -> 0
      | 'L' -> 0
      | 'B' -> nRows/2
      | 'R' -> nRows/2
      | _ -> failwith "Not valid"
    thisPartition + inc

let parseSeat (line:string) =
  {
    row = line.Substring(0,7) |> departition
    column = line.Substring 7 |> departition
  }