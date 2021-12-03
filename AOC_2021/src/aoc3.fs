module AOC2021.Day3

open System
open AOC2021.Common

let transposeBits (input: string seq) =
  input
  |> Seq.collect Seq.indexed
  |> Seq.groupBy fst
  |> Seq.map ( snd >> Seq.map snd )
  |> Seq.map ( Seq.toArray >> String )

let mostCommonBit (s:string) =
  match s |> Seq.filter (fun x -> x = '0') |> Seq.length with 
  | n when n > (s.Length / 2) -> '0'
  | _ -> '1'

let leastCommonBit (s:string) =
  match s |> Seq.filter (fun x -> x = '0') |> Seq.length with 
  | n when n > (s.Length / 2) -> '1'
  | _ -> '0'

let toDec (s: char seq) =
  s
  |> Seq.rev
  |> Seq.indexed
  |> Seq.fold (
      fun acc (i, c) ->
        acc + (
          if c = '0' then 0 else (System.Math.Pow(2, i) |> int)
        )
      ) 0