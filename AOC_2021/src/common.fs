module AOC2021.Common

open System 
open System.IO
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Result
open System.Collections.Generic

let parseInts x = 
    try
        Some (Int32.Parse x)
    with
    | _ -> None

let raw n = File.ReadAllText(sprintf "../../../../data/aoc%d_input.txt" n)

let lines n = File.ReadAllLines(sprintf "../../../../data/aoc%d_input.txt" n)

type Result<'A, 'B> with
  member this.ResultValue =
    match this with
    | Ok x -> x
    | _ -> failwith "Not OK"

let pretty result =
    match result with
    | Ok _ -> "OK"
    | Error s -> sprintf "ERROR: %s" s

let (>=>) f g x =
  match f x with 
  | Ok y -> g y
  | Error s -> Error s

let (>==>) f g x =
  match f x with 
  | Some y -> Some y
  | None -> g x

let isError =
  function
  | Ok _ -> false
  | Error _ -> true

let log x =
  printfn "%A" x
  x

let logf (transform: 'a -> 'b) x =
  x |> transform |> printfn "%A"
  x

let printseq seq = 
  seq 
  |> Seq.map (fun x -> x.ToString())
  |> Seq.toArray 
  |> Array.sort 
  |> fun x -> String.Join(",", x)

let logseq seq =
  seq
  |> Seq.map log
  
let logseqf (transform: 'a -> 'b) seq =
  seq
  |> Seq.map transform
  |> Seq.iter (log >> ignore)
  seq

let filterDoubleArray<'T> (predicate: 'T -> bool ) (board: 'T array array) =
  board
  |> Array.collect (Array.filter predicate)

let (|Regex|_|) pattern input =
    if input = null then None
    else
        let m = Regex.Match(input, pattern, RegexOptions.Compiled)
        if m.Success then Some [for x in m.Groups -> x.Value]
        else None

let charInt (c:char) = int c - int '0'

type IDictionary<'A, 'B> with 
  member this.OrDefault (defaultValue: 'B) (key: 'A) =
    if this.ContainsKey key then 
      this.[key]
    else
      defaultValue