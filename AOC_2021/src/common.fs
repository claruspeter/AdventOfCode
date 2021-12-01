module AOC2021.Common

open System 
open System.IO
open Microsoft.FSharp.Core.Result

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

let logseq seq =
  seq
  |> Seq.iter (fun x -> printfn "%A" x)
  seq