module AOC_Common

open System 
open System.IO
open FSharp.Scanf.Scanf
open Microsoft.FSharp.Core.Result



let parseInts x = 
    try
        Some (Int32.Parse x)
    with
    | _ -> None

let raw n = File.ReadAllText(sprintf "../../../../data/aoc%d_input.txt" n)

let lines n = File.ReadAllLines(sprintf "../../../../data/aoc%d_input.txt" n)

let toMultiLineBlocks (s:string) =
  let spaced = s.Replace("\n", " ");
  spaced.Split([|"  "|], StringSplitOptions.RemoveEmptyEntries)


let preProcess (block:string) =
  block.Split('\n', StringSplitOptions.TrimEntries) 
  |> Array.toList
        
[<Measure>] type cm  
[<Measure>] type inch
[<Measure>] type unknown

let cmPerInch : float<cm/inch> = 2.54<cm/inch>

type Distance =
  | InCms of int<cm>
  | InInches of int<inch>
  | Unknown of int<unknown>
  with
    static member Parse s =
      try
        sscanf "%d%s" s
        |>  function
            | x, "cm" -> InCms (x * 1<cm>)
            | x, "in" -> InInches (x * 1<inch>)
            | x, y -> Unknown (x * 1<unknown>)
      with
      | _ ->
        let x = Int32.Parse(s)
        Unknown (x * 1<unknown>)

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