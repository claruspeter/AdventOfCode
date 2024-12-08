module AOC2023.Common

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

let parseIntList (line: string) =
  line.Split([|' '|], StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
  |> Array.map (Int32.Parse)
  |> Array.toList

let raw n = File.ReadAllText(sprintf "../../../../data/aoc%d_input.txt" n)

let lines n = File.ReadAllLines(sprintf "../../../../data/aoc%d_input.txt" n) |> Array.toList

type Result<'A, 'B> with
  member this.ResultValue =
    match this with
    | Ok x -> x
    | _ -> failwith "Not OK"

let pretty result =
    match result with
    | Ok _ -> "OK"
    | Error s -> sprintf "ERROR: %s" s

let swap (a:int) (b:int) (items: 'a list) = 
  let aVal = items.[a]
  let bVal = items.[b]
  items |> List.mapi (fun i x -> if i=a then bVal else if i=b then aVal else x )

let swapByValue (a:'a) (b:'a) (items: 'a list) =
  match items |> List.tryFindIndex (fun x -> x=a), items |> List.tryFindIndex (fun x -> x=b) with 
  | Some aIdx, Some bIdx -> items |> swap aIdx bIdx
  | _, _ -> items
  

/// <summary>Kliesi operator with Result</summary>
let (>=>) f g x =
  match f x with 
  | Ok y -> g y
  | Error s -> Error s

/// <summary>Kliesi operator with Option</summary>
let (>==>) f g x =
  match f x with 
  | Some y -> Some y
  | None -> g x

let choose (pipe: ('a -> 'b option) list) (a: 'a): 'b option =
  pipe
  |> List.skipWhile ( fun p -> p a |> Option.isNone)
  |>  function
      | [] -> None
      | head::tail -> head a

let isError =
  function
  | Ok _ -> false
  | Error _ -> true

let log x =
  printfn "%A" x
  x

let logm msg x =
  printfn "%s: %A" msg x
  x

let logm2File fn msg x =
  sprintf "%s: %A\r\n" msg x
  |> fun txt -> File.AppendAllText(fn, txt)
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

let Join (separator: string) (parts: 'a seq) = 
  String.Join(separator, parts)

let (|Regex|_|) pattern input =
    if input = null then None
    else
        Regex.Matches(input, pattern, RegexOptions.Compiled)
        |> Seq.collect (fun m -> [for x in m.Groups -> x.Value] )
        |> Seq.toList
        |> Some

let (|RegexMatch|_|) pattern input =
    if input = null then None
    else
        Regex.Matches(input, pattern, RegexOptions.Compiled)
        |> Seq.map (fun m -> m.Value )
        |> Seq.toList
        |> Some

let (|Index|_|) (a:'a) (items: 'a list)  =
  items |> List.tryFindIndex (fun x -> x=a) 

let charInt (c:char) = int c - int '0'

let CharInt c =
  match charInt c with 
  | x when x >=0 && x <=9 -> Some x
  | _ -> None

let (|CharInt|_|) c = CharInt c

type IDictionary<'A, 'B> with 
  member this.OrDefault (defaultValue: 'B) (key: 'A) =
    if this.ContainsKey key then 
      this.[key]
    else
      defaultValue