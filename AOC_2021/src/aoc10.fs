module AOC2021.Day10

open System
open AOC2021.Common

let private findIllegal' (openers: char list) (current:char) =
  let head = if openers = [] then ' ' else openers.[0]
  match head, current with 
  | _, '('
  | _, '{' 
  | _, '[' 
  | _, '<' -> [current] @ openers
  | '(', ')' -> openers |> List.tail
  | '[', ']' -> openers |> List.tail
  | '{', '}' -> openers |> List.tail
  | '<', '>' -> openers |> List.tail
  | _ -> failwithf "%c" current

let findIllegal (s:string) =
  try
    s
    |> Seq.fold findIllegal' []
    |> Ok
  with
  | exc -> 
    match exc.Message with
    | ")" -> 3
    | "]" -> 57
    | "}" -> 1197
    | ">" -> 25137
    | _ -> 0
    |> Error

let private _scores = [' '; '('; '['; '{'; '<']

let scoreClosers (openers: char list) =
  openers
  |> Seq.map (fun c -> _scores |> List.findIndex ((=) c) |> int64 )
  |> Seq.fold (fun acc i -> acc * 5L + i ) 0L
