module AOC2021.Day10

open System
open AOC2021.Common

type Corruption = {
  expected: char
  found: char
}
with 
  member this.value = 
    match this.found with 
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> failwith "unknown"

type Chunk = {
  value: char
  child: Chunk[]
}

type LineCheck = Result<Chunk[], Corruption>

let symbols = "([{<)]}>"

let closedBy closing opening =
  match  opening, closing with
  | '(', ')' -> true
  | '[', ']' -> true
  | '{', '}' -> true
  | '<', '>' -> true
  | _ -> false

let space = "........................................"

let rec private parseChunks' (depth: int) (parent: char) (line:string) =
  if line = "" then 
    {expected=parent; found=' '} |> Error
  else
    match line.[0] |> symbols.IndexOf with 
    | n when n < 4 -> 
      //opening
      printfn "%s %c" (space.Substring(0,depth)) line.[0]
      parseChunks' (depth+1) line.[0] (line.Substring 1)
    | n -> 
      //closing
      printfn "%s %c" (space.Substring(0,depth-1)) line.[0]
      if parent |> closedBy line.[0]  then 
        [| {value=parent; child=[||]} |] |> Ok
      else 
        {expected=parent; found=line.[0] } |> Error

let parseChunks (line:string) = parseChunks' 0 (line.[0]) (line.Substring(1))

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