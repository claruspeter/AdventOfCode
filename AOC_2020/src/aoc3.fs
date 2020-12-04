module AOC3

open System
open AOC_Common

type Pos = {x: int; y: int}

type PosResponse =
  | Tree of Pos
  | Space of Pos
  | Done

let inputs = 
    raw 3
    |> fun s -> s.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)

let isTree pos =
  if pos.y >= inputs.Length then 
    Done
  else
    match inputs.[pos.y].[pos.x % 31] with
    | '#' -> Tree pos
    | '.' -> Space pos
    | _ -> failwith "???"

let private getPos x =
  match x with 
  | Tree p -> Some p
  | Space p -> Some p
  | _ -> None

let move left down from =
  match getPos from with 
  | None -> Done
  | Some p -> 
    let pos = {x = p.x + left; y = p.y + down}
    isTree pos

let rec traverse left down from =
  match getPos from with 
  | None -> [Done]
  | Some p -> 
    from 
      |> move left down 
      |> traverse left down
      |> (@) [from]

let nTrees chain =
  chain 
  |> List.filter (function
    | Tree _ -> true
    | _ -> false
  )
  |> List.length