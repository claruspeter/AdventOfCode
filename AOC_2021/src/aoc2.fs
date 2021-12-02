module AOC2021.Day2

open System
open AOC2021.Common


type Direction = 
  | Forward of int
  | Backward  of int
  | Up of int
  | Down of int

type Position = { x: int; depth: int;}
type PositionAndAim = { posn: Position; aim: int; }

let parseDirection (line:string) = 
  let parts = line.Split([|' '|])
  match parts.[0], Int32.Parse(parts.[1]) with 
  | "forward", n -> Forward n |> Some
  | "backward", n -> Backward n |> Some
  | "up", n -> Up n |> Some
  | "down", n -> Down n |> Some
  | _ -> None

let moveInDirection (initialPos: Position) dirn =
  match dirn with
  | Forward n -> {initialPos with x=initialPos.x + n}
  | Backward n -> {initialPos with x=initialPos.x - n}
  | Up n -> {initialPos with depth=initialPos.depth - n}
  | Down n -> {initialPos with depth=initialPos.depth + n}

let moveInAimedDirection (initialPos: PositionAndAim) dirn =
  match dirn with
  | Forward n -> 
    {initialPos with 
      posn={
        x=initialPos.posn.x + n; 
        depth=initialPos.posn.depth + n * initialPos.aim 
      }
    }
  | Backward n -> 
    {initialPos with 
      posn = {
        x=initialPos.posn.x - n;  
        depth=initialPos.posn.depth - n * initialPos.aim
      }
    }
  | Up n -> {initialPos with aim=initialPos.aim - n}
  | Down n -> {initialPos with aim=initialPos.aim + n}