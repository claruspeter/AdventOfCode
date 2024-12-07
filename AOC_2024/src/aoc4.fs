module AOC2023.Day4

open System
open AOC2023.Common

type Board = list<list<char>>

let toBoard (lines: string list) =
  lines
  |> List.map Seq.toList

type Starter = {
  x: int;
  y: int;
  c: char
}

let indexStarters startChar (board: Board) =
  board
  |> List.mapi (fun y row -> 
    row
    |> List.mapi (fun x c ->
      {x=x; y=y;c =c}
    )
  )
  |> List.collect id
  |> List.filter (fun x -> x.c = startChar)

type FoundWord = {
  word: string
  dx: int
  dy: int
}

type WithWords = {
  starter: Starter
  words: FoundWord list
}

let getLetter (board: Board) x y =
  if x < 0 || y < 0 || y >= board.Length || x >= board.[0].Length then 
    '.'
  else
    board.[y].[x]

let findWord board x y dx dy =
  [|1..3|] 
  |> Array.map (fun i -> getLetter board (x + i * dx) (y + i * dy) ) 
  |> String
  |> fun w -> {word = w; dx=dx; dy=dy}


let findCrossWord board x y dx dy =
  [|-1..1|] 
  |> Array.map (fun i -> getLetter board (x + i * dx) (y + i * dy) ) 
  |> String
  |> fun w -> {word = w; dx=dx; dy=dy}

let makeWords (board: Board) (starters: Starter list) = 
  starters
  |> List.map (fun s -> 
    let finder = findWord board s.x s.y
    {
      starter=s
      words = [
        finder 0 1
        finder 0 -1
        finder 1 -1
        finder 1 0
        finder 1 1
        finder -1 -1
        finder -1 0
        finder -1 1
      ]
    }
  )

let makeDiagonalWords (board: Board) (starters: Starter list) = 
  starters
  |> List.map (fun s -> 
    let finder = findCrossWord board s.x s.y
    {
      starter=s
      words = [
        finder 1 -1
        finder 1 1
        finder -1 -1
        finder -1 1
      ]
    }
  )

let allXmas (words:WithWords list) =
  words
  |> List.collect (fun x -> x.words |> List.map (fun w -> (x.starter, w)))
  |> List.filter (fun (s,w) -> w.word = "MAS")

let allCrossedMasses (words:WithWords list) =
  words
  |> List.map ( fun x -> {x with words=x.words |> List.filter (fun w -> w.word = "MAS")})
  |> List.filter (fun x -> x.words.Length = 2)

