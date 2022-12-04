module AOC2022.Day2

open System
open AOC2022.Common

type RPS = Rock | Paper | Scissors

type RpsRound = {
  them: RPS
  us: RPS
} 

let winsAgainst = function 
  | Rock -> Paper
  | Paper -> Scissors 
  | Scissors -> Rock

let losesAgainst = function 
  | Rock -> Scissors
  | Paper -> Rock 
  | Scissors -> Paper

let parseRpsRound (line:string) =
  {
    them =  match line.[0] with 
            | 'A' -> Rock
            | 'B' -> Paper
            | 'C' -> Scissors
            | _ -> failwithf "Not a valid rps for them: %A" line.[0]
    us =  match line.[2] with 
          | 'X' -> Rock
          | 'Y' -> Paper
          | 'Z' -> Scissors
          | _ -> failwithf "Not a valid rps for us: %A" line.[0]
  }
  
let parseRpsRoundStrategically (line:string) =
  let them =  
    match line.[0] with 
    | 'A' -> Rock
    | 'B' -> Paper
    | 'C' -> Scissors
    | _ -> failwithf "Not a valid rps for them: %A" line.[0]
  let us =  
    match line.[2] with 
    | 'X' -> them |> losesAgainst
    | 'Y' -> them 
    | 'Z' -> them |> winsAgainst 
    | _ -> failwithf "Not a valid rps for us: %A" line.[2]
  { them=them; us=us }


type RPS with 
  member this.value =
    match this with 
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3
  member this.beats other =
    match this, other with
    | a, b when a = b -> 3
    | a,b when a = winsAgainst b -> 6
    | _ -> 0

type RpsRound with 
  member this.score =
    this.us.value + (this.us.beats this.them)