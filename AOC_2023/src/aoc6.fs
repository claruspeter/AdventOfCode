module AOC2023.Day6

open System
open AOC2023.Common

type private Race = {
  time: int64; 
  distance: int64
}with 
  member this.WinningDistances =
    [1L..this.time]
    |> List.choose (fun speed -> 
        match speed * (this.time - speed) with 
        | d when d > this.distance -> Some d
        | _ -> None
    )

let private parseRaceLineWithSpaces (line:string) =
  line.Substring(10).Split([|' '|], StringSplitOptions.RemoveEmptyEntries) |> Array.toList

let private parseRaceLineWithoutSpaces (line:string) =
  line.Substring(10).Replace(" ", "") |> List.singleton

let private parseRaces lineParser (lines:string list) =
  lines
  |> List.map lineParser
  |>  function
      | [a;b] -> List.zip a b
      | x -> failwithf "Impossible! %A" x
  |> List.map (fun (a, b) -> {time=int64 a; distance=int64 b})
  |> List.map (fun race -> race.WinningDistances |> List.length )


let numberWaysToWin (lines:string list) =
  lines
  |> parseRaces parseRaceLineWithSpaces

let numberWaysToWinLong (lines:string list) =
  lines
  |> parseRaces parseRaceLineWithoutSpaces