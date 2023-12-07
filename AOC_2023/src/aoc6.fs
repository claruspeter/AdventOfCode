module AOC2023.Day6

open System
open AOC2023.Common

type private Race = {
  time: int64; 
  distance: int64
}with 
  member this.WinningDistanceCount =
    // The calculation is symmetric.  So the number of "losers" is equal at the top and bottom ends
    seq [1L..this.time] 
    |> Seq.takeWhile (fun speed -> speed * (this.time - speed) <= this.distance)
    |> Seq.length
    |> int64
    |> fun nLowLosers -> this.time - (nLowLosers * 2L) - 1L
    |> int

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


let numberWaysToWin (lines:string list) =
  lines
  |> parseRaces parseRaceLineWithSpaces
  |> List.map (fun race -> race.WinningDistanceCount )

let numberWaysToWinLong (lines:string list) =
  lines
  |> parseRaces parseRaceLineWithoutSpaces
  |> List.map (fun race -> race.WinningDistanceCount )
