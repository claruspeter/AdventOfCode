module AOC2023.Day6

open System
open AOC2023.Common

type private Race = {
  time: int; 
  distance: int
}with 
  member this.PossibleDistances =
    [1..this.time]
    |> List.map (fun speed -> speed * (this.time - speed))

let private parseRaces (lines:string list) =
  lines
  |> List.map (fun line -> line.Substring(10).Split([|' '|], StringSplitOptions.RemoveEmptyEntries) |> Array.toList)
  |>  function
      | [a;b] -> List.zip a b
      | x -> failwithf "Impossible! %A" x
  |> List.map (fun (a, b) -> {time=int a; distance=int b})
  |> List.map (fun race -> race.PossibleDistances |> List.filter (fun d -> d > race.distance) |> List.length )
  

let numberWaysToWin (lines:string list) =
  lines
  |> parseRaces
