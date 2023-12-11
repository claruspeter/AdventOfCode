module AOC2023.Day9

open System
open AOC2023.Common

let private parseIntegers (line: string) = 
  line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
  |> Array.map int
  |> Array.toList

let rec private generateDiffs (numbers: int list) =
  let diffs =
    numbers 
    |> List.pairwise
    |> List.map (fun (a,b) -> b - a)
  match diffs |> List.forall (fun x -> x=0) with 
  | true -> [diffs]
  | false -> (generateDiffs diffs) @[diffs]

let private predictNextInLine numbers =
  let diffs = (generateDiffs numbers) @ [numbers]
  //printfn "Predicting from %A" diffs
  diffs 
  |> List.fold ( fun acc i -> i |> List.last |> (+) acc ) 0

let private predictPrevInLine numbers =
  let diffs = (generateDiffs numbers) @ [numbers]
  diffs 
  |> List.fold ( fun acc i -> i |> List.head |> fun x -> x - acc ) 0

let predictNext (lines: string list) =
  lines
  |> List.map parseIntegers
  |> List.map predictNextInLine

let predictPrev (lines: string list) =
  lines
  |> List.map parseIntegers
  |> List.map predictPrevInLine