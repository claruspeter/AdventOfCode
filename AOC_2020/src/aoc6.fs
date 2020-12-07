module AOC6

open System
open AOC_Common


let inputs = 
    raw 6
    |> toMultiLineBlocks

let n = inputs.Length

let yesCount (data:string) =
  data
  |> Set
  |> Set.remove ' '
  |> Set.count

let discreteBlocks (data:string) =
  data.Trim().Split(' ')

let yesAll (discreteData:string []) =
  discreteData
  |> Array.map Set
  |> Set.intersectMany
  |> Set.count