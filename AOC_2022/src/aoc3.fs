module AOC2022.Day3

open System
open AOC2022.Common

let inBothCompartments (packContents: string) : char =
  let n = packContents.Length
  packContents 
    |> Seq.chunkBySize (n/2) 
    |> Seq.map set 
    |> Seq.toList
    |> Set.intersectMany
    |> Seq.head

let private letters = ['a'..'z'] @ ['A'..'Z']

let packItemPriority (item:char) : int =
  letters |> List.findIndex ((=) item) |> (+) 1

let inElfGroups<'a> : 'a list -> 'a list list = List.chunkBySize 3

let groupBadge (grp: string list) : char =
  grp
    |> List.map set 
    |> Set.intersectMany
    |> Seq.head
