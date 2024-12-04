module AOC2023.Day1

open System
open AOC2023.Common

let private makeLists (input: string list) =
  input
  |> List.map (fun x -> x.Split([|' '|], StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries))
  |> List.map (fun x -> x |> fun pair -> (pair[0], pair[1]))
  |> List.map (fun (a,b) -> (int a, int b) )
  |> List.unzip

let calcDistances (input: string list) =
  input
  |> makeLists
  |> fun (listA, listB) -> (listA |> List.sort, listB |> List.sort)
  |> fun (listA, listB) -> List.zip listA listB
  |> List.map (fun (a,b) -> Math.Abs(a - b) )

let calcSimilarity (input: string list) =
  input
  |> makeLists
  |> fun (a,b) -> a |> List.map(fun x -> (x, b |> List.filter(fun y -> y = x) |> List.length ))
  |> List.map (fun (a,b) -> a*b )