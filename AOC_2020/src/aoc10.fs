module AOC10

open AOC_Common

let inputs = 
    lines 10
    |> Array.toList
    |> List.choose parseInts

let  n = inputs.Length

let makeSteps data =
  let max = data |> List.max
  let sorted = [0] @ (data |> List.sort) @ [max + 3]
  sorted 
  |> List.pairwise
  |> List.map (fun (a, b) -> b - a)