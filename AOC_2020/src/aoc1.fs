module AOC1

open AOC_Common
open AOC_Pairs

let inputs = 
    (raw 1).Split([|'\r'; '\n'|])
    |> Array.choose parseInts
    |> Array.map int64

let  n = inputs.Length

let pairs = 
  inputs 
  |> Array.toList
  |> pairup

let trios = 
  inputs 
  |> Array.toList
  |> trioup

let ``pairs that add to 2020``=
  pairs
  |> List.filter (fun x -> x.Sum = 2020L)

let ``trios that add to 2020``=
  trios
  |> List.filter (fun x -> x.Sum = 2020L)