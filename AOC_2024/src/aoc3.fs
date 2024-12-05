module AOC2023.Day3

open System
open AOC2023.Common

let extractMuls (line:string) = 
  match line with 
  | Regex @"mul\((\d+),(\d+)\)" x -> 
    x
    |> List.chunkBySize 3
    |> List.map (fun x -> (x.[1] |> int, x.[2] |> int))
  | _ -> []

let calcMul (a,b) = a*b