module AOC2022.Day1

open System
open System.Collections.Generic
open AOC2022.Common

let groupCalories (lines: string list) =
  lines 
  |> List.fold 
    (fun acc line -> 
      match line, acc with 
      | "", _ -> [[]] @ acc
      //| x, [head] -> [ head @ [ x ] ]
      | x, head::tail -> [ head @ [ Int64.Parse(x) ] ] @ tail
      | _ -> failwithf "huh?  %A %A" line acc
    ) 
    [[]]
  |> List.rev