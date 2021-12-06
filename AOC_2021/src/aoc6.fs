module AOC2021.Day6

open System
open AOC2021.Common

let nextDay fish =
  fish
  |> Seq.collect (fun n -> 
      match n with 
      | 0 -> [6;8]
      | _ -> [n-1]
  )
