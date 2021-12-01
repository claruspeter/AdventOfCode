module AOC2021.Day1

open System
open AOC2021.Common

let countIncreasingDepths data =
  data
  |> Seq.pairwise
  |> Seq.filter (fun (a,b) -> a < b )
  |> Seq.length

let sumWindow windowSize (data: seq<int>) =
  data
  |> Seq.windowed windowSize
  |> Seq.map (Seq.sum)