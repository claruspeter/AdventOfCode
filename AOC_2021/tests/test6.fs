module Tests.Day6

open System
open Xunit
open FsUnit.Xunit
open AOC2021.Common
open AOC2021.Day6

let sample = "3,4,3,1,2"
let sampleAfter18Days = 
  [6;0;6;4;5;6;0;1;1;2;6;0;1;1;1;2;2;3;3;4;6;7;8;8;8;8]
  |> Seq.sort

[<Fact>]
let A_Sample () =
  let data = sample.Split([|','|]) |> Seq.map Int32.Parse
  let day18 = 
    [1..18] 
    |> Seq.fold (fun acc _ -> nextDay acc ) data
  day18 |> printseq |> should equal (sampleAfter18Days |> printseq)
  day18 |> Seq.length |> should equal 26
  let day80 = 
    [1..80] 
    |> Seq.fold (fun acc _ -> nextDay acc ) data
  day80 |> Seq.length |> should equal 5934

[<Fact>]
let A () =
  let data = (raw 6).Split([|','|]) |> Seq.map Int32.Parse
  let day80 = 
    [1..80] 
    |> Seq.fold (fun acc _ -> nextDay acc ) data
  day80 |> Seq.length |> should equal 390923
