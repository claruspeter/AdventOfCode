module Tests.Day13

open System
open Xunit
open FsUnit.Xunit
open AOC2021.Common
open AOC2021.Grids
open AOC2021.Day13

let sample = [|
  "6,10"
  "0,14"
  "9,10"
  "0,3"
  "10,4"
  "4,11"
  "6,0"
  "6,12"
  "4,1"
  "0,13"
  "10,12"
  "3,4"
  "3,0"
  "8,4"
  "1,10"
  "2,14"
  "8,10"
  "9,0"
  ""
  "fold along y=7"
  "fold along x=5"
|]

[<Fact>]
let A_Sample () =
  let points, instructions =
    sample
    |> Array.partition (fun x -> x.Length > 0 && x.[0] |> Char.IsDigit) 
  let positions = points |> Array.map Position.parse
  let folds = instructions |> Array.choose parseFold
  let paper = {values = positions |> Array.map (fun p -> (p,true) ) }
  paper.values.Length |> should equal 18

  let folded1 = foldAlong paper (folds.[0])
  folded1.values.Length |> should equal 17

  let folded2 = foldAlong folded1 (folds.[1])
  folded2.values.Length |> should equal 16
  
[<Fact>]
let A () =
  let points, instructions =
    lines 13
    |> Array.partition (fun x -> x.Length > 0 && x.[0] |> Char.IsDigit) 
  let positions = points |> Array.map Position.parse
  let folds = instructions |> Array.choose parseFold
  let paper = {values = positions |> Array.map (fun p -> (p,true) ) }
  let folded1 = foldAlong paper (folds.[0])
  folded1.values.Length |> should equal 788

[<Fact>]
let B () =
  let points, instructions =
    lines 13
    |> Array.partition (fun x -> x.Length > 0 && x.[0] |> Char.IsDigit) 
  let positions = points |> Array.map Position.parse
  let folds = instructions |> Array.choose parseFold
  let paper = {values = positions |> Array.map (fun p -> (p,true) ) }
  let folded = 
    folds
    |> Seq.fold foldAlong paper
  folded |> printHeightMap (fun b -> if b then "#" else ".") |> ignore //|> printfn "%s"
  
