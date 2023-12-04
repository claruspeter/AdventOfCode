module Tests.Day3

open System
open Xunit
open FsUnit.Xunit
open AOC2023.Common
open AOC2023.Day3

let sample = [
  "467..114.."
  "...*......"
  "..35..633."
  "......#..."
  "617*......"
  ".....+.58."
  "..592....."
  "......755."
  "...$.*...."
  ".664.598.."
]

[<Fact>]
let A_Sample () =
  sample
  |> partNumbers
  |> should equal [467;35;633;617;592;755;664;598]

[<Fact>]
let A () =
  lines 3
  |> partNumbers
  |> List.sum
  |> should equal 532445

