module Tests.Day3

open System
open Xunit
open FsUnit.Xunit
open AOC2023.Common
open AOC2023.Day3


let sample = ["xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"]
let sampleB = ["xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"]

[<Fact>]
let A_Sample () =
  let muls = 
    sample
    |> List.collect extractMuls
  muls
    |> should equal [(2,4); (5,5); (11,8); (8,5)] 
  muls
    |> List.map calcMul
    |> List.sum
    |> should equal 161

[<Fact>]
let A () =
  lines 3
    |> List.collect extractMuls
    |> List.map calcMul
    |> List.sum
    |> should equal 153469856

[<Fact>]
let B_Sample () =
  let muls = 
    sampleB
    |> List.collect splitConditional
    |> List.collect extractMuls
  muls
    |> should equal [(2,4); (8,5)] 
  muls
    |> List.map calcMul
    |> List.sum
    |> should equal 48

[<Fact>]
let B () =
  lines 3
    |> List.collect splitConditional
    |> List.collect extractMuls
    |> List.map calcMul
    |> List.sum
    |> should equal 153469856