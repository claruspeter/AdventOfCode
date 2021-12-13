module Tests.Day11

open System
open Xunit
open FsUnit.Xunit
open AOC2021.Common
open AOC2021.Grids
open AOC2021.Day11

let sample = [|
  "5483143223"
  "2745854711"
  "5264556173"
  "6141336146"
  "6357385478"
  "4167524645"
  "2176841721"
  "6882881134"
  "4846848554"
  "5283751526"
|]
let sample2 = [|
  "11111"
  "19991"
  "19191"
  "19991"
  "11111"
|]

[<Fact>]
let A_Small_Sample () =
  let after1 =
    sample2
    |> parseOctupus
    |> stepTime
    // |> logf (printHeightMap (fun c -> c.energy.ToString()))
  after1.values
  |> Array.filter (fun a -> (snd a).totalenergy > 9)
  |> Array.length |> should equal 9
  let after2 =
    after1
    |> stepTime
    // |> logf (printHeightMap (fun c -> c.energy.ToString()))
  after2.values
  |> Array.filter (fun a -> (snd a).totalenergy > 9)
  |> Array.length |> should equal 9


[<Fact>]
let A_Sample () =
  let after2 =
    sample
    |> parseOctupus
    |> stepTime
    |> stepTime
  after2.values
    |> Array.filter (fun a -> (snd a).totalenergy > 9)
    |> Array.length
    |> should equal 35

  let data = sample |> parseOctupus
  let after100 = 
    [1..100]
    |> Seq.fold (fun g i -> stepTime g ) data
  after100.values
    |> Array.sumBy (fun a -> (snd a).flashes)
    |> should equal 1656

[<Fact>]
let A () =
  let data = lines 11 |> parseOctupus
  let after100 = 
    [1..100]
    |> Seq.fold (fun g i -> stepTime g ) data
  after100.values
    |> Array.sumBy (fun a -> (snd a).flashes)
    |> should equal 1591

[<Fact>]
let B_Sample () =
  let data = sample |> parseOctupus
  let after195 = 
    [1..195]
    |> Seq.fold (fun g i -> stepTime g ) data
  after195.values
    |> Array.filter (fun a -> (snd a).energy = 0)
    |> Array.length
    |> should equal 100
  stepTimeTillSynchronised 0 data
  |> should equal 195

[<Fact>]
let B () =
  lines 11 
    |> parseOctupus
    |> stepTimeTillSynchronised 0
    |> should equal 314