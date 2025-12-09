module Tests.Day8

open System
open Xunit
open FsUnit.Xunit
open AOC2023.Common
open AOC2023.Day8

let sample = [
  "162,817,812"
  "57,618,57"
  "906,360,560"
  "592,479,940"
  "352,342,300"
  "466,668,158"
  "542,29,236"
  "431,825,988"
  "739,650,466"
  "52,470,668"
  "216,146,977"
  "819,987,18"
  "117,168,530"
  "805,96,715"
  "346,949,466"
  "970,615,88"
  "941,993,340"
  "862,61,35"
  "984,92,344"
  "425,690,689"
]

[<Fact>]
let A_Sample () =
  let boxes = 
    sample
    |> parse
  let byDistance =
    boxes
    |> calculateDistances
    |> List.sortBy _.distance
  byDistance
    |> List.take 3
    |> List.map (fun x -> $"{x.a.x},{x.a.y},{x.a.z} - {x.b.x},{x.b.y},{x.b.z}")
    |> should equal [ "162,817,812 - 425,690,689"; "162,817,812 - 431,825,988"; "906,360,560 - 805,96,715"]
  let connected = 
    byDistance
    |> connect 10 boxes
  connected
    |> List.map _.boxes.Count
    |> List.sortDescending
    |> should equal [ 5; 4; 2; 2; 1; 1; 1; 1; 1; 1; 1]

[<Fact>]
let A () =
  let boxes = 
    lines 8
    |> parse
  let byDistance =
    boxes
    |> calculateDistances
    |> List.sortBy _.distance
  let connected = 
    byDistance
    |> connect 1000 boxes
  connected
    |> List.map _.boxes.Count
    |> List.sortDescending
    |> List.take 3
    |> List.reduce ( * )
    |> should equal 75582



