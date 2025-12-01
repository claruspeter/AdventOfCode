module Tests.Day1

open System
open Xunit
open FsUnit.Xunit
open AOC2023.Common
open AOC2023.Day1

let sample = [
  "L68"
  "L30"
  "R48"
  "L5"
  "R60"
  "L55"
  "L1"
  "L99"
  "R14"
  "L82"
]

let start = {startingValue=50; cmd="initial"; finalValue=50; isZero=false; negativeCrosses=0; positiveCrosses=0}

[<Fact>]
let A_Sample () =
  sample
  |> List.scan dial start
  |> List.map _.finalValue
  |> should equal [
      50 
      82
      52
      0
      95
      55
      0
      99
      0
      14
      32
    ]

[<Fact>]
let A() =
  lines 1
  |> List.scan dial start
  |> List.filter (fun x -> x.finalValue = 0)
  |> List.length
  |> should equal 984

[<Fact>]
let B_Sample () =
  let result = 
    sample
    |> List.scan dial start
  result
    |> should equal [
        start
        {startingValue=50; cmd="L68"; finalValue=82; isZero=false; negativeCrosses=1; positiveCrosses=0}
        {startingValue=82; cmd="L30"; finalValue=52; isZero=false; negativeCrosses=0; positiveCrosses=0}
        {startingValue=52; cmd="R48"; finalValue=0 ; isZero=true ; negativeCrosses=0; positiveCrosses=0}
        {startingValue=0;  cmd="L5";  finalValue=95; isZero=false; negativeCrosses=0; positiveCrosses=0}
        {startingValue=95; cmd="R60"; finalValue=55; isZero=false; negativeCrosses=0; positiveCrosses=1}
        {startingValue=55; cmd="L55"; finalValue=0 ; isZero=true ; negativeCrosses=0; positiveCrosses=0}
        {startingValue=0;  cmd="L1";  finalValue=99; isZero=false; negativeCrosses=0; positiveCrosses=0}
        {startingValue=99; cmd="L99"; finalValue=0 ; isZero=true ; negativeCrosses=0; positiveCrosses=0}
        {startingValue=0;  cmd="R14"; finalValue=14; isZero=false; negativeCrosses=0; positiveCrosses=0}
        {startingValue=14; cmd="L82"; finalValue=32; isZero=false; negativeCrosses=1; positiveCrosses=0}
      ]
  result
  |> List.sumBy _.numberZeroTouches
  |> should equal 6

[<Fact>]
let B() =
  lines 1
  |> List.scan dial start
  |> List.sumBy _.numberZeroTouches
  |> should equal 5841 //too high!
