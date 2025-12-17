module Tests.Day11

open System
open Xunit
open FsUnit.Xunit
open AOC2023.Common
open AOC2023.Day11

let sample = [
  "aaa: you hhh"
  "you: bbb ccc"
  "bbb: ddd eee"
  "ccc: ddd eee fff"
  "ddd: ggg"
  "eee: out"
  "fff: out"
  "ggg: out"
  "hhh: ccc fff iii"
  "iii: out"
]

[<Fact>]
let A_Sample () =
  sample
  |> parse
  |> log
  |> findRoutes
  |> ignore
  // |> should equal [
  //   [ "you"; "bbb"; "ddd"; "ggg"; "out" ]
  //   [ "you"; "bbb"; "eee"; "out"]
  //   [ "you"; "ccc"; "ddd"; "ggg"; "out"]
  //   [ "you"; "ccc"; "eee"; "out"]
  //   [ "you"; "ccc"; "fff"; "out"]
  // ]

