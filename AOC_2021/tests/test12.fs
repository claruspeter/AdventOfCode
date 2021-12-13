module Tests.Day12

open System
open Xunit
open FsUnit.Xunit
open AOC2021.Common
open AOC2021.Day12

let sample1 = [|
  "start-A"
  "start-b"
  "A-c"
  "A-b"
  "b-d"
  "A-end"
  "b-end"
|]

let sample2 = [|
  "dc-end"
  "HN-start"
  "start-kj"
  "dc-start"
  "dc-HN"
  "LN-dc"
  "HN-end"
  "kj-sa"
  "kj-HN"
  "kj-dc"
|]

let sample3 = [|
  "fs-end"
  "he-DX"
  "fs-he"
  "start-DX"
  "pj-DX"
  "end-zg"
  "zg-sl"
  "zg-pj"
  "pj-he"
  "RW-he"
  "fs-DX"
  "pj-RW"
  "zg-RW"
  "start-pj"
  "he-WI"
  "zg-he"
  "pj-fs"
  "start-RW"
|]

[<Fact>]
let A_Sample1 () =
  sample1
  |> parseCaves
  |> findPaths
  |> Seq.length |> should equal 10

[<Fact>]
let A_Sample2 () =
  sample2
  |> parseCaves
  |> findPaths
  |> Seq.length |> should equal 19

[<Fact>]
let A_Sample3 () =
  sample3
  |> parseCaves
  |> findPaths
  |> Seq.length |> should equal 226

[<Fact>]
let A () =
  lines 12
  |> parseCaves
  |> findPaths
  |> Seq.length |> should equal 4241

[<Fact>]
let B_Sample () =
  sample1
    |> parseCaves
    |> findPaths2
    |> Seq.length |> should equal 36
  sample2
    |> parseCaves
    |> findPaths2
    |> Seq.length |> should equal 103
  sample3
    |> parseCaves
    |> findPaths2
    |> Seq.length |> should equal 3509

// [<Fact>]  /// oooh - takes long time!
let B () =
  lines 12
  |> parseCaves
  |> findPaths2
  |> Seq.length |> should equal 122134
