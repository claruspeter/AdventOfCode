module Tests.Day14

open System
open Xunit
open FsUnit.Xunit
open AOC2021.Common
open AOC2021.Day14

let sample = [|
  "NNCB"
  ""
  "CH -> B"
  "HH -> N"
  "CB -> H"
  "NH -> C"
  "HB -> C"
  "HC -> B"
  "HN -> C"
  "NN -> C"
  "BH -> H"
  "NC -> B"
  "NB -> B"
  "BN -> B"
  "BB -> N"
  "BC -> B"
  "CC -> N"
  "CN -> C"
|]

[<Fact>]
let A_Sample () =
  let starting, rules = 
    sample
    |> parsePolymerTemplate
  let stepper = stepPolymer rules
  let after1 = stepper starting 
  after1 |> should equal "NCNBCHB"
  let after2 = stepper after1 
  after2 |> should equal "NBCCNBBBCBHCB"
  let after3 = stepper after2 
  after3 |> should equal "NBBBCNCCNBBNBNBBCHBHHBCHB"
  let after4 = stepper after3 
  after4 |> should equal "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"    

  let after10 =
    [1..10]
    |> Seq.fold (fun acc _ -> stepper acc) starting
  after10.Length |> should equal 3073
  let counts = 
    after10
    |> Seq.countBy id
    |> Seq.sortBy snd
  let most = counts |> Seq.last
  let least = counts |> Seq.head
  most |> should equal ('B', 1749)
  least |> should equal ('H', 161)
  (snd most) - (snd least) |> should equal 1588

[<Fact>]
let A () =
  let starting, rules = 
    lines 14
    |> parsePolymerTemplate
  let stepper = stepPolymer rules
  let after10 =
    [1..10]
    |> Seq.fold (fun acc _ -> stepper acc) starting
  let counts = 
    after10
    |> Seq.countBy id
    |> Seq.sortBy snd
  let most = counts |> Seq.last
  let least = counts |> Seq.head
  (snd most) - (snd least) |> should equal 2797

[<Fact>]
let B_Sample () =
  let starting, rules = 
    sample
    |> parsePolymerTemplate
  let polymer = initPolymer starting
  let stepper = stepPolymer2 rules
  let after1 = stepper polymer 
  after1.length |> should equal 7L
  after1.numPair "NN" |> should equal 0L
  after1.numPair "BC" |> should equal 1L
  after1.numPair "CH" |> should equal 1L
  after1.numPair "CN" |> should equal 1L
  after1.numPair "HB" |> should equal 1L
  after1.numPair "NB" |> should equal 1L
  after1.numPair "NC" |> should equal 1L
  after1.numElement 'B' |> should equal 2L
  let after2 = stepper after1 
  after2.numElement 'B' |> should equal 6L
  after2.length |> should equal 13L
  let after3 = stepper after2
  after3.length |> should equal 25L
  after3.numElement 'B' |> should equal 11L
  let after4 = stepper after3
  after4.length |> should equal 49L
  let after5 = stepper after4
  after5.length |> should equal 97L

  let after10 =
    [1..10]
    |> Seq.fold (fun acc _ -> stepper acc) polymer
  after10.length |> should equal 3073L
  let most = after10.elements |> Seq.maxBy (fun x -> snd x)
  let least = after10.elements |> Seq.minBy (fun x -> snd x)
  most |> should equal ('B', 1749L)
  least |> should equal ('H', 161L)
  (snd most) - (snd least) |> should equal 1588L

  let after40 =
    [1..40]
    |> Seq.fold (fun acc _ -> stepper acc) polymer
  let most40 = after40.elements |> Seq.maxBy (fun x -> snd x)
  let least40 = after40.elements |> Seq.minBy (fun x -> snd x)
  most40 |> should equal ('B', 2192039569602L)
  least40 |> should equal ('H', 3849876073L)
  (snd most40) - (snd least40) |> should equal 2188189693529L

[<Fact>]
let B () =
  let starting, rules = 
    lines 14
    |> parsePolymerTemplate
  let polymer = initPolymer starting
  let stepper = stepPolymer2 rules
  let after40 =
    [1..40]
    |> Seq.fold (fun acc _ -> stepper acc) polymer
  let most = after40.elements |> Seq.maxBy (fun x -> snd x)
  let least = after40.elements |> Seq.minBy (fun x -> snd x)
  (snd most) - (snd least) |> should equal 2926813379532L    