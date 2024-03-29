module Tests.Day8

open System
open Xunit
open FsUnit.Xunit
open AOC2021.Common
open AOC2021.Day8

let sample = [
  "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
  "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
  "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
  "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
  "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
  "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
  "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
  "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
  "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
  "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
]

[<Fact>]
let A_Sample () =
  let data = sample |> Seq.map parse7DigitDisplayLine
  data 
  |> Seq.collect (fun x ->  x |> snd |> Seq.choose classify)
  |> Seq.length
  |> should equal 26

[<Fact>]
let A () =
  let data = lines 8 |> Seq.map parse7DigitDisplayLine
  data 
  |> Seq.collect (fun x ->  x |> snd |> Seq.choose classify)
  |> Seq.length
  |> should equal 548

[<Fact>]
let B_Sample () =
  let data = sample |> Seq.map parse7DigitDisplayLine
  let result = 
    data
    |> Seq.map decode
    |> Seq.toList
  result |> should equal [ 8394; 9781; 1197; 9361; 4873; 8418; 4548; 1625; 8717; 4315 ]

[<Fact>]
let B () =
  let data = (lines 8) |> Seq.map parse7DigitDisplayLine
  let result = 
    data
    |> Seq.map decode
    |> Seq.sum
  result |> should equal 1074888