module Tests.Day10

open System
open Xunit
open FsUnit.Xunit
open AOC2021.Common
open Microsoft.FSharp.Core
open AOC2021.Day10

let sample = [
  "[({(<(())[]>[[{[]{<()<>>"
  "[(()[<>])]({[<{<<[]>>("
  "{([(<{}[<>[]}>{[]{[(<()>"
  "(((({<>}<{<{<>}{[]{[]{}"
  "[[<[([]))<([[{}[[()]]]"
  "[{[{({}]{}}([{[{{{}}([]"
  "{<[[]]>}<{[{[{[]{()[[[]"
  "[<(<(<(<{}))><([]([]()"
  "<{([([[(<>()){}]>(<<{{"
  "<{([{{}}[<[[[<>{}]]]>[]]"
]

[<Fact>]
let A_Sample () =
  let result = 
    sample
    |> Seq.map findIllegal
    |> Seq.choose (fun x -> 
      match x with
      | Error n -> Some n
      | Ok _ -> None
    )
    |> Seq.sum
  result |> should equal 26397

[<Fact>]
let A () =
  let result = 
    lines 10
    |> Seq.map findIllegal
    |> Seq.choose (fun x -> 
      match x with
      | Error n -> Some n
      | Ok _ -> None
    )
    |> Seq.sum
  result |> should equal 26397



