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
  result |> should equal 344193

[<Fact>]
let B_Sample () =
  let result = 
    sample
    |> Seq.map findIllegal
    |> Seq.choose (fun x -> 
      match x with
      | Error _ -> None
      | Ok a -> Some a
    )
  let scores = 
    result
    |> Seq.map scoreClosers
    |> Seq.toList
  scores |> should equal [288957L; 5566L; 1480781L; 995444L; 294L]
  scores 
  |> List.sort 
  |> fun data -> data.[data.Length / 2] 
  |> should equal 288957L

[<Fact>]
let B () =
  let result = 
    lines 10
    |> Seq.map findIllegal
    |> Seq.choose (fun x -> 
      match x with
      | Error _ -> None
      | Ok a -> Some a
    )
  result
    |> Seq.map scoreClosers
    |> Seq.sort 
    |> Seq.toList
    |> fun data -> data.[data.Length / 2] 
    |> should equal 3241238967L