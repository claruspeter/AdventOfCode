module Tests.Day7

open System
open Xunit
open FsUnit.Xunit
open AOC2023.Common
open AOC2023.Day7

let sample = [
  "32T3K 765"
  "T55J5 684"
  "KK677 28"
  "KTJJT 220"
  "QQQJA 483"
]

[<Fact>]
let A_Sample () =
  sample
  |> parseAndSortHands
  |> List.mapi (fun i hand -> (hand.bid, i + 1))
  |> should equal [(765,1); (220,2); (28,3); (684,4); (483,5)]

[<Fact>]
let A () =
  lines 7
  |> parseAndSortHands
  |> List.mapi (fun i hand -> hand.bid * (i + 1))
  |> List.sum
  |> should equal 250347426

[<Fact>]
let B_Sample () =
  sample
  |> parseAndSortHandsWithJacksWild
  |> List.mapi (fun i hand -> (hand.bid, i + 1))
  |> should equal [(765,1); (28,2); (684,3); (483,4); (220,5)]

[<Fact>]
let B () =
  lines 7
  |> parseAndSortHandsWithJacksWild
  |> List.mapi (fun i hand -> hand.bid * (i + 1))
  |> List.sum
  |> should equal 253134644