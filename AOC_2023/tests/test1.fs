module Tests.Day1

open System
open Xunit
open FsUnit.Xunit
open AOC2023.Common
open AOC2023.Day1

[<Fact>]
let A_Sample () =
  let input = [
    "1abc2"
    "pqr3stu8vwx"
    "a1b2c3d4e5f"
    "treb7uchet"
  ]
  input
  |> List.map calibrationValue
  |> should equal [12; 38; 15; 77]

[<Fact>]
let A () =
  lines 1
  |> List.map calibrationValue
  |> List.sum
  |> should equal 53194

[<Fact>]
let B_Sample () =
  let input = [
    "two1nine"
    "eightwothree"
    "abcone2threexyz"
    "xtwone3four"
    "4nineeightseven2"
    "zoneight234"
    "7pqrstsixteen"
    "xxxxonexxxx"
  ]
  input
  |> List.map calibrationValueWithWords
  |> should equal [29; 83; 13; 24; 42; 14; 76; 11]

[<Fact>]
let B () =
  lines 1
  |> List.map calibrationValueWithWords
  |> List.sum
  |> should equal 54249