namespace Tests

open System
open System.Linq
open Xunit

module Test2 =
  open AOC2

  [<Fact>]
  let ``aoc 2a`` () =
    Assert.Equal(1000, n)
    Assert.Equal(546, numValid valid inputs)
    Assert.Equal(275, numValid validV2 inputs)

  [<Fact>]
  let ``aoc 2b`` () =
    Assert.Equal(275, numValid validV2 inputs)

