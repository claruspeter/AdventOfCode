namespace Tests

open System
open System.Linq
open Xunit
open AOC_Common


module Test10 =
  open AOC10
  
  let smallSample =
    "16
    10
    15
    5
    1
    11
    7
    19
    6
    12
    4"
    |> preProcess
    |> List.choose parseInts

  let sample = 
    "28
    33
    18
    42
    31
    14
    46
    20
    48
    47
    24
    23
    49
    45
    19
    38
    39
    11
    1
    32
    25
    35
    8
    17
    7
    9
    4
    2
    34
    10
    3"
    |> preProcess
    |> List.choose parseInts

  [<Fact>]
  let ``prevalidate 10a`` () =
    let steps = sample |> makeSteps
    Assert.Equal(1, steps.[0])
    Assert.False(steps.Any(fun x -> x < 0))
    Assert.Equal(22, steps |> List.filter (fun x -> x = 1) |> List.length)
    Assert.Equal(10, steps |> List.filter (fun x -> x = 3) |> List.length)

  [<Fact>]
  let ``Count steps to adapter`` () =
    Assert.Equal(114, n)
    let steps = inputs |> makeSteps
    Assert.Equal(75, steps |> List.filter (fun x -> x = 1) |> List.length)
    Assert.Equal(40, steps |> List.filter (fun x -> x = 3) |> List.length)
    Assert.Equal(3000, 75 * 40)

  [<Fact>]
  let ``prevalidate 10b`` () =
    Assert.Equal(8L, smallSample |> countCombos)
    Assert.Equal(19208L, sample |> countCombos)

  [<Fact>]
  let ``Count combinations`` () =
    Assert.Equal(193434623148032L, inputs |> countCombos)