namespace Tests

open System
open System.Collections.Generic
open System.Linq
open Xunit
open AOC_Common


module Test9 =
  open AOC9

  let sample = 
    "35
    20
    15
    25
    47
    40
    62
    55
    65
    95
    102
    117
    150
    182
    127
    219
    299
    277
    309
    576"
    |> preProcess
    |> List.map Int64.Parse

  [<Fact>]
  let ``prevalidate 9a`` () =
    Assert.Equal(1000, n)
    Assert.Equal(5, sample |> preambleList 5 |> fst |> List.length)
    Assert.Equal(15, sample |> preambleList 5 |> snd |> List.length)
    Assert.Equal(127L, sample |> preambleList 5 ||> findNonSum )

  [<Fact>]
  let ``Find non-sum`` () =
    Assert.Equal(1504371145L, inputs |> preambleList 25 ||> findNonSum )

  [<Fact>]
  let ``prevalidate 9b`` () =
    Assert.Equal<int64 list>([15L; 25L; 47L; 40L], sample |> contiguousAddends 127L  )

  [<Fact>]
  let ``Find contiguous addends`` () =
    let contiguous = inputs |> contiguousAddends 1504371145L
    let weakness = (contiguous |> List.min) + (contiguous |> List.max)
    Assert.Equal(183278487L, weakness )