namespace Tests

open System
open System.Linq
open Xunit
open AOC_Common

module Test6 = 
  open AOC6

  let combined = "
abc

a
b
c

ab
ac

a
a
a
a

b"

  [<Fact>]
  let ``prevalidate 6a`` () =
    Assert.Equal(3, "abc" |> toMultiLineBlocks |> Array.head |> yesCount) 
    Assert.Equal(3, "a\nb\nc" |> toMultiLineBlocks |> Array.head |> yesCount)
    Assert.Equal(3, "ab\nac" |> toMultiLineBlocks |> Array.head |> yesCount)
    Assert.Equal(1, "a\na\na\na" |> toMultiLineBlocks |> Array.head |> yesCount)
    Assert.Equal(1, "b" |> toMultiLineBlocks |> Array.head |> yesCount)
    Assert.Equal([3;3;3;1;1], combined |> toMultiLineBlocks |> Array.map yesCount)

  [<Fact>]
  let ``count yeses`` () =
    Assert.Equal(461, inputs.Length)
    Assert.Equal(6534, inputs |> Array.map yesCount |> Array.sum)

  [<Fact>]
  let ``prevalidate 6b`` () =
    Assert.Equal([3;0;1;1;1], combined |> toMultiLineBlocks |> Array.map discreteBlocks |> Array.map yesAll)

  [<Fact>]
  let ``count yes alls`` () =
    let discreteData = inputs |> Array.map discreteBlocks
    Assert.Equal(461, discreteData.Length)
    Assert.Equal(1601, discreteData |> Array.sumBy (fun x -> x.Length))
    Assert.Equal(3402, discreteData |> Array.map yesAll |> Array.sum)
