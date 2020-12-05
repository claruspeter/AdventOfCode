namespace Tests

open System
open System.Linq
open Xunit


module Test1 =
  open AOC1

  [<Fact>]
  let ``aoc 1a`` () =
    Assert.Equal(200, n)
    Assert.Equal(19900, pairs.Count())
    let p2020s = ``pairs that add to 2020``
    Assert.Equal(1, p2020s.Count())
    let p2020 = p2020s |> List.head
    Assert.Equal(633216, p2020.Product)
    Assert.Equal({ a=1632; b=388 }, p2020)

  [<Fact>]
  let ``aoc 1b`` () =
    Assert.Equal(1313400, trios.Count())
    let t2020s = ``trios that add to 2020``
    Assert.Equal(1, t2020s.Count())
    let t2020 = t2020s |> List.head
    Assert.Equal(68348924, t2020.Product)
    Assert.Equal({ a=1607; b=196; c=217 }, t2020)



