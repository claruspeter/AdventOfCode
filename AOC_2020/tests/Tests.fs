module Tests

open System
open System.Linq
open Xunit

[<Fact>]
let ``aoc 1a`` () =
    Assert.Equal(200, AOC1.n)
    Assert.Equal(19900, AOC1.pairs.Count())
    let p2020s = AOC1.``pairs that add to 2020``
    Assert.Equal(1, p2020s.Count())
    let p2020 = p2020s |> List.head
    Assert.Equal(633216, p2020.Product)
    Assert.Equal({ AOC1.Pair.a=1632; AOC1.Pair.b=388 }, p2020)

[<Fact>]
let ``aoc 1b`` () =
    Assert.Equal(1313400, AOC1.trios.Count())
    let t2020s = AOC1.``trios that add to 2020``
    Assert.Equal(1, t2020s.Count())
    let t2020 = t2020s |> List.head
    Assert.Equal(68348924, t2020.Product)
    Assert.Equal({ AOC1.Trio.a=1607; AOC1.Trio.b=196; AOC1.Trio.c=217 }, t2020)