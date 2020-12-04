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

module Test3 = 
  open AOC3

  [<Fact>]
  let ``aoc 3a - check mechanics`` () =
    let doMove = move 3 1
    Assert.Equal(Space {x=0;y=0},  isTree {x=0;y=0}  )
    Assert.Equal(Tree {x=3;y=1},  doMove (Space {x=0;y=0}) )
    Assert.Equal(Tree {x=6;y=2},  doMove (Tree {x=3;y=1}) )

  [<Fact>]
  let ``aoc 3a`` () =
    let chain = traverse 3 1 (Space {x=0;y=0})
    Assert.Equal(inputs.Length + 1, chain.Length)  // extra one for the Done at the end
    Assert.Equal(240, nTrees chain)

  [<Fact>]
  let ``aoc 3b`` () =
    let start = (Space {x=0;y=0})
    let a = start |> traverse 1 1 |> nTrees |> int64
    let b = start |> traverse 3 1 |> nTrees |> int64
    let c = start |> traverse 5 1 |> nTrees |> int64
    let d = start |> traverse 7 1 |> nTrees |> int64
    let e = start |> traverse 1 2 |> nTrees |> int64
    Assert.Equal(2832009600L, a * b * c * d * e)
