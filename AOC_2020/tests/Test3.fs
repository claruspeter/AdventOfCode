namespace Tests

open System
open System.Linq
open Xunit

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


