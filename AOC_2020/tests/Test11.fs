namespace Tests

open System
open System.Linq
open Xunit
open AOC_Common


module Test11 =
  open AOC11

  let sample = 
    "L.LL.LL.LL
    LLLLLLL.LL
    L.L.L..L..
    LLLL.LL.LL
    L.LL.LL.LL
    L.LLLLL.LL
    ..L.L.....
    LLLLLLLLLL
    L.LLLLLL.L
    L.LLLLL.LL"
    |> preProcess
    |> asSeating 

  [<Fact>]
  let ``prevalidate 11a`` () =
    Assert.Equal(12, sample.height)  //includes surrounding with floor
    let phase1 = sample |> getSeated
    Assert.Equal(71, phase1 |> numSeated)
    let phase2 = phase1 |> getSeated
    Assert.Equal(20, phase2 |> numSeated)
    let phase3 = phase2 |> getSeated
    Assert.Equal(51, phase3 |> numSeated)
    let phase4 = phase3 |> getSeated
    Assert.Equal(30, phase4 |> numSeated)
    let phase5 = phase4 |> getSeated
    Assert.Equal(37, phase5 |> numSeated)
    let phase6 = phase5 |> getSeated
    Assert.Equal(37, phase6 |> numSeated)

  [<Fact>]
  let ``Count stable occupied seats`` () =
    Assert.Equal(99, inputs.Length)  
    let seating = inputs |> asSeating
    Assert.Equal(101, seating.height)  //includes surrounding with floor
    Assert.Equal(94, seating.width)    //includes surrounding with floor
    let phase1 = seating |> getSeated
    Assert.Equal(7417, phase1 |> numSeated)
    Assert.Equal(2346,seating |> findStable |> numSeated )