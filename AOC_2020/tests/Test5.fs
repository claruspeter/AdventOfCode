namespace Tests

open System
open System.Linq
open Xunit
open AOC_Common

module Test5 = 
  open AOC5

  [<Fact>]
  let ``prevalidate 5a`` () =
    Assert.Equal(902, n)
    Assert.Equal(44, departition "FBFBBFF")
    Assert.Equal(70, departition "BFFFBBF")
    Assert.Equal(14, departition "FFFBBBF")
    Assert.Equal(102, departition "BBFFBBF")
    Assert.Equal(5, departition "RLR")
    Assert.Equal(7, departition "RRR")
    Assert.Equal(4, departition "RLL")
    Assert.Equal(357, parseSeat("FBFBBFFRLR").id )
    Assert.Equal(567, parseSeat("BFFFBBFRRR").id )
    Assert.Equal(119, parseSeat("FFFBBBFRRR").id )
    Assert.Equal(820, parseSeat("BBFFBBFRLL").id )

  [<Fact>]
  let ``Heighest Seat Id`` () =
    let seatIds = 
      inputs
      |> Array.map parseSeat
      |> Array.map (fun s -> s.id)
    Assert.Equal(970, seatIds |> Array.max)
    
  [<Fact>]
  let ``Find my seat`` () =
    let seatIds = 
      inputs
      |> Array.map parseSeat
      |> Array.map (fun s -> s.id)
      |> Array.sort
    let max = seatIds |> Array.max
    let min = seatIds |> Array.min
    let allSeats = [|min..max|]
    let emptySeats = allSeats |> Array.except seatIds
    Assert.Equal(1, emptySeats.Length)
    Assert.Equal(587, emptySeats.[0])