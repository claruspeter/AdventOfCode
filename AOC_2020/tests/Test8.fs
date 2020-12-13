namespace Tests

open System
open System.Linq
open Xunit
open AOC_Common


module Test8 =
  open AOC8

  let sample = "nop +0
    acc +1
    jmp +4
    acc +3
    jmp -3
    acc -99
    acc +1
    jmp -4
    acc +6"


  let ErrorValue (err:Result<_,_>) =
    match err with 
    | Ok _ -> failwith "Not an error"
    | Error x -> x
    
  [<Fact>]
  let ``prevalidate 8a`` () =
    Assert.Equal(641, n)
    Assert.Equal(641, inputs |> instructions |> Seq.length)
    Assert.Equal(Error 5, sample |> preProcess |> instructions |> execute )

  [<Fact>]
  let ``Get accumulator when infinite loop starts`` () =
    Assert.Equal(2080, inputs |> instructions  |> execute |> ErrorValue )


  [<Fact>]
  let ``prevalidate 8b`` () =
    Assert.Equal(Ok 8, sample |> preProcess |> instructions |> executeFiddle )


  [<Fact>]
  let ``Get accumulator when infinite loop is corrected`` () =
    Assert.Equal(2477, inputs |> instructions  |> executeFiddle |> fun x -> x.ResultValue )