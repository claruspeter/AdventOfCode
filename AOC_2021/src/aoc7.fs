module AOC2021.Day7

open System
open AOC2021.Common

let cheapestSpot (data: int[]) =
  //assume the median is the cheapest position 
  data.[data.Length / 2]

let movementCost (center:int) (data: int[]) =
  data
  |> Array.map (fun x -> Math.Abs(center - x))
  |> Array.sum

let increasingMovementCost (a:int) (b:int) =
  // Use Gauss' equation (n / 2)(first number + last number) 
  // https://study.com/academy/lesson/finding-the-sum-of-consecutive-numbers.html
  let distance = float (Math.Abs(b - a))
  ( (distance + 1.0) / 2.0) * distance |> int

let calcIncreasingCost (center: int) (data: int[]) =
  data
  |> Array.map (increasingMovementCost center)
  |> Array.sum

type CenterCost = {center:int; cost: int}

let findIncreasingCenter (data: int[]) =
  let max = data |> Array.last
  let min = data.[0]
  [min..max]
  |> List.map (fun i -> calcIncreasingCost i data)
  |> List.indexed
  |> List.minBy snd
  |> fun x -> {center = fst x; cost = snd x }