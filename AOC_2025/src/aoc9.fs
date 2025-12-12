module AOC2023.Day9

open System
open AOC2023.Common

type Point = {
  x: int
  y: int
}

type Rectangle = {
  a: Point
  b: Point
}with 
  member this.area = 
    let dx = Math.Abs(this.a.x - this.b.x) + 1 |> int64
    let dy = Math.Abs(this.a.y - this.b.y) + 1 |> int64
    dx * dy

let private distance a b = 
  let dx = Math.Abs(a.x - b.x) + 1 |> float
  let dy = Math.Abs(a.y - b.y) + 1 |> float
  Math.Sqrt( dx * dx + dy * dy )

let parse (lines: string list) = 
  lines
  |> List.map (
    fun line ->
      line.Split ','
      |> Array.map Int32.Parse
      |> fun parts -> {x = parts.[0]; y = parts.[1]}
  )

let printGrid points = 
  let maxX = points |> List.map _.x |> List.max
  let maxY = points |> List.map _.y |> List.max
  [0 .. maxY + 1]
  |> Seq.map (
    fun y ->
      [0..maxX + 1]
      |> Seq.map (
        fun x ->
          match points |> List.contains {x=x;y=y} with
          | true -> '#'
          | false -> '.'
      )
      |> Seq.toArray
      |> String
  )
  |> Join "\n"

let findLargestRectangle points = 
  points
  |> Seq.allPairs points
  |> Seq.filter (fun (a,b) -> a.x < b.x)
  |> Seq.map (fun (a,b) -> { a=a; b=b })
  |> Seq.sortByDescending _.area
  |> Seq.head
