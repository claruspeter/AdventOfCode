module AOC2023.Day5

open System
open AOC2023.Common

type Range = {
  a: int64
  b: int64
}with
  member this.includes x = x >= this.a && x <= this.b
  member this.length = this.b - this.a + 1L
  static member parse (s:string) = 
    let parts = s.Split('-')
    {
      a=parts.[0] |> Int64.Parse
      b=parts.[1] |> Int64.Parse
    }

let private expandRange range = [range.a..range.b]

//   |-----------|
//           |-------|
let private overlaps r1 r2 = r1.a <= r2.b && r1.b >= r2.a

//     |-----------|
//           |-------|
// =>  |-------------|
let private combine r1 r2 = {a = Math.Min(r1.a, r2.a); b = Math.Max(r1.b, r2.b)}

type Ingredients = {
  fresh: Range list
  available: int64 list
}



let private isFresh (ranges: Range list) item =
  ranges
  |> List.exists (fun r -> r.includes item)

let findSpoiled ingredients =
  ingredients.available
  |> List.filter (isFresh ingredients.fresh >> not)

let findFresh ingredients =
  ingredients.available
  |> List.filter (isFresh ingredients.fresh)

let parse (lines: string list ) =
  let f, a = 
    lines
    |> List.filter (String.IsNullOrWhiteSpace >> not)
    |> List.partition (fun line -> line.Contains('-'))
  {
    fresh = f |> List.map Range.parse
    available = a |> List.map Int64.Parse
  }

let private mergeRanges (ranges: Range list) =
  ranges
  |> List.fold (
      fun acc x -> 
        match acc |> List.partition (overlaps x) with 
        | [], _ -> acc @ [x]
        | overlapping, notOverlapping -> 
            let combined = overlapping |> List.reduce (fun acc i -> combine acc i)
            notOverlapping @ [combined]
  ) []

let countFresh ingredients =
  ingredients.fresh
  |> mergeRanges
  |> List.sumBy _.length
