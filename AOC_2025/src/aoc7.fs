module AOC2023.Day7

open System
open AOC2023.Common

type TachyonPath = {
  lines: string list
  numSplit: int
}

let parse (lines: string list ) =
  {
    lines = lines
    numSplit = 0
  }

let private inc (by: char) (c:char) =
  let byAmt = if by = 'S' then 1 else by - '0' |> int
  match c with 
  | '.' -> '1'
  | x -> x |> int |> (+) byAmt |> char


let rec traceTachyons path = 
  match path.lines with 
  | [] -> path
  | head::tail when tail = [] -> path 
  | head::tail -> 
      let mutable next = tail.Head |> Seq.toArray
      let mutable splitsThisRow = 0
      let rest = tail.Tail
      for i = 0 to head.Length - 1 do
        if head.[i] <> '.' && head.[i] <> '^' then
          if next.[i] = '^' then 
            next.[i - 1] <- next.[i - 1] |> inc (head.[i])
            next.[i + 1] <- next.[i + 1] |> inc (head.[i])
            splitsThisRow <- splitsThisRow + 1
          else
            next.[i] <- if head.[i] = 'S' then '1' else head.[i]

      let traced = traceTachyons { lines = [ next |> String ] @ rest; numSplit = path.numSplit }

      {
        lines = [head] @ traced.lines
        numSplit = traced.numSplit + splitsThisRow
      }

let countExtraLifetimes path =
  let lastLine = path.lines.[path.lines.Length - 1]
  lastLine
  |> Seq.sumBy (
    function 
    | '.' -> 0
    | '^' -> 0
    | x -> x - '0' |> int
  )
