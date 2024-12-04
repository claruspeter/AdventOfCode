module AOC2023.Day2

open System
open AOC2023.Common

let private calcSafety (line: int list) = 
  let diffs = 
    line 
    |> List.pairwise |> List.map (fun (a,b) -> a - b)
  
  let hasZero = diffs |> List.exists ( fun x -> x = 0 )
  let hasLarge = diffs |> List.exists ( fun x -> Math.Abs(x) > 3 )
  let numNeg = diffs |> List.filter ( fun x -> x < 0 ) |> List.length
  let safe = (not hasZero) && (not hasLarge) && (numNeg = 0 || numNeg = diffs.Length)
  {| line=line; hasZero=hasZero; hasLarge=hasLarge; numNeg=numNeg; safe=safe  |}
  

let safety (lines: string list) =
  lines
  |> List.map parseIntList
  |> List.map calcSafety

let private withTolerance (line: int list) =
  Seq.init line.Length id
  |> Seq.map (fun i -> 
    List.removeAt i line
    |> calcSafety
  )
  |> Seq.exists (fun x -> x.safe)

let safetyWithTolerance lines =
  lines
  |> List.map parseIntList
  |> List.map withTolerance
