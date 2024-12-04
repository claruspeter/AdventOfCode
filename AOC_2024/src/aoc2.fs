module AOC2023.Day2

open System
open AOC2023.Common

let safety lines =
  lines
  |> List.map parseIntList
  |> List.map (fun line -> line |> List.pairwise |> List.map (fun (a,b) -> a - b))
  |> List.map ( fun line -> 
    let hasZero = line |> List.exists ( fun x -> x = 0 )
    let hasLarge = line |> List.exists ( fun x -> Math.Abs(x) > 3 )
    let numNeg = line |> List.filter ( fun x -> x < 0 ) |> List.length
    let safe = (not hasZero) && (not hasLarge) && (numNeg = 0 || numNeg = line.Length)
    {| line=line; hasZero=hasZero; hasLarge=hasLarge; numNeg=numNeg; safe=safe  |}
  )
