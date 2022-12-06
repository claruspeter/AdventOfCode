module AOC2022.Day4

open System
open AOC2022.Common

type Assignment = {
  lower: int
  upper:int
}

type AssignmentPair = {
  a: Assignment
  b: Assignment
}

let parseAssignmentPair (line: string) =
  line.Split([|','; '-'|])
  |> Array.map int
  |> fun x -> {a={lower=x.[0]; upper=x.[1]}; b={lower=x.[2]; upper=x.[3]}}

let fullyContained pair =
  (pair.a.lower <= pair.b.lower && pair.a.upper >= pair.b.upper)
  || (pair.b.lower <= pair.a.lower && pair.b.upper >= pair.a.upper)

let overlaps pair =
  pair.a.lower <= pair.b.upper && pair.a.upper >= pair.b.lower