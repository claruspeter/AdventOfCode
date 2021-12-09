module AOC2021.Day8

open System
open AOC2021.Common

type DisplayLine = {
  input: string[]
  output: string[]
}

let parse7DigitDisplayLine (s: string) =
  let [| lhs; rhs |] = s.Split([| '|' |]) |> Array.take 2
  {
    input = lhs.Split([| ' '|], StringSplitOptions.RemoveEmptyEntries)
    output = rhs.Split([| ' '|], StringSplitOptions.RemoveEmptyEntries)
  }

type Digit = D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 | D0
type ClassifiedDigit = {
  digit: Digit
  combination: string
}

let classify (s: string) =
  match s.Length with 
  | 2 -> {digit=D1; combination=s} |> Some
  | 3 -> {digit=D7; combination=s} |> Some
  | 4 -> {digit=D4; combination=s} |> Some
  | 7 -> {digit=D8; combination=s} |> Some
  | _ -> None