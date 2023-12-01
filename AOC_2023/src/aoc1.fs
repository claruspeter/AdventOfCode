module AOC2023.Day1

open System
open AOC2023.Common

let calibrationValue (line:string) =
  line
  |> Seq.choose CharInt
  |> Seq.toList
  |>  function
      | [] -> "00"
      | x -> sprintf "%d%d" (List.head x) (List.last x)
  |> int

let private words = 
  [
    ("one", 1)
    ("two", 2)
    ("three", 3)
    ("four", 4)
    ("five", 5)
    ("six", 6)
    ("seven", 7)
    ("eight", 8)
    ("nine", 9)
    ("1", 1)
    ("2", 2)
    ("3", 3)
    ("4", 4)
    ("5", 5)
    ("6", 6)
    ("7", 7)
    ("8", 8)
    ("9", 9)
  ]
  |> dict

type WordFound = {
  word: string
  at: int
}with 
  member this.value = words.[this.word]

let private findAllInstancesIn (input: string) (pattern: string) =
  match input.IndexOf(pattern), input.LastIndexOf(pattern) with 
  | first, _ when first < 0 -> []
  | first, last when first = last -> [{word=pattern; at=first}]
  | first, last -> [{word=pattern; at=first}; {word=pattern; at=last}]

let calibrationValueWithWords (line:string) =
  words.Keys
  |> Seq.toList
  |> List.collect (findAllInstancesIn line)
  |> List.sortBy (fun x -> x.at)
  |> fun found ->
      let first = found |> List.head
      let last = found |> List.last
      sprintf "%d%d" first.value last.value
      |> int
