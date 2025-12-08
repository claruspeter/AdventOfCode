module AOC2023.Day6

open System
open AOC2023.Common

type CephalopodMath = {
  numbers: int64 list
  operator: char
}

let private transpose (data: 'a list list) =
  let width = data.[0].Length
  let height = data.Length
  List.init width (
    fun x ->
      List.init height (
        fun y -> data.[y].[x]
      )
  )



let parse (lines: string list) = 
  let split = 
    lines
    |> List.map _.Split([|' '|], StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
  let nums = split.[0..split.Length-2]
  let ops = split[split.Length - 1]
  let width = ops.Length
  List.init 
    width
    (fun i -> 
      {
        numbers  = nums |> List.map (fun line -> line.[i] |> Int64.Parse)
        operator = ops.[i].[0] 
      }
    )

let calculate math =
  match math.operator with 
  | '+' -> math.numbers |> List.sum
  | '*' -> math.numbers |> List.reduce ( * )
  | c -> failwithf "Unknown operator: %c" c

let parseVertical (lines: string list) =
  let lineLength = lines.[0].Length
  let opLine = lines.[lines.Length - 1]
  let ops = 
    opLine
    |> Seq.mapi (fun i x -> {| op=x; i=i |})
    |> Seq.filter (fun x -> x.op <> ' ')
    |> Seq.toList

  let nums =
    lines.[0..lines.Length-2]
    |> List.map (
      fun line ->
        [0..ops.Length-1]
        |> List.map (
          fun i -> 
            let a = ops.[i].i
            let b = if i = ops.Length - 1 then lineLength else ops.[i+1].i - 1
            line.Substring(a, b - a)
            // |> Seq.toList
        )
        // |> transpose
    )
    |> transpose
    |> List.map (
      fun column -> 
        column
        |> List.map Seq.toList
        |> transpose
        |> List.map (Join "")
        |> List.map Int64.Parse
    )
  
  ops
  |> List.zip nums
  |> List.map (fun x -> {operator = x |> snd |> _.op; numbers = x |> fst })
  |> List.rev