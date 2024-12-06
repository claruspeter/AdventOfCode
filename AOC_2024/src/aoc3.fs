module AOC2023.Day3

open System
open AOC2023.Common

let private _do = "do()"
let private _dont = "don't()"

let extractMuls (line:string) = 
  match line with 
  | Regex @"mul\((\d+),(\d+)\)" x -> 
    x
    |> List.chunkBySize 3
    |> List.map (fun x -> (x.[1] |> int, x.[2] |> int))
  | _ -> []

let calcMul (a,b) = a*b

type Token =
  | Mul of a:int * b:int
  | Do
  | Dont

let tokenise line =
  match line with 
  | RegexMatch @"(mul\(\d+,\d+\))|(do\(\))|(don't\(\))" x ->
    x 
    |> List.choose (
      function 
      | Regex @"mul\((\d+),(\d+)\)" m when m.Length = 3 -> 
          m
          |> fun x -> (x.[1] |> int, x.[2] |> int) |> Mul
          |> Some
      | "do()" -> Some Do
      | "don't()" -> Some Dont
      | _ -> None
    )
  | _ -> []

type Processor = { processing: bool; data:(int*int) list}

let maskMuls tokens = 
  let result = 
    tokens 
    |> List.fold (fun (acc:Processor) x -> 
      match acc.processing, x with 
      | _, Do -> { acc with processing=true }
      | _, Dont -> { acc with processing=false }
      | true, Mul (a,b) -> {acc with data = acc.data @ [(a,b)]}
      | false, Mul (a,b) -> acc
    ) { Processor.processing=true; data=[] }
  result.data