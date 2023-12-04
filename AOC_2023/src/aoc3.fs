module AOC2023.Day3

open System
open AOC2023.Common

type private Symbol = {x:int; y:int; v:char }
type private Number = {
  y: int;
  x1: int;
  x2: int;
  v: int;
}

let private findSymbols (lines: string list) = 
  lines
  |> List.indexed
  |> List.collect ( fun (y, line) ->
      line
      |> Seq.indexed
      |> Seq.choose ( fun (x, c) -> 
          match c with 
          | c when c <> '.' && not (['0'..'9'] |> Seq.contains c) -> Some {x=x; y=y; v=c}
          | _ -> None
        )
      |> Seq.toList
    )

let private updateNumberSoFar x y numbers v =
  match numbers |> List.tryFind (fun a -> a.y=y && a.x2=x-1) with 
  | Some numberSoFar -> 
      let was = numberSoFar.v
      let updated = was * 10 + v
      let updatedNumber = {numberSoFar with v = updated; x2=x}
      let inserted = numbers |> List.map (fun a -> if a.y=y && a.x2=x-1 then updatedNumber else a)
      // printfn "UPDATING at (%d,%d) was:(%d-%d,%d)=%d <- %d" x y numberSoFar.x1 numberSoFar.x2 numberSoFar.y was updated
      inserted
  | None ->
      // printfn "ADDING at (%d,%d) %d" x y v
      numbers @ [{x1=x; x2=x; y=y; v=v}]

let rec private findNumbersInLine y (numbers: Number list) (line: (int * char) list) : Number list =
  match line with 
  | [] -> numbers
  | [(x, v)] when ['0'..'9'] |> Seq.contains v -> updateNumberSoFar x y numbers (charInt v)
  | (x, v)::tail when ['0'..'9'] |> Seq.contains v ->
      let updated = updateNumberSoFar x y numbers (charInt v)
      findNumbersInLine y updated tail
  | head::tail -> findNumbersInLine y numbers tail
  | _ -> numbers

let private findNumbers (lines: string list) = 
  lines
  |> List.indexed
  |> List.collect (fun (y,line) -> line |> Seq.indexed |> Seq.toList |> findNumbersInLine y [] )

let private isAdjacentTo (symbols:Symbol list) (number:Number) =
  symbols
  |> Seq.exists ( fun symbol -> 
      symbol.y >= number.y - 1
      && symbol.y <= number.y + 1
      && symbol.x >= number.x1 - 1
      && symbol.x <= number.x2 + 1
    )

let partNumbers lines =
  let symbols = lines |> findSymbols
  let numbers = lines |> findNumbers
  let result = 
    numbers
    |> List.filter (isAdjacentTo symbols)

  result |> List.map (fun x -> x.v)