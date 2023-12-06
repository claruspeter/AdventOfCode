module AOC2023.Day4

open System
open AOC2023.Common

type Card = {
  winning: Set<int>
  numbers: Set<int>
}

let private parseCard (line:string) =
  let sections = line.Split([|':'; '|'|]) |> Seq.map (fun x -> x.Substring(1)) |> Seq.toList
  sections
  |> List.skip 1
  |> List.map (fun section -> section.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) |> Seq.map int)
  |>  function 
      | [ a; b ] -> {winning=a |> set ; numbers=b |> set;} |> Some
      | _ -> None


let private calculateMatches card =
  card.numbers 
  |> Set.intersect card.winning 
  |> Set.count

let private calculatePoints card =
  match calculateMatches card with
  | 0 -> 0
  | n -> Math.Pow(2, float(n - 1)) |> int

let cardPoints lines = 
  lines 
  |> List.choose parseCard
  |> List.map calculatePoints

let rec private addToCounts n amt counts =
  match n, counts with 
  | n, _ when n < 1 -> counts
  | _, [] -> counts
  | n, c::tail ->
      [c + amt] @ (addToCounts (n - 1) amt tail)

let rec private countMultiples counts matches = 
  match counts, matches with 
  | [], [] -> counts
  | [c], [m] -> counts
  | c::cTail , m::mTail ->
      let updatedCounts = addToCounts m c cTail
      [c] @ (countMultiples updatedCounts mTail)
  | _ -> failwith "Impossible"



let totalCardCounts lines =
  lines
    |> List.choose parseCard
    |> List.map calculateMatches
    |> countMultiples (List.replicate lines.Length 1)
