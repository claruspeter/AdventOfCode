module AOC2023.Day5

open System
open AOC2023.Common

type Rule = {
  before: int
  after: int
}

let parse (lines: string list) = 
  let rules = 
    lines
    |> List.filter (fun x -> x.Contains('|'))
    |> List.map (fun x -> x.Split('|') |> fun p -> {before=int p.[0]; after=int p.[1]}) 
  let updates =
    lines
    |> List.filter (fun x -> x.Contains(','))
    |> List.map (fun x -> x.Split(',') |> Array.map int |> Array.toList) 
  rules, updates

let obeysRules (rules:Rule list) (update: List<int>) =
  rules
  |> List.exists (fun rule -> 
    match update |> List.tryFindIndex (fun x -> x=rule.before), update |> List.tryFindIndex (fun x -> x=rule.after) with 
    | None, _
    | _, None -> false
    | Some b, Some a -> b >= a
  )
  |> not

let middle (update: int list) =
  update.[update.Length / 2]