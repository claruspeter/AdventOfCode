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

let ruleFails (update: List<int>)  (rule:Rule) =
    match update |> List.tryFindIndex (fun x -> x=rule.before), update |> List.tryFindIndex (fun x -> x=rule.after) with 
    | None, _
    | _, None -> false
    | Some b, Some a -> b >= a

let obeysRules (rules:Rule list) (update: List<int>) =
  rules
  |> List.exists (ruleFails update)  |> not

let middle (update: int list) =
  update.[update.Length / 2]

let rec reorder n (rules:Rule list) (update: List<int>) =
  if n > 100 then 
    printfn "FAILE!!"
    update
  else
    match rules |> List.filter (ruleFails update) with 
    | [] -> 
        printfn "ROUND: %d" n
        update
    | fails ->
        fails
        |> List.fold (fun acc rule -> acc |> swapByValue rule.before rule.after) update
        |> reorder (n+1) rules

