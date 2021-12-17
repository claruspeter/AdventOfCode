module AOC2021.Day14

open System
open System.Collections.Generic
open AOC2021.Common

type InsertionRule = {
  pair: char * char
  insert: char
}

let parseInsertionRule (line:string) =
  {
    pair=(line.[0], line.[1])
    insert=line.[6]
  }

let parsePolymerTemplate (lines: string[]) =
  (
    lines.[0], 
    lines
    |> Seq.skip 2
    |> Seq.map parseInsertionRule
  )

let stepPolymer rules (template:string) : string = 
  template
  |> Seq.pairwise
  |> Seq.map (fun (a,b)->
      rules 
      |> Seq.tryFind (fun r -> r.pair = (a,b))
      |> Option.map (fun r -> seq [a; r.insert])
  )
  |> Seq.choose id
  |> Seq.collect id
  |> fun s -> Seq.append s (template.Substring(template.Length - 1)) 
  |> Seq.toArray
  |> String

type Polymer = {
  pairs: ((char*char) * int64) list
  elements: (char * int64) list
}
with 
  member this.numPair (s:string) = 
    this.pairs 
    |> List.tryFind (fun x -> (fst x) = (s.[0], s.[1]))
    |> Option.map snd
    |> Option.defaultValue 0L
  member this.numElement (c:char) = 
    this.elements
    |> List.tryFind (fun x -> (fst x) = c)
    |> Option.map snd
    |> Option.defaultValue 0L
  member this.length = this.elements |> Seq.map snd |> Seq.sum

let initPolymer (template: string) =
  let pairs = 
    template
    |> Seq.pairwise
    |> Seq.countBy id
    |> Seq.map (fun (a, b) -> (a, int64 b))
  let elements = 
    template
    |> Seq.countBy id
    |> Seq.map (fun (a, b) -> (a, int64 b))
  {pairs = pairs |> Seq.toList; elements = elements |> Seq.toList}

let private ensurePair x pairs = 
  if pairs |> List.exists ( fun p -> (fst p) = x) 
  then pairs 
  else pairs @ [(x, 0L)]

let applyRuleToPolymer (polymer:Polymer) (rule: InsertionRule) (numPairsSplit: int64) =
  let prevPair = (rule.pair |> fst, rule.insert)
  let nextPair = (rule.insert, rule.pair |> snd)
  let updatedPairs = 
    polymer.pairs
    |> ensurePair prevPair
    |> ensurePair nextPair
    |> List.map (fun x -> if (fst x) = rule.pair then (fst x, (snd x) - numPairsSplit) else x)
    |> List.map (fun x -> if (fst x) = prevPair then (fst x, (snd x) + numPairsSplit) else x)
    |> List.map (fun x -> if (fst x) = nextPair then (fst x, (snd x) + numPairsSplit) else x)
  let updatedElements = 
    polymer.elements
    |> ensurePair rule.insert
    |> List.map (fun x -> if (fst x) = rule.insert then (fst x, (snd x) + numPairsSplit) else x)
  {pairs = updatedPairs; elements = updatedElements}


let stepPolymer2 rules (polymer:Polymer) : Polymer =
  rules
  |> Seq.choose ( fun r -> 
      polymer.pairs 
      |> Seq.tryFind (fun p -> (fst p) = r.pair && (snd p) > 0L) 
      |> Option.map (fun p ->  (r, (snd p)))
    )
  |> Seq.fold (fun acc (rule, num) -> applyRuleToPolymer acc rule num) polymer