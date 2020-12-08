module AOC7

open System
open FSharp.Scanf.Scanf
open AOC_Common


let inputs = 
    lines 7
    |> Array.toList
  
let n = inputs.Length

type BagPair = {
  inside: string
  count: int
  outside: string
}

let parseBagContents (innerBagDescription:string) =
  let normalised = innerBagDescription.Replace("bags", "bag")
  match normalised.Contains "no other" with 
  | false -> normalised |> sscanf "%d %s bag" |> Some
  | true -> None

let parseBag s =
  let (name, contents) = sscanf "%s bags contain %s." s
  contents.Split(',', StringSplitOptions.TrimEntries )
    |> Array.toList
    |> List.choose parseBagContents
    |> List.map (fun inBag -> {BagPair.outside = name; inside = snd inBag; count = fst inBag})

let parseBags lines = 
  lines
  |> List.collect parseBag

let rec private containedWithinBag bagPairs innerBagName =
  let directParents =
    bagPairs
    |> List.filter (fun b -> b.inside = innerBagName)
    |> List.map (fun b -> b.outside)
  match directParents with 
  | [] -> []
  | parents ->
    let grandParents = 
      parents
      |> List.collect (containedWithinBag bagPairs)
    parents @ grandParents

let containedWithin bagPairs innerBagName =
  containedWithinBag bagPairs innerBagName
  |> List.sort
  |> Set

let rec countInBag bagPairs outerBagName =
  let children =
    bagPairs
    |> List.filter (fun b -> b.outside = outerBagName)
  let childrenCount = 
    children
    |> List.sumBy (fun b -> b.count)
  let grandChildrenCount = 
    children
    |> List.sumBy (fun b -> b.count * (countInBag bagPairs b.inside))
  childrenCount + grandChildrenCount
  



