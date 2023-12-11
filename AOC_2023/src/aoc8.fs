module AOC2023.Day8

open System
open System.Collections.Generic
open AOC2023.Common

type NetworkNode = {
  name: string
  left: string
  right: string
}

type Network = {
  leftRight : char list
  nodes: IDictionary<string,NetworkNode>
}

let private parseNetworkNode line =
  match line with 
  | Regex "([A-Z0-9]{3}) = \\(([A-Z0-9]{3}), ([A-Z0-9]{3})\\)" [_; a; b; c] -> {name=a; left=b; right=c} |> Some
  | _ -> None


let private parseNetwork (lines: string list) =
  let nodes = 
    lines 
    |> List.skip 2
    |> List.choose parseNetworkNode
    |> List.map (fun x -> (x.name, x))
    |> dict
  {
    leftRight = lines.[0] |> Seq.toList
    nodes = nodes
  }

let private branch network currentNode leftRight = 
  match leftRight with 
  | 'L' -> network.nodes.[currentNode.left]
  | 'R' -> network.nodes.[currentNode.right]
  | _ -> failwithf "Impossible! %A" leftRight

let rec private walk currentNodes lrIndex network =
  match currentNodes |> List.map (fun x -> x.name) with 
  | x when x |> List.forall (fun name -> name.EndsWith "Z") -> [x]
  | names -> 
    let nextNodes = currentNodes |> List.map (fun node -> branch network node network.leftRight.[lrIndex])
    let nextLR = (lrIndex + 1) % network.leftRight.Length
    walk nextNodes nextLR network
    |> (@) [names]

let private walkNetwork starters network =
  let startingNodes = network |> starters
  //startingNodes |> List.map (fun x -> x.name) |> printfn "Starting with %A" 
  walk startingNodes 0 network

let starterGhost network = 
  network.nodes 
  |> Seq.filter (fun x -> x.Key.EndsWith "A") 
  |> Seq.map (fun x -> x.Value) 
  |> Seq.toList

let starterSimple network = 
  network.nodes["AAA"]
  |> List.singleton

let walkThePath starters (lines: string list ) =
  lines
  |> parseNetwork 
  |> walkNetwork starters
