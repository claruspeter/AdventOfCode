module AOC2023.Day8

open System
open System.Collections.Generic
open AOC2023.Common

type private NetworkNode = {
  name: string
  left: string
  right: string
}

type private Network = {
  leftRight : char list
  nodes: IDictionary<string,NetworkNode>
}

let private parseNetworkNode line =
  match line with 
  | Regex "([A-Z]{3}) = \\(([A-Z]{3}), ([A-Z]{3})\\)" [_; a; b; c] -> {name=a; left=b; right=c} |> Some
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

let rec private walk currentNode lrIndex network =
  match currentNode.name with 
  | "ZZZ" -> ["ZZZ"]
  | name -> 
    let nextNode = branch network currentNode network.leftRight.[lrIndex]
    walk nextNode ((lrIndex + 1) % network.leftRight.Length) network
    |> (@) [name]

let private walkNetwork network =
  walk (network.nodes.["AAA"]) 0 network

let walkThePath (lines: string list ) =
  lines
  |> parseNetwork 
  |> walkNetwork
