module AOC2021.Day12

open System
open AOC2021.Common

// type Cave  = {
//   name: string
//   visited: bool
// }
// with 
//   member this.isLarge = ['A'..'Z'] |> List.contains this.name.[0] 
//   member this.isVisitable = not(this.visited) || this.isLarge

let isLarge (s:string) = ['A'..'Z'] |> List.contains s.[0]
let isSmall (s:string) = s |> isLarge |> not

type Tunnel = {
  a: string
  b: string
}

let parseCaves lines =
  lines 
    |> Array.map (fun (line:string) -> 
      match line.Split([|'-'|]) with 
      | [| lhs; rhs |] -> {a=lhs; b=rhs}
      | _ -> failwith "Not a valid line"
    )

let isVisitableOnce (visited: string list) (candidate: string) =
  isLarge candidate
  || not(List.contains candidate visited)

let isVisitableTwice (visited: string list) (candidate: string) =
  match candidate with 
  | x when isLarge x -> true
  | "start" -> false
  | x when not(List.contains x visited) -> true
  | x ->
      visited 
      |> List.filter isSmall 
      |> List.countBy id 
      |> List.map snd 
      |> List.exists ((=) 2)
      |> not



let rec private followPaths (visitable: string list -> string -> bool) (visited: string list) (current: string) caves =
  match current with
  | "end" -> [| visited @ [current] |> List.toArray |> fun x -> String.Join(",", x) |]
  | _ -> 
    let nowVisited = visited @ [current]
    let nextChoices = 
      caves 
      |> Array.collect (fun c -> 
        [|
          if c.a = current then Some c.b else None
          if c.b=current then Some c.a else None
        |] 
        |> Array.choose id
      )
      |> Array.filter (visitable nowVisited)
    nextChoices
    |> Array.collect (fun x -> followPaths visitable nowVisited x caves)
    

let findPaths (caves:Tunnel[]) =
  followPaths isVisitableOnce [] "start" caves
  |> Array.sort

let findPaths2 (caves:Tunnel[]) =
  followPaths isVisitableTwice [] "start" caves
  |> Array.sort