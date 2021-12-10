module AOC2021.Day9

open System
open AOC2021.Common
open System.Collections.Generic

type HeightPosition = {x:int;y:int}

type HeightValue = {
  value: int
  position: HeightPosition
  basin: int option
}

type HeightMap = {
  values: (HeightPosition * HeightValue)[]
}
with 
  member this.get (x,y)   = 
    this.values 
    |> Array.find (fun a -> (fst a) = {x=x;y=y} ) 
    |> snd
  member this.set (x,y) v = 
    this.values 
    |> Array.map (fun a -> if (fst a) = {x=x;y=y} then ({x=x;y=y},v) else a)
    |> fun a-> {values = a}
  member this.xMax = this.values |> Seq.map (fun a -> (fst a).x) |> Seq.max
  member this.yMax = this.values |> Seq.map (fun a -> (fst a).y) |> Seq.max

let charInt (c:char) = int c - int '0'

let parseHeightmap (lines: string[]) : HeightMap = 
  lines 
  |> Array.mapi (fun y line -> 
      line 
      |> Seq.map charInt 
      |> Seq.mapi (fun x v -> ({x=x;y=y}, {value=v; position={x=x;y=y}; basin=None}))
      |> Seq.toArray
  )
  |> Array.collect id
  |> fun a -> {values = a }

let neighbours (x, y) (heightmap: HeightMap) =
  [
    if y > 0 then heightmap.get(x,y-1) |> Some else None
    if x > 0 then heightmap.get(x-1,y) |> Some else None
    if y < heightmap.yMax then heightmap.get(x,y+1) |> Some else None
    if x < heightmap.xMax then heightmap.get(x+1,y) |> Some else None
  ]
  |> List.choose id

let findLowPoints (heightmap: HeightMap) =
  let mutable basin = 0
  [0..heightmap.yMax] |> List.collect (fun y -> 
    [0..heightmap.xMax] |> List.choose (fun x -> 
      let current = heightmap.get(x,y)
      let minAdjacent = neighbours (x,y) heightmap |> Seq.minBy (fun a -> a.value)
      if current.value < minAdjacent.value then
        basin <- basin + 1
        Some {current with basin=Some basin}
      else 
        None
    )
  )

let riskLevel : int seq -> int =
  Seq.map ((+) 1)
  >> Seq.sum

let printHeightMap (formatter: HeightValue -> string) (heightmap: HeightMap) =
  [|0..heightmap.yMax|]
  |> Array.map (fun y -> 
      [|0..heightmap.xMax|]
      |> Array.map (fun x -> heightmap.get(x,y) |> formatter)
      |> fun s -> String.Join("", s )
  )

let rec infectNeighours (heightmap: HeightMap) (lowpoint: HeightValue) : HeightMap =
  let posn = lowpoint.position
  if 
    posn.x > heightmap.xMax
    || posn.y > heightmap.yMax
    || posn.x < 0
    || posn.y < 0
  then
    // printfn "## Hit edge at %A" posn
    heightmap
  else
    let current = heightmap.get(posn.x,posn.y)
    if current.value >= 9 || current.basin.IsSome then
      // printfn "## Hit wall at %A (%A)" posn current
      heightmap
    else
      // printfn "## Updating at %A (%A) to %d" posn current.value lowpoint.basin.Value
      {current with basin = lowpoint.basin}
      |> heightmap.set(posn.x,posn.y)
      |> fun hm -> infectNeighours hm {lowpoint with position={x=posn.x-1;y=posn.y}}
      |> fun hm -> infectNeighours hm {lowpoint with position={x=posn.x+1;y=posn.y}}
      |> fun hm -> infectNeighours hm {lowpoint with position={x=posn.x;y=posn.y-1}}
      |> fun hm -> infectNeighours hm {lowpoint with position={x=posn.x;y=posn.y+1}}

let infectAdjacent (heightmap: HeightMap) (lowpoints: HeightValue list) : HeightMap =
  lowpoints
  |> List.fold infectNeighours heightmap