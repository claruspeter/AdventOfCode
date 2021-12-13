module AOC2021.Day9

open System
open AOC2021.Common
open AOC2021.Grids
open System.Collections.Generic

type HeightValue = {
  value: int
  position: Position
  basin: int option
}

let parseHeightmap (lines: string[]) : Grid<HeightValue> = 
  parseGrid<HeightValue> (fun p v -> {value=v; position=p; basin=None} )lines 

let findLowPoints (heightmap: Grid<HeightValue>) =
  let mutable basin = 0
  [0..heightmap.yMax] |> List.collect (fun y -> 
    [0..heightmap.xMax] |> List.choose (fun x -> 
      let current = heightmap.get(x,y)
      let minAdjacent = neighbours {x=x;y=y} heightmap |> Seq.minBy (fun a -> (snd a).value)
      if current.value < (snd minAdjacent).value then
        basin <- basin + 1
        Some {current with basin=Some basin}
      else 
        None
    )
  )

let riskLevel : int seq -> int =
  Seq.map ((+) 1)
  >> Seq.sum

let rec infectNeighours (heightmap: Grid<HeightValue>) (lowpoint: HeightValue) : Grid<HeightValue> =
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

let infectAdjacent (heightmap: Grid<HeightValue>) (lowpoints: HeightValue list) : Grid<HeightValue> =
  lowpoints
  |> List.fold infectNeighours heightmap