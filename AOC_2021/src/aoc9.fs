module AOC2021.Day9

open System
open AOC2021.Common

type HeightMap = int[][]

let charInt (c:char) = int c - int '0'

let parseHeightmap (lines: string[]) : HeightMap = 
  lines 
  |> Array.map (Seq.map charInt >> Seq.toArray)

let neighbours (x, y) (heightmap: HeightMap) =
  [
    if y > 0 then Some heightmap.[y - 1].[x] else None
    if x > 0 then Some heightmap.[y].[x - 1] else None
    if y < heightmap.Length - 1 then Some heightmap.[y + 1].[x] else None
    if x < heightmap.[0].Length - 1 then Some heightmap.[y].[x + 1] else None
  ]
  |> List.choose id

let findLowPoints (heightmap: HeightMap) =
  [0..heightmap.Length - 1]
  |> List.collect (fun y -> 
      let line = heightmap.[y]
      [0..line.Length - 1]
      |> List.choose (fun x -> 
          let minAdjacent = neighbours (x,y) heightmap |> Seq.min
          if heightmap.[y].[x] < minAdjacent then
            Some heightmap.[y].[x]
          else 
            None
      )
  )

let riskLevel : int seq -> int =
  Seq.map ((+) 1)
  >> Seq.sum