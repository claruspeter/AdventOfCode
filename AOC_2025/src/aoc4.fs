module AOC2023.Day4

open System
open System.Collections.Generic
open AOC2023.Common

type Point = {
  x: int
  y: int
}
type Cell = {
  at: Point
  c: char
  numNeighbours: int
}with 
  static member empty = {at={x= -1; y= -1}; c=' '; numNeighbours=0}
  override this.ToString (): string = 
      match this.c with 
      | '@' -> this.numNeighbours.ToString()
      | x -> x.ToString()
  member this.isRemovable = this.c = '@' && this.numNeighbours < 4
  member this.isRemoved = this.c = 'x'

type private IndexedCells = {
  xMax: int
  yMax: int
  lookup: IDictionary<Point,Cell>
}

let private safeGetCell (indexed: IndexedCells) x y =
  indexed.lookup.OrDefault Cell.empty {y=y; x=x}

let private countNeighbours (indexed: IndexedCells) pt =
    [
      safeGetCell indexed (pt.x-1) (pt.y-1)
      safeGetCell indexed (pt.x)   (pt.y-1)
      safeGetCell indexed (pt.x+1) (pt.y-1)
      safeGetCell indexed (pt.x-1) (pt.y)
      safeGetCell indexed (pt.x+1) (pt.y)
      safeGetCell indexed (pt.x-1) (pt.y+1)
      safeGetCell indexed (pt.x)   (pt.y+1)
      safeGetCell indexed (pt.x+1) (pt.y+1)
    ]
    |> List.filter (fun cell -> cell.c = '@')
    |> List.length

let asGridString xMax yMax (cells: Cell list) =
  cells
  |> Seq.sortBy (fun c -> c.at.y * xMax + c.at.x)
  |> Seq.map _.ToString()
  |> Seq.chunkBySize xMax
  |> Seq.map (Join "")
  |> Join "\n"
  |> (+) "\n"

let private indexCells  xMax yMax (cells: Cell list) =
  {
    xMax=xMax
    yMax=yMax
    lookup=
      cells
      |> Seq.map (fun cell -> (cell.at, cell))
      |> dict
  }

let countAdjacentRolls (cells: Cell list) =
  let xMax = cells |> List.map _.at.x |> List.max
  let yMax = cells |> List.map _.at.y |> List.max
  let indexed = indexCells xMax yMax cells
  cells 
  |> List.map (
    fun cell -> 
      {cell with 
        numNeighbours = countNeighbours indexed cell.at 
        c = if cell.c = 'x' then '.' else cell.c
      }
  )

let parse (lines: string list) = 
  lines
    |> List.mapi (
      fun y line ->
        line
        |> Seq.mapi (
          fun x c -> 
            {
              at={x=x; y=y}
              c=c
              numNeighbours = 0
            }
        )
    )
    |> List.collect Seq.toList
  

let removeRoll (cell: Cell) = if cell.isRemovable then {cell with c = 'x'} else cell

let rec removeUntilImpossible (phase: Cell list) =
    let next = 
      phase
      |> countAdjacentRolls
      |> List.map removeRoll
    match next |> List.filter _.isRemoved |> _.Length with
    | 0 -> []
    | n -> [n] @ (removeUntilImpossible next)