module AOC2021.Day15

open System
open System.Collections.Generic
open AOC2021.Common
open AOC2021.Grids


type AVal = {
  cost: int
  g: int
  f: int
  parent: Position option
  isClosed : bool
  position: Position
}

type Cell = Position * AVal

type SearchParams<'A> = {
  grid: 'A[,]
  opened: Position Set
}

let hDistance position =
  (1000 - position.x) + (1000 - position.y)

let neighbours {x=x; y=y} (grid: AVal[,]) =
  [
    if y > 0 then grid.[x,y-1] |> Some else None
    if x > 0 then grid.[x-1,y] |> Some else None
    if y+1 < (grid.GetLength 0) then grid.[x,y+1] |> Some else None
    if x+1 < (grid.GetLength 1) then grid.[x+1,y] |> Some else None
  ]
  |> List.choose id

let private applyNeighbour (currentVal) (acc: SearchParams<AVal>) (candidate) =
  let notOpen = acc.opened |> Set.contains candidate.position |> not
  let isCostEffective = candidate.g > currentVal.g + candidate.cost
  if notOpen || isCostEffective then
    let newG = currentVal.g + candidate.cost
    let newF = newG + (hDistance candidate.position)
    acc.grid[candidate.position.x, candidate.position.y] <- 
      {
        candidate with 
          g = newG
          f = newF
          parent = Some candidate.position
      }
    let newOpened = if notOpen then acc.opened.Add candidate.position else acc.opened
    { grid=acc.grid; opened=newOpened; }
  else
    acc

let private printIn (data: AVal[,]) =
  [|0..(data.GetLength 1)-1|]
  |> Array.map (fun y -> 
    [|0..(data.GetLength 0)-1|]
    |> Array.map (fun x -> if data[x,y].isClosed then "#" else "." )
    |> fun a -> String.Join("", a)
  )
  |> fun a -> String.Join("\r\n", a)

let rec private findPath  {grid=grid; opened=opened; } (currentVal: AVal) =
  // let map = printIn grid
  // if currentVal.position = {x=0;y=0} then 
  //   printfn "%s" map
  // else
  //   printfn "\x1B[%dA%s" ((grid.GetLength 1) + 1) map 
  // printfn "\x1B[LDistance: %d" (hDistance currentVal.position)

  let unclosedNeighbours = neighbours currentVal.position grid |> List.filter (fun a -> a.isClosed |> not)
  match unclosedNeighbours |> Seq.tryFind (fun v -> v.position = {x=(grid.GetLength 0)-1;y=(grid.GetLength 1)-1}) with 
  | Some final -> 
      grid.[final.position.x,final.position.y] <- {final with g=currentVal.g + final.cost}
      {
        grid=grid
        opened=opened
      }
  | None -> 
      let updated = 
        unclosedNeighbours
        |> List.fold (applyNeighbour currentVal) { grid=grid; opened=opened;}
      updated.grid.[currentVal.position.x, currentVal.position.y] <- {(updated.grid.[currentVal.position.x, currentVal.position.y] ) with isClosed=true}
      let updatedAndClosed = {
        grid=updated.grid
        opened=updated.opened.Remove currentVal.position
      }
      let nextCurrent =
        updatedAndClosed.opened 
        |> Set.toSeq
        |> Seq.map (fun p -> updatedAndClosed.grid[p.x,p.y]) 
        |> Seq.minBy (fun a -> a.f)
      findPath updatedAndClosed nextCurrent

let aStar (grid: Grid<int>) = 
  let initial = 
    Array2D.init 
      (grid.xMax + 1) 
      (grid.yMax + 1)
      (fun x y -> 
        {
          cost=grid.get (x,y)
          parent=None
          f=0
          g=0
          isClosed=false 
          position={x=x;y=y}
        }
      )

  findPath {grid=initial; opened=Set.empty.Add {x=0;y=0}; } (initial.[0,0])
  |> fun a -> a.grid
