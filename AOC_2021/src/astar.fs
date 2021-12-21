module AOC2021.AStar_Day15

open System
open System.Collections.Generic
open AOC2021.Common
open AOC2021.Grids

type AVal = {
  cost: int
  g: int
  f: int
  parent: Position option
}

type Cell = Position * AVal

type Grid<'A> with 
  member this.hDistance (posn: Position) = (this.xMax - posn.x) + (this.yMax - posn.y)

type SearchParams<'A> = {
  grid: Grid<'A>
  opened: Position Set
  closed: Position Set
}

let private applyNeighbour (currentPos, currentVal) (acc: SearchParams<AVal>) (posn, candidate) =
  let notOpen = acc.opened |> Set.contains posn |> not
  let isCostEffective = candidate.g > currentVal.g + candidate.cost
  if notOpen || isCostEffective then
    let newGrid =
      let newG = currentVal.g + candidate.cost
      let newF = newG + (acc.grid.hDistance posn)
      acc.grid.set (posn.x, posn.y) {
        candidate with 
          g = newG
          f = newF
          parent = Some currentPos
      }
    let newOpened = if notOpen then acc.opened.Add posn else acc.opened
    { grid=newGrid; opened=newOpened; closed=acc.closed }
  else
    acc

let private printIn xMax yMax data1 data2 =
  [|0..yMax|]
  |> Array.map (fun y -> 
    [|0..xMax|]
    |> Array.map (fun x -> if data2 |> Set.contains ({x=x;y=y}) then "0" else if data1 |> Set.contains ({x=x;y=y}) then "#" else "." )
    |> fun a -> String.Join("", a)
  )
  |> fun a -> String.Join("\r\n", a)

let rec private findPath  {grid=grid; opened=opened; closed=closed} (currentPos:Position, currentVal: AVal) =
  let map = printIn grid.xMax grid.yMax closed opened
  if currentPos = {x=0;y=0} then 
    printfn "%s" map
  else
    printfn "\x1B[%dA%s" (grid.yMax + 1) map 
  printfn "Closed: %d  Open: %d " (closed.Count) (opened.Count)

  let unclosedNeighbours = neighbours currentPos grid |> List.filter (fun (p,a) -> closed |> Set.contains p |> not)
  match unclosedNeighbours |> Seq.tryFind (fun (p,_) -> p = {x=grid.xMax;y=grid.yMax}) with 
  | Some (posn, final) -> 
      {
        grid=grid.set (posn.x,posn.y) {final with g=currentVal.g + final.cost}
        opened=opened
        closed=closed
      }
  | None -> 
      let updated = 
        unclosedNeighbours
        |> List.fold (applyNeighbour (currentPos, currentVal)) { grid=grid; opened=opened; closed=closed }
      let updatedAndClosed = {
        grid=updated.grid
        closed=updated.closed.Add currentPos
        opened=updated.opened.Remove currentPos
      }
      let nextCurrent =
        updatedAndClosed.opened 
        |> Set.toSeq
        |> Seq.map (fun p -> updatedAndClosed.grid.getP (p.x,p.y)) 
        |> Seq.minBy (fun (p,a) -> a.f)
      findPath updatedAndClosed nextCurrent

let aStar (grid: Grid<int>) = 
  let initial = 
    grid.values
    |> Array.map (fun (p,cost) -> (p,{cost=cost; parent=None; f=0; g=0 }))
    |> fun x -> {values = x}
  findPath {grid=initial; opened=Set.empty.Add {x=0;y=0}; closed=Set.empty} (initial.getP (0,0))
  |> fun a -> a.grid

