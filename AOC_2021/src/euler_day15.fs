module AOC2021.Euler_Day15

open System
open System.Collections.Generic
open AOC2021.Common
open AOC2021.Grids

type VisitedValue = {
  value: int
  distance: int
}
with member this.isVisited = this.distance <> Int32.MaxValue

type Cell = Position * int



let pathcost (path: Cell list) = 
  path 
  |> List.sumBy snd

let printPath (path: Cell list) = 
  path
  |> List.map ( fun (pos, _) -> sprintf "%d,%d" pos.x pos.y)
  |> List.toArray
  |> fun x -> String.Join( " - ", x)

let inc (acc:Grid<VisitedValue>) at =
  let value = acc.get at
  let nb = 
    neighbours {x=fst at; y=snd at} acc
    |> List.filter (fun (p,a) -> a.isVisited )
  match nb with 
  | [] -> acc
  | nb -> 
    let minDistance = 
      nb
      |> List.map (fun (p, a) -> a.distance )
      |> List.min
    let candidateDistance = value.value + minDistance
    acc.set at {value with distance=if candidateDistance < value.distance then candidateDistance else value.distance}



let traverse (grid: Grid<int>) =
  let visitedGrid = {
    values = 
      grid.values 
      |> Array.map (fun (p, n) -> 
          match p with 
          | {x=0;y=0} ->  (p, {value=n; distance=0 } )
          | _ ->  (p, {value=n; distance=Int32.MaxValue } )
      )
  }

  let indexes = 
    [0..grid.xMax] 
    |> Seq.collect (fun x -> 
        [0..grid.yMax] 
        |> Seq.map (fun y -> (x,y))
      )
    |> Seq.sortBy (fun (x,y) -> x + y )
    |> logseq
  indexes
    |> Seq.fold inc visitedGrid

let euler (grid: Grid<int>) =
  let size = grid.xMax + 1
  let costs = grid.values |> dict |> Dictionary<Position, int>
  costs[{x=0;y=0}] <- 0
  for i in [size-2..-1..0] do
    costs.[{x=size-1; y=i}] <- costs.[{x=size-1; y=i}] + costs.[{x=size-1; y=i+1}]
    costs.[{x=i; y=size-1}] <- costs.[{x=i; y=size-1}] + costs.[{x=i+1; y=size-1}]
  for i in [size-2..-1..0] do
    for j in [size-2..-1..0] do
      costs.[{x=i; y=j}] <- costs.[{x=i; y=j}] + (min costs.[{x=i+1; y=j}] costs.[{x=i; y=j+1}])
  costs
  |> Seq.toArray
  |> Seq.map (fun kv -> (kv.Key, kv.Value))
  |> fun a -> {values = a |> Seq.toArray}