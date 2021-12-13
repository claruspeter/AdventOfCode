module AOC2021.Day11

open System
open AOC2021.Common
open AOC2021.Grids

type Octopus = {
  totalenergy: int
  flasher: bool
  hasFlashed: bool
}
with 
  member this.flashes = this.totalenergy / 10
  member this.energy = this.totalenergy % 10

let parseOctupus (lines: string[]) : Grid<Octopus> = 
  parseGrid (fun p v -> {totalenergy=v; flasher=false; hasFlashed=false }) lines

let flashOctopus octopus =
  if octopus.hasFlashed then 
    octopus
  else
    {
      octopus with 
        totalenergy=octopus.totalenergy+1
        flasher=octopus.flasher || octopus.energy=9
        hasFlashed=octopus.hasFlashed || octopus.energy=9
    }
    
let stepOctopus octopus =
  {
    octopus with 
      totalenergy=octopus.totalenergy+1
      flasher=octopus.energy=9
      hasFlashed=octopus.energy=9
  }

let rec propagateFlash (grid: Grid<Octopus>): Grid<Octopus> =
  // printfn "Propagating..."
  let flashers = 
    grid.values
    |> Array.filter (fun a -> (snd a).flasher)
    |> Array.map fst
    |> Seq.toArray
  // printfn "## %d flashers" (flashers.Length)
  let flashed =
    flashers
    |> Array.collect (fun a -> allNeighbours a grid)
    |> Array.map fst
  // printfn "## %d flashed" (flashed.Length)
  let removeFlashers =
    flashers
    |> Seq.fold (fun (g: Grid<Octopus>) p  -> g.update (p.x, p.y) ( fun v -> {v with flasher=false}) ) grid
  let updated =
    flashed
    |> Seq.fold (fun (g: Grid<Octopus>) p  -> g.update (p.x, p.y) ( fun v -> flashOctopus v) ) removeFlashers
  if updated.values |> Array.exists (fun a -> (snd a).flasher) then 
    propagateFlash updated
  else
    updated

let stepTime (grid: Grid<Octopus>): Grid<Octopus> =
  let inc = 
    {
      values=
        grid.values
        |> Array.map (fun (p,v) -> (p, stepOctopus v) )
    }
  inc 
  |> propagateFlash