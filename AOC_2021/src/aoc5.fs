module AOC2021.Day5

open System
open AOC2021.Common

type Point = { x:int; y: int}

type Vent = {
  a: Point
  b: Point
} 


type VentPoint = {
  at: Point
  score: int
}

let parseVent (s: string) =
  match s with 
  | Regex @"^(\d+),(\d+) -> (\d+),(\d+)$" [_; x1; y1; x2; y2] -> 
      {
        a={
          x=Int32.Parse(x1)
          y=Int32.Parse(y1)
        }
        b={
          x=Int32.Parse(x2)
          y=Int32.Parse(y2)
        }
      }
  | _ -> failwithf "Failed to parse vent: %A:" s

let private mapVent (vent:Vent) =
  let dx = Math.Sign( vent.b.x - vent.a.x )
  let dy = Math.Sign( vent.b.y - vent.a.y )
  let nsteps = Math.Max(Math.Abs(vent.b.x - vent.a.x), Math.Abs(vent.b.y - vent.a.y))
  [0..nsteps]
  |> List.map (fun i -> 
      {
        x = vent.a.x + dx * i
        y = vent.a.y + dy * i
      }
  )

let mapVents =
  List.collect mapVent
  >> List.groupBy id
  >> List.map (fun (p, n) -> {at=p; score=n.Length})
  
let private nums = ['0'; '1'; '2'; '3'; '4'; '5'; '6' ;'7'; '8'; '9']
let logmap xmax ymax (ventPoints: VentPoint list) =
  let empty = [0..ymax-1] |> List.map (fun _ -> String.init xmax (fun _ -> "."))
  let filled = 
    ventPoints 
    |> List.fold (fun map pt -> 
        map
        |> List.mapi (fun i (line:string) -> 
          if pt.at.y = i then
            line 
            |> Seq.mapi (fun x c -> if x = pt.at.x then nums.[pt.score] else c)
            |> Seq.toArray
            |> String
          else
            line
        )
      ) empty
  logseq filled |> Seq.length |> printfn "%A"
  ventPoints
