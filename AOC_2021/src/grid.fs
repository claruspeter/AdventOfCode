module AOC2021.Grids

open System 
open AOC2021.Common

type Position = {x:int;y:int}
with 
  static member parse (s:string) =
    match s.Split([|','|]) with
    | [|lhs; rhs|] -> {x=Int32.Parse lhs; y= Int32.Parse rhs}
    | _ -> failwith "Not a Position"
  member this.tuple = (this.x, this.y)

type Grid<'V> = {
  values: (Position * 'V)[]
}
with 
  member this.get (x,y) = 
    this.values 
    |> Array.tryFind (fun a -> (fst a) = {x=x;y=y} ) 
    |> Option.map snd
    |> Option.defaultValue (Unchecked.defaultof<'V>)
  member this.getP (x,y) = ({x=x;y=y}, this.get(x,y))
  member this.set (x,y) v = 
    this.values 
    |> Array.map (fun a -> if (fst a) = {x=x;y=y} then ({x=x;y=y},v) else a)
    |> fun a-> {values = a}
  member this.update (x,y) (f: 'V -> 'V) = 
    this.values 
    |> Array.map (fun a -> if (fst a) = {x=x;y=y} then ({x=x;y=y}, f (snd a)) else a)
    |> fun a -> {values = a}
  member this.xMax = this.values |> Seq.map (fun a -> (fst a).x) |> Seq.max
  member this.yMax = this.values |> Seq.map (fun a -> (fst a).y) |> Seq.max


let parseGrid<'V> (valueParser: Position -> int -> 'V) (lines: string[]) : Grid<'V> = 
  lines 
  |> Array.mapi (fun y line -> 
      line 
      |> Seq.map charInt 
      |> Seq.mapi (fun x v -> ({x=x;y=y}, valueParser {x=x;y=y} v))
      |> Seq.toArray
  )
  |> Array.collect id
  |> fun a -> {values = a }

let neighbours (posn:Position) (heightmap: Grid<'V>) =
  let {x=x; y=y} = posn
  [
    if y > 0 then heightmap.getP(x,y-1) |> Some else None
    if x > 0 then heightmap.getP(x-1,y) |> Some else None
    if y < heightmap.yMax then heightmap.getP(x,y+1) |> Some else None
    if x < heightmap.xMax then heightmap.getP(x+1,y) |> Some else None
  ]
  |> List.choose id

let allNeighbours (posn:Position) (heightmap: Grid<'V>) =
  let {x=x; y=y} = posn
  let diags =
    [|
      if y > 0 && x > 0 then heightmap.getP(x-1,y-1) |> Some else None
      if y > 0 && x < heightmap.xMax then heightmap.getP(x+1,y-1) |> Some else None
      if y < heightmap.yMax && x > 0 then heightmap.getP(x-1,y+1) |> Some else None
      if y < heightmap.yMax && x < heightmap.xMax then heightmap.getP(x+1,y+1) |> Some else None
    |]
    |> Array.choose id
  Array.append diags (neighbours posn heightmap |> List.toArray)

let printHeightMap (formatter: 'V -> string) (heightmap: Grid<'V>) =
  [|0..heightmap.yMax|]
  |> Array.map (fun y -> 
      [|0..heightmap.xMax|]
      |> Array.map (fun x -> heightmap.get(x,y) |> formatter)
      |> fun s -> String.Join("", s )
  )
  |> fun q -> String.Join("\r\n", q)