module AOC2023.Day8

open System
open AOC2023.Common

type JunctionBox = {
  x: int32
  y: int32
  z: int32
}

type BoxPair = {
  a: JunctionBox
  b: JunctionBox
  distance: int32
}

type Circuit = {
  boxes: JunctionBox Set
}with 
  member this.size = this.boxes.Count

let private IsConnected (pair: BoxPair) (boxes: JunctionBox Set)  =
  match boxes |> Set.contains pair.a, boxes |> Set.contains pair.b with 
  | false, false -> false
  | true, false -> true
  | false, true -> true
  | true, true -> true

let private IsContained (pair: BoxPair) (boxes: JunctionBox Set)  =
  match boxes |> Set.contains pair.a, boxes |> Set.contains pair.b with 
  | false, false -> false
  | true, false -> false
  | false, true -> false
  | true, true -> true

let private distance a b =
  if a = b then 
    Int32.MaxValue
  else
    let xd = b.x - a.x |> double
    let yd = b.y - a.y |> double
    let zd = b.z - a.z |> double
    xd * xd + yd * yd + zd * zd
    |> Math.Sqrt
    |> int32



let parse (lines: string list) =
  lines
  |> List.map (
    fun line -> 
      line.Split(',')
      |> Array.map Int32.Parse
      |> fun parts -> {x=parts.[0]; y=parts.[1]; z=parts.[2]}
  )

let rec calculateDistances (boxes: JunctionBox list) =
  match boxes with 
  | [] -> []
  | head::tail ->
    tail 
    |> List.map (
      fun box ->
        let d = distance head box 
        { a = head; b = box; distance = d}
    )
    |> (@) (calculateDistances tail)

let connect n (boxes: JunctionBox list) (pairs: BoxPair list) =
  let initial = boxes |> List.map (fun x -> { boxes = set [ x ] })

  pairs
  |> List.take n
  |> List.fold (
    fun (circuits: Circuit list) pair ->
      let touching = 
        circuits 
        |> List.filter ( fun c -> c.boxes.Contains pair.a || c.boxes.Contains pair.b )
      let notTouching = 
        circuits 
        |> List.except touching
      notTouching @ [{boxes = touching |> List.map _.boxes |> Set.unionMany }]
  ) initial