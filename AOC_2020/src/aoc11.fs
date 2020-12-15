module AOC11

open AOC_Common
open System.Collections.Generic

let inputs = 
    lines 11
    |> Array.toList

let  n = inputs.Length

type Seat = Floor | Empty | Occupied
type Point = {x : int; y: int}

let left pt = {x=pt.x-1; y=pt.y}
let right pt = {x=pt.x+1; y=pt.y}
let up pt = {x=pt.x; y=pt.y-1}
let down pt = {x=pt.x; y=pt.y+1}

type SeatingPlan = {
  width: int
  height: int
  seats: Map<Point,Seat>
}

let parseSeat =
  function
  | '.' -> Floor
  | 'L' -> Empty
  | '#' -> Occupied
  | x -> failwithf "Unknown seat type %A" x

let asSeating (lines: string list) =
  //also surround with floor so we can do lookups without boundary conditions
  let floorline = String.init (lines.[0].Length + 2) (fun i -> ".")
  lines 
  |> List.map (fun line -> "." + line + ".")
  |> fun lines -> [floorline] @ lines @ [floorline]
  |> List.map (fun line -> line |> Seq.map parseSeat |> Seq.toList)
  |> List.mapi (fun rowIndex row ->
    row |> List.mapi (fun colIndex cell ->
      ({x=colIndex; y=rowIndex}, cell)
    )
  )
  |> List.collect id
  |> Map
  |> fun seats -> {
    width=floorline.Length
    height=lines.Length + 2
    seats=seats
  }

let unmap (map: IDictionary<_,_>) =
  map
  |> Seq.map (fun kv -> (kv.Key, kv.Value))

let numSeated seating =
  seating.seats
  |> unmap
  |> Seq.filter (fun (pt, s) -> s = Occupied)
  |> Seq.length

let getSurrounding seating pt  =
    [
      pt |> up |> left
      pt |> up
      pt |> up |> right
      pt |> left
      pt |> right
      pt |> down |> left
      pt |> down
      pt |> down |> right
    ]
    |> List.map (fun at -> seating.seats.[at])


let getSeated (seating: SeatingPlan) =
  let newSeats =
    seating.seats
    |> unmap
    |> Seq.map (
        function
        | pt, Empty -> 
          let numSurroundingOccupied =
            getSurrounding seating pt
            |> List.filter ((=) Occupied)
            |> List.length
          if numSurroundingOccupied = 0 then
            pt, Occupied
          else
            pt, Empty
        | pt, Occupied -> 
          let numSurroundingOccupied =
            getSurrounding seating pt
            |> List.filter ((=) Occupied)
            |> List.length
          if numSurroundingOccupied >= 4 then
            pt, Empty
          else
            pt, Occupied
        | pt, Floor -> pt, Floor
    )
  {seating with seats = newSeats |> Map}

let rec findStable seating =
  let reseated = seating |> getSeated
  match (numSeated seating) = (numSeated reseated) with
  | true -> reseated
  | false -> findStable reseated