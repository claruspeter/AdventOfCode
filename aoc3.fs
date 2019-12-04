module AOC3

open System
open System.IO
open AOC_Common


type Direction = R | L | U | D

type Cmd = { direction: Direction; amount: int }

let save nm points =
    File.WriteAllLines (
        (nm + ".csv"),
        points
        |> List.map (fun x -> x.ToString())
    )


let parseCommands (values:string[]) =
    values
    |> Array.map ( fun v ->
        let dirn =
            match v.[0] with
            | 'R' -> R
            | 'L' -> L
            | 'U' -> U
            | 'D' -> D
            | x -> failwithf "Unknown direction: %A from %A" x v
        { direction = dirn; amount = v.Substring(1) |> Int32.Parse}
    )


let inputs = 
    (raw 3).Split([|'\r'; '\n'|], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun line ->
        line.Split([|','|], System.StringSplitOptions.RemoveEmptyEntries)
        |> parseCommands
        |> Array.toList
    )
    
type Coord = { x: int; y: int}
    with override this.ToString() = sprintf "%3i, %3i" this.x this.y

let step dirn pos =
    match dirn with 
    | R -> {pos with x = pos.x + 1}
    | L -> {pos with x = pos.x - 1}
    | U -> {pos with y = pos.y + 1}
    | D -> {pos with y = pos.y - 1}

let processCmd pos cmd =
    let zzz = 
        [0..cmd.amount]
        |> List.scan (fun state _ -> step cmd.direction state ) pos
    zzz

let rec coords pos line  = 
    match line with
    | [] -> []
    | [cmd] -> processCmd pos cmd
    | cmd::tail -> 
        let segment = processCmd pos cmd
        let remaining = coords (segment |> List.last) tail
        segment @ remaining



let line1 = coords {x=0; y=0} inputs.[0] |> List.filter (fun x -> x <> {x=0;y=0})
let line2 = coords {x=0; y=0} inputs.[1] |> List.filter (fun x -> x <> {x=0;y=0})

let intersections = 
    let set1 = line1 |> Set.ofList
    let set2 = line2 |> Set.ofList
    set1 
    |> Set.intersect set2
    |> Set.map (fun v -> {|x=v.x; y=v.y; dist=Math.Abs(v.x+v.y) |})

let minDistance =
    intersections
    |> Set.toList
    |> List.map (fun x -> x.dist)
    |> List.min

