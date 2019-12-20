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
        [0..cmd.amount-1]
        |> List.scan (fun state _ -> step cmd.direction state ) pos
        |> List.tail
    zzz

let rec coords pos line  = 
    match line with
    | [] -> []
    | [cmd] -> processCmd pos cmd
    | cmd::tail -> 
        let segment = processCmd pos cmd
        let remaining = coords (segment |> List.last) tail
        segment @ remaining



let line n = coords {x=0; y=0} inputs.[n] //|> List.filter (fun x -> x <> {x=0;y=0})

let intersections set1 set2 = 
    set1 
    |> Set.intersect set2
    |> Set.map (fun v -> {|x=v.x; y=v.y; dist=Math.Abs(v.x+v.y) |})

let print ints = 
    ints 
    |> Set.iter (printfn "%A")

let printC coords = 
    coords 
    |> List.map (sprintf "%O")
    |> List.toArray
    |> fun x -> String.Join("\n", x)


let minDistance line1 line2=
    let set1 = line line1 |> Set.ofList
    let set2 = line line2 |> Set.ofList
    intersections set1 set2
    |> Set.toList
    |> List.map (fun x -> x.dist)
    |> List.min

let minPath index1 index2 = 
    let line1 = line index1 
    let line2 = line index2 
    let intersects = intersections (line1 |> Set.ofList) (line2 |> Set.ofList)
    intersects
    |> Set.toList
    |> List.map (fun i -> 
        {|
            x=i.x
            y=i.y
            step1=(line1 |> List.findIndex (fun c -> c.x=i.x && c.y=i.y)) + 1
            step2=(line2 |> List.findIndex (fun c -> c.x=i.x && c.y=i.y)) + 1
        |}
    )
    |> List.map ( fun x -> x.step1 + x.step2)
    |> List.min
