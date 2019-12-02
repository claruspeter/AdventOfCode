module AOC1

open System 
open System.IO

let parser x = 
    try
        Some (Int32.Parse x)
    with
    | _ -> None

let raw = File.ReadAllText("data/aoc1_input.txt").Split([|'\r'; '\n'|])

let fuelFor weight = 
    let a = weight / 3
    a - 2

let rec additional input =
    let fuel = fuelFor input
    if fuel <= 0 then
        0
    else
        let result = fuel + additional fuel
        printfn "-- %d " result
        result

let data = 
    raw
    |> Array.choose (fun x -> parser x)
    |> Array.map fuelFor
    |> Array.sum

let data_b = 
    raw
    |> Array.choose (fun x -> parser x)
    |> Array.map fuelFor
    |> Array.map (fun x -> x + additional x)
    |> Array.sum