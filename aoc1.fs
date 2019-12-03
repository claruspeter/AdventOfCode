module AOC1

open AOC_Common

let inputs = 
    (raw 1).Split([|'\r'; '\n'|])
    |> Array.choose parseInts


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
    inputs
    |> Array.map fuelFor
    |> Array.sum

let data_b = 
    inputs
    |> Array.map fuelFor
    |> Array.map (fun x -> x + additional x)
    |> Array.sum