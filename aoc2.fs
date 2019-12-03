module AOC2

open AOC_Common

let printProgram (ints: int []) =
    ints
    |> Array.chunkBySize 4
    |> Array.map (fun x -> if x.Length = 4 then x else Array.append x [|-1; -1; -1; -1|] |> Array.take 4 )
    |> Array.iteri (fun i x -> printfn "%4i: %4i %4i %4i %4i" (i*4) x.[0] x.[1] x.[2] x.[3])


let inputs = 
    (raw 2).Split([|','|])
    |> Array.choose parseInts


let doctor noun verb (ints: int []) = 
    let result = ints |> Array.copy
    result.[1] <- noun 
    result.[2] <- verb 
    result

let rec processIntMachine position (ints: int [])  =
    let a = ints.[position + 1]
    let aValue = ints.[a]
    let b = ints.[position + 2]
    let bValue = ints.[b]
    let at = ints.[position + 3]
    // printfn "PROG %4i: %4i %4i(%4i) %4i(%4i) %4i" position ints.[position] a aValue b bValue at
    match ints.[position] with
    | 1 -> 
        let result = ints |> Array.copy
        result.[at] <- aValue + bValue
        processIntMachine (position + 4) result 
    | 2 -> 
        let result = ints |> Array.copy
        result.[at] <- aValue * bValue
        processIntMachine (position + 4) result 
    | 99 -> 
        // End
        ints
    | _ -> failwithf "Unkown Command: %i" ints.[position]

let generate1202Error = 
    inputs
    |> doctor 12 02
    |> processIntMachine 0
    |> printProgram

let target = 19690720

let findTarget = 
    let run = 
        [for noun in [0..99] do
            for verb in [0..99] do
                let result = 
                    inputs
                    |> doctor noun verb
                    |> processIntMachine 0
                    |> (fun x -> x.[0])
                yield {| noun=noun; verb=verb; result=result; success=(result = target)|}
        ]
    run
    |> List.find (fun x -> x.success)