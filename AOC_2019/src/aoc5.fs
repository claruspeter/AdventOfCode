module AOC5

open System
open AOC_Common
open AOC2

let processCmd3 ints paras = 
    let result = ints.data |> Array.copy
    let inputValue = ints.inputs.[0]
    // printfn "INPUT: %A" inputValue
    result.[paras.destination] <- inputValue
    { data = result; inputs=ints.inputs |> List.tail; outputs=ints.outputs}

let processCmd4 ints (paras: IntParams) = 
    let outputValue = ints.data.[paras.destination]
    // printfn "OUTPUT: %A" outputValue
    {ints with outputs=ints.outputs @ [outputValue] }

let FourCommands = 
    TwoCommands @ [
        { processor = processCmd3; numberOfParameters=0 }
        { processor = processCmd4; numberOfParameters=0 }
    ]

let test1 inValue = 
    {data = [|3;0;4;0;99|]; inputs=[inValue]; outputs=[]}
    |> processIntMachine FourCommands 0
    |> fun x -> x.outputs.[0]

let testModes() =
    {data = [|1002;4;3;4;33|]; inputs=[]; outputs=[]}
    |> processIntMachine FourCommands 0
    |> fun x -> x.data.[4]

let inputs5() = 
    (raw 5).Split([|','|])
    |> Array.choose parseInts


let runDiagnosticA() =
    inputs5()
    |> fun data -> {data=data; inputs=[1]; outputs=[]}
    |> processIntMachine FourCommands 0
    |> fun x -> x.outputs |> List.last
