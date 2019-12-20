module AOC5

open System
open AOC2

let processCmd3 ints paras = 
    let result = ints.data |> Array.copy
    let inputValue = ints.inputs.[0]
    result.[paras.destination] <- inputValue
    { data = result; inputs=ints.inputs |> List.tail; outputs=ints.outputs}

let processCmd4 ints (paras: IntParams) = 
    let outputValue = ints.data.[paras.destination]
    printfn "OUTPUT: %i" outputValue
    {ints with outputs=ints.outputs @ [outputValue] }

let FourCommands = 
    TwoCommands @ [
        { processor = processCmd3; numberOfParameters=0 }
        { processor = processCmd4; numberOfParameters=0 }
    ]

let test1() = 
    {data = [|3;0;4;0;99|]; inputs=[42]; outputs=[]}
    |> processIntMachine FourCommands 0
    |> fun x -> x.outputs.[0]