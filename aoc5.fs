module AOC5

open System
open AOC2

let processCmd3 ints paras = 
    let result = ints |> Array.copy
    Console.Write ("INPUT PLEASE > ")
    let inputValue = Console.ReadLine() |> Int32.Parse
    result.[paras.destination] <- inputValue
    result

let processCmd4 (ints: int[]) (paras: IntParams) = 
    printfn "OUTPUT: %i" ints.[paras.destination]
    ints

let FourCommands = 
    TwoCommands @ [
        { processor = processCmd3; numberOfParameters=0 }
        { processor = processCmd4; numberOfParameters=0 }
    ]

let test1() = 
    [|3;0;4;0;99|]
    |> processIntMachine FourCommands 0