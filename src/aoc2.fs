module AOC2

open System
open AOC_Common

let printProgram (ints: int []) =
    ints
    |> Array.chunkBySize 4
    |> Array.map (fun x -> if x.Length = 4 then x else Array.append x [|-1; -1; -1; -1|] |> Array.take 4 )
    |> Array.iteri (fun i x -> printfn "%4i: %4i %4i %4i %4i" (i*4) x.[0] x.[1] x.[2] x.[3])
    ints

let inputs() = 
    (raw 2).Split([|','|])
    |> Array.choose parseInts

let doctor noun verb (ints: int []) = 
    let result = ints |> Array.copy
    result.[1] <- noun 
    result.[2] <- verb 
    result

type ParamMode = Position | Immediate

// type ParaValue = {
//     value: int
//     valueAtPosition: int
// } with
//     override this.ToString() = sprintf "{%i -> %i}" this.value this.valueAtPosition

type IntParams = {
    destination: int
    parameters: int list
}

type IntMachine = {
    data: int[]
    inputs: int list
    outputs: int list
}  

let mcFromData ints = {data=ints; inputs=[]; outputs=[]}

let compareData data intMc = 
    intMc.data 
    |> Array.compareWith (fun a b -> a.CompareTo b) data

type CmdProcessor = IntMachine -> IntParams -> IntMachine
type Cmd = {
    processor: CmdProcessor
    numberOfParameters: int
}

type Instruction = {
    opcode: int
    modes: ParamMode list
}

let readParam paraPosition mode (ints: int []) =
    let immediateValue = ints.[paraPosition]
    match mode with 
    | Immediate -> immediateValue
    | Position -> ints.[immediateValue]

let readInstruction instructionValue =
    {
        opcode = instructionValue % 100
        modes = 
            [ (instructionValue / 100) % 10; (instructionValue / 1000) % 10; instructionValue / 10000]
            |> List.map (
                function
                | 0 -> Position
                | _ -> Immediate
            )
    }

let processCmd1 ints paras = 
    let result = ints.data |> Array.copy
    result.[paras.destination] <- paras.parameters.[0] + paras.parameters.[1]
    result |> mcFromData

let processCmd2 ints paras = 
    let result = ints.data |> Array.copy
    result.[paras.destination] <- paras.parameters.[0] * paras.parameters.[1]
    result |> mcFromData

let TwoCommands = [
    { processor = processCmd1; numberOfParameters=2 }
    { processor = processCmd2; numberOfParameters=2 }
]

let rec processIntMachine (commandProcessors: Cmd list) position (ints: IntMachine)  =
    let instruction = readInstruction ints.data.[position]
    // printfn "CMD at %i: %A" position instruction
    if instruction.opcode = 99 then 
        // printfn "END"
        ints
    else
        let cmd = commandProcessors.[instruction.opcode - 1]
        // printfn "-paras: %i" cmd.numberOfParameters
        let paraValues = 
            [1..cmd.numberOfParameters]
            |> List.map ( fun i -> readParam (position + i) (instruction.modes.[i-1]) ints.data)
        // paraValues |> List.iteri (fun i x -> printfn "--para: %i=%O" (i + position) x)
        let paras = {
            destination = ints.data.[position + cmd.numberOfParameters + 1 ]
            parameters = paraValues
        }
        let result = cmd.processor ints paras
        processIntMachine commandProcessors (position + cmd.numberOfParameters + 2) result 

let test1() = 
    [|1;0;0;0;99|]
    |> mcFromData
    |> processIntMachine TwoCommands 0
    |> compareData [|2;0;0;0;99|]

let test2() = 
    [|2;3;0;3;99|]
    |> mcFromData
    |> processIntMachine TwoCommands 0
    |> compareData [|2;3;0;6;99|]

let test3() = 
    [|2;4;4;5;99;0|]
    |> mcFromData
    |> processIntMachine TwoCommands 0
    |> compareData [|2;4;4;5;99;9801|]

let test4() = 
    [|1;1;1;4;99;5;6;0;99|]
    |> mcFromData
    |> processIntMachine TwoCommands 0
    |> compareData [|30;1;1;4;2;5;6;0;99|]

let generate1202Error() = 
    inputs()
    |> doctor 12 02
    |> mcFromData
    |> processIntMachine TwoCommands 0
    // |> printProgram
    |> fun x -> x.data.[0]

let target = 19690720

let findTarget() = 
    let run = 
        [for noun in [0..99] do
            for verb in [0..99] do
                let result = 
                    inputs()
                    |> doctor noun verb
                    |> mcFromData
                    |> processIntMachine TwoCommands 0
                    |> (fun x -> x.data.[0])
                yield {| noun=noun; verb=verb; result=result; success=(result = target)|}
        ]
    run
    |> List.find (fun x -> x.success)