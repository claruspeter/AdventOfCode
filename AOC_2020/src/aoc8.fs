module AOC8

open FSharp.Scanf.Scanf
open AOC_Common

let inputs = 
    lines 8
    |> Array.toList

let  n = inputs.Length

type Instruction =
  | NOP of int
  | ACC of int
  | JMP of int

let (|Sign|_|) (c:char) =
  match c with
  | '+' -> Some +1
  | '-' -> Some -1
  | _ -> None


let parseInstruction s = 
  s 
  |> sscanf "%s %c%d"
  |>  function
      | "nop", Sign sign, x -> sign * x |> NOP
      | "acc", Sign sign, x -> sign * x |> ACC
      | "jmp", Sign sign, x -> sign * x |> JMP
      | _ -> failwith "Not valid"

let instructions s = s |> Seq.map parseInstruction |> Seq.toList

let rec private executeSafely (history: int list) (acc:int) (data: Instruction list) (from: int) = 
  if history.Length > 99999 then failwith "Too many entries!"

  match history |> List.contains from with 
  | true -> Error acc
  | false -> 
    let (newIndex, newAcc ) =
      match data.[from] with 
      | NOP x -> 
        (from + 1, acc)
      | ACC x -> 
        (from + 1, acc + x)
      | JMP x -> 
        (from + x, acc)
    match newIndex >= data.Length with 
    | true -> Ok newAcc
    | false ->
      executeSafely (history @ [from]) newAcc data newIndex

let rec executeWithFiddle (history: int list) (acc:int) (data: Instruction list) (from: int) =
  match history |> List.contains from with 
  | true -> Error acc
  | false ->    
    match data.[from] with 
    | NOP x -> 
      //try switching with a JMP and continue with run
      let hijackedData = data |> List.mapi ( fun i elem -> if i=from then JMP x else elem )
      match executeSafely history acc hijackedData from with 
      | Error _ -> 
        // didn't work so move forward down the chain
        executeWithFiddle history acc data (from + 1)
      | Ok value ->
        Ok value
    | JMP x -> 
      //try switching with a NOP and continue with run
      let hijackedData = data |> List.mapi ( fun i elem -> if i=from then NOP x else elem )
      match executeSafely history acc hijackedData from with 
      | Error _ -> 
        // didn't work so move forward down the chain
        executeWithFiddle history acc data (from + x)
      | Ok value -> 
        Ok value
    | ACC x -> 
      executeWithFiddle (history @ [from]) (acc + x) data (from + 1)



let execute (data: Instruction list)=
  executeSafely [] 0 data 0 

let executeFiddle (data: Instruction list)=
  executeWithFiddle [] 0 data 0 