module AOC2022.Day5

open System
open System.Collections.Generic
open AOC2022.Common

type Instruction = {
  n: int
  source: int
  destination: int
}

type CrateStack = Stack<char>

type World = {
  stacks: Dictionary<int, CrateStack>
  instructions: Instruction list
}

let displayStack (stack:CrateStack) =
  stack |> Seq.rev |> Seq.map (fun x -> $"[{x}]") |> fun x -> String.Join(" ", x)

let displayIndexedStack (stack: KeyValuePair<int, CrateStack>) =
  printfn "%d: %O" stack.Key (displayStack stack.Value)

let private parseInstruction (line:string) =
  line.Split([|' '|])
  |> fun x -> [x.[1]; x.[3]; x.[5]]
  |> List.map int
  |> fun x -> { n=x.[0]; source=x.[1]; destination=x.[2] }

let private parseStackLine (line:string) (stacks: Dictionary<int, CrateStack>) =
  let cratesAt = line |> Seq.indexed |> Seq.filter (fun (i, x) -> x = '[') |> Seq.map (fun (i,x) -> (i/4))
  let crates = cratesAt |> Seq.map (fun i -> (i, line.[i*4+1]))
  crates |> Seq.iter (fun (i,x) -> if stacks.ContainsKey (i+1) then stacks[i+1].Push x else stacks.Add(i+1, CrateStack([x])))
  stacks

let rec private parse (lines: string list) (world: World) : World =
  match lines with 
  | [] -> failwith "Empty list not allowed"
  | [head] -> 
      { world with instructions = world.instructions @ [parseInstruction head] }

  | head::tail -> 
      let updated = 
        match head with 
        | line when line.StartsWith('m') -> {world with instructions = world.instructions @ [parseInstruction line]}
        | line when line.Contains('[') -> {world with stacks = parseStackLine line world.stacks}
        | _ -> world
      parse tail updated


let parseCratesAndInstructions (lines: string list) =
  let reversedCrates =
    parse 
      lines
      {
        stacks = new Dictionary<int, CrateStack>()
        instructions = []
      }

  {
    reversedCrates with 
      instructions = reversedCrates.instructions
      stacks = 
        reversedCrates.stacks 
        |> Seq.map (fun x ->
            KeyValuePair<int,CrateStack>(
              x.Key,
              x.Value |> CrateStack
            )
          )
        |> Dictionary<int, CrateStack>
  }