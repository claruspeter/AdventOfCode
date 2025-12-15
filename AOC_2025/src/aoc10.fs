module AOC2023.Day10

open System
open AOC2023.Common

type Button = int list

type Machine = {
  lights: bool list
  buttons: Button list
  joltage: int list
}

let private parseButton (part: string) = 
  part
    .Replace(")", "")
    .Replace("(", "")
    .Split(',')
    |> Seq.map Int32.Parse
    |> Seq.toList

let parse (lines: string list) =
  lines
  |> List.map (
    fun line -> 
      let bIdx = line.IndexOf('(')
      let cIdx = line.IndexOf('{')
      let a = line[1..bIdx-3]
      let b = line[bIdx..cIdx-2]
      let c = line[cIdx+1..line.Length-2]
      {
        lights = a |> Seq.map ((=) '#') |> Seq.toList
        buttons = b.Split('(', StringSplitOptions.RemoveEmptyEntries) |> Seq.map parseButton |> Seq.toList
        joltage = c.Split(',', StringSplitOptions.RemoveEmptyEntries) |> Seq.map Int32.Parse |> Seq.toList
      }
  )

let rec private combinations n l : 'a list seq=
  match (n,l) with
  | 0,_     -> seq [[]]
  | _,[]    -> Seq.empty
  | n,x::xs ->
    seq {
      let useX = Seq.map (fun l -> x::l) (combinations (n-1) xs)
      let noX = combinations n xs
      yield! useX
      yield! noX
    }

let private apply (lights: bool list) (btn: Button)  =
  lights
  |> List.mapi ( fun i light -> if btn |> List.contains i then not light else light )



let findLeastButtons (machine: Machine) : Button list seq= 
  let target = machine.lights // |> logm "TARGET"
  let initial = List.init target.Length (fun _ -> false) // |> logm "INITIAL"
  seq {
    for comboLength in Seq.init target.Length id do
      let combo = machine.buttons |> combinations comboLength
      for c in combo do
        // log c |> ignore
        let transformed = 
          c
          |> List.fold apply initial
        if target = transformed then 
          // log "Found!" |> ignore
          yield c
  }