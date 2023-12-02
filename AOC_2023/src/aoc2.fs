module AOC2023.Day2

open System
open AOC2023.Common

type ColourCount = {
  n: int
  red: int
  green: int
  blue: int
}

let (|Int|_|) (input:string) =
  match Int32.TryParse input with 
  | true, i -> Some i
  | _ -> None

let (|Colour|_|) (input: string) =
  match input.Split(' ') with 
  | [|Int v; name|] -> (name, v) |> Some
  | _ -> 
    printfn "ERROR: Not a colour - %A" input
    None

let parseHead (head:string) = 
  head.Substring(5) |> int

let parseColourCount n (pull: string) =
  let zero = {n=n; red=0; green=0; blue=0}
  let parts = pull.Split([|','|], StringSplitOptions.TrimEntries)
  parts 
  |> Seq.fold ( fun acc i ->
      match i with
      | Colour (name, v) when name = "red" -> { acc with red = v}
      | Colour (name, v) when name = "green" -> { acc with green = v}
      | Colour (name, v) when name = "blue" -> { acc with blue = v}
      | _ -> 
        printfn "Input not valid: %A" i
        acc
    ) zero

let maxCount (line:string) : ColourCount =
  match line.Split([|':'|]) with 
  | [|head; body|] -> 
      let n = parseHead head
      let pulls = body.Split([|';'|], StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
      let counts = pulls |> Array.map (parseColourCount n)
      counts 
      |> Array.reduce (fun acc i -> 
          {acc with red = max acc.red i.red; green = max acc.green i.green; blue = max acc.blue i.blue  }
        )
  | _ -> failwith "Not valid line"

let findPossibleGames (limit:ColourCount) (lines:string list) =
  lines 
  |> List.map maxCount
  |> List.choose (
      function
      | x when x.blue <= limit.blue && x.green <=limit.green && x.red <= limit.red -> Some x.n
      | _ -> None
  )

