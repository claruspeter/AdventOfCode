module AOC2021.Day8

open System
open AOC2021.Common

let parse7DigitDisplayLine (s: string) =
  let [| lhs; rhs |] = s.Split([| '|' |]) |> Array.take 2
  (
    lhs.Split([| ' '|], StringSplitOptions.RemoveEmptyEntries),
    rhs.Split([| ' '|], StringSplitOptions.RemoveEmptyEntries)
  )

let classify (s: string) =
  match s.Length with 
  | 2 -> 2 |> Some
  | 3 -> 3 |> Some
  | 4 -> 4 |> Some
  | 7 -> 7 |> Some
  | _ -> None

let train (inputs: string[]) =
  let grouped = 
    inputs 
    |> Array.map set
    |> Array.groupBy Set.count 
    |> dict
  let d1 = grouped.[2].[0]
  let d7 = grouped.[3].[0]
  let d4 = grouped.[4].[0] 
  let d8 = grouped.[7].[0]
  let d3 = grouped.[5] |> Array.find (fun x -> x.IsSupersetOf d1)
  let d9 = grouped.[6] |> Array.find (fun x -> x.IsSupersetOf d3)
  let d6 = grouped.[6] |> Array.except [| d9 |] |> Array.find (
    fun six -> grouped.[5] |> Array.except [| d3 |] |> Array.exists (fun five -> six.IsSupersetOf five)
  )
  let d5 = grouped.[5] |> Array.except [| d3 |] |> Array.find (fun x -> x.IsSubsetOf d6)
  let d0 = grouped.[6] |> Array.except [| d6; d9 |] |> Array.exactlyOne
  let d2 = grouped.[5] |> Array.except [| d3; d5 |] |> Array.exactlyOne
  [| d0; d1; d2; d3; d4; d5; d6; d7; d8; d9|]

let classifyFromTraining trained (s:string) =
  let s' = set s
  trained
  |> Array.findIndex ((=) s')

let decode parsedLine =
  let classifier = parsedLine |> fst |> train |> classifyFromTraining
  parsedLine 
  |> snd 
  |> Array.map classifier
  |> Array.map string
  |> fun x -> String.Join("", x)
  |> Int32.Parse