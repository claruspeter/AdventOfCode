module AOC2023.Day2

open System
open AOC2023.Common

type ProductRange = {
  min: int64
  max: int64
  invalid: int64 list
}

let repeatsMany (s:string) =
  [1..s.Length/2]
  |> List.exists (
      fun n ->
        let times = s.Length / n
        let segment = s.Substring(0,n)
        let candidate = String.replicate times segment
        candidate = s
  )



let repeatsOnce (s:string) =
  s.Substring(0, s.Length / 2) = s.Substring(s.Length/2)

let findInvalid strategy range = 
  [range.min..range.max]
  |> List.choose (
      fun i ->
        let s = i.ToString()
        if strategy s then 
          Some i
        else 
          None
  )
  |> fun invalid -> {range with invalid=invalid}

let parse (line:string) =
  line
    |> _.Split([|','; '-'|])
    |> Seq.map Int64.Parse
    |> Seq.chunkBySize 2
    |> Seq.map (fun [|a; b|] -> {min=a; max=b; invalid=[]})
    |> Seq.toList
  
