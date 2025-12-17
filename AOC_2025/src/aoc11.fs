module AOC2023.Day11

open System
open AOC2023.Common

type Connection = {
  name: string
  outlets: string list
}

let parse (lines: string list ) =
  lines
  |> List.map (
    fun line -> 
      line.Split(' ', StringSplitOptions.RemoveEmptyEntries)
      |> fun x -> { name = x |> Array.head |> _.TrimEnd(':') ; outlets = x |> Array.tail |> Seq.toList }
  )

let rec private findRoutesFromMap connections current =
  match current.name with 
  | "out" -> [["out"]]
  | _ -> 
    let remaining = connections |> List.except [current]
    let found = 
      current.outlets
      |> List.collect (
          fun key ->
            match remaining |> List.tryFind (fun c -> c.name = key) with 
            | None -> []
            | Some next -> findRoutesFromMap remaining next
      )
    found

let findRoutes (connections: Connection list) =
  let initial = connections |> List.find (fun c -> c.name = "you")
  findRoutesFromMap connections initial