module AOC9

open System
open AOC_Common
open AOC_Pairs

let inputs = 
    lines 9
    |> Array.map (Int64.Parse)
    |> Array.toList

let  n = inputs.Length

let preambleList n xs=
  (xs |> List.take n, xs |> List.skip n)

let rec findNonSum preamble collection =
  match collection with 
  | [] -> failwith "can't find non-sum match"
  | x::tail ->
    let pairs = preamble |> pairup |> List.map (fun x -> x.Sum)
    match pairs |> List.contains x with
    | false -> x
    | true -> findNonSum (preamble.Tail @ [x]) tail

let rec sumFrom target acc subCollection =
  match subCollection with 
  | [] -> None
  | [x] -> if x + acc = target then Some [x] else None
  | x::tail -> 
    match x + acc with 
    | a when a = target -> Some [x]
    | a when a > target -> None
    | a when a < target ->
      match sumFrom target a tail with 
      | None -> None
      | Some contiguous -> [x] @ contiguous |> Some
    | err -> failwithf "Huh? %A" err


let rec contiguousAddends target collection  =
  match collection with 
  | [] -> failwith "contiguous not found"
  | _ ->
    match sumFrom target 0L collection with 
    | Some x -> x
    | None -> 
      //step forward one place and try again
      contiguousAddends target collection.Tail

