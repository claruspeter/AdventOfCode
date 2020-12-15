module AOC10

open AOC_Common

let inputs = 
    lines 10
    |> Array.toList
    |> List.choose parseInts

let  n = inputs.Length

let makeSteps data =
  let max = data |> List.max
  let sorted = [0] @ (data |> List.sort) @ [max + 3]
  sorted 
  |> List.pairwise
  |> List.map (fun (a, b) -> b - a)


let rec private countAllCombos data =
  match data with 
  | [] -> [1L]
  | [a] -> [1L]
  | head::tail ->
    let tailCounts = countAllCombos tail
    let possibleNext = tail |> List.filter (fun x -> x - head <= 3) 
    let numExtraBranches = possibleNext.Length - 1
    // printfn "%2d: %A = %d ... %A" head possibleNext numPossibleNext tailCounts
    let numChildBranches = 
      [0..numExtraBranches]
      |> List.map (fun i -> tailCounts.[i])
      |> List.reduce (fun acc x -> acc + (x - 1L) )
    [numChildBranches + (int64 numExtraBranches)] @ tailCounts


let countCombos data =
  let sorted = [0] @ (data |> List.sort) @ [data |> List.max |> (+) 3]
  // printfn "%A" sorted
  countAllCombos sorted
  |> List.head