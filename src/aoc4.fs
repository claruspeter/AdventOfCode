module AOC4


let hasDoubleAdjacent (pwd: char[]) =
    [0..4]
    |> Seq.exists (fun i -> pwd.[i] = pwd.[i+1])

let hasUniqueDoubleAdjacent (pwd: char[]) =
    pwd
    |> Seq.groupBy id
    |> Seq.map (fun (key, group) -> (key, group |> Seq.length))
    |> Seq.exists (fun (_, len) -> len = 2)

let isAlwaysIncreasing (pwd: char[]) =
    [0..4]
    |> Seq.forall (fun i -> pwd.[i] <= pwd.[i+1])

let toPwd (x:int) =
    x.ToString().ToCharArray()

let passesTestsA pwd =
    hasDoubleAdjacent pwd
    && isAlwaysIncreasing pwd

let passesTestsB pwd =
    hasUniqueDoubleAdjacent pwd
    && isAlwaysIncreasing pwd

let TEST_START = 356261
let TEST_END = 846303

let partA = 
    seq [TEST_START..TEST_END]
    |> Seq.map toPwd
    |> Seq.filter passesTestsA
    |> Seq.length

let partB = 
    seq [TEST_START..TEST_END]
    |> Seq.map toPwd
    |> Seq.filter passesTestsB
    |> Seq.length


