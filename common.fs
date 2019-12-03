module AOC_Common

open System 
open System.IO

let parseInts x = 
    try
        Some (Int32.Parse x)
    with
    | _ -> None


let raw n = File.ReadAllText(sprintf "data/aoc%d_input.txt" n)
