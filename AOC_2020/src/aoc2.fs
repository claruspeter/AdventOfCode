module AOC2

open System
open FSharp.Scanf
open AOC_Common

type PwdAndPolicy = {min: int; max: int; letter: char; pwd: string;}


let inputs = 
    raw 2
    |> fun s -> s.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (sscanf "%d-%d %c: %s")
    |> Array.map (fun (min, max, letter, pwd) -> {min=min; max=max; letter=letter; pwd=pwd;})

let  n = inputs.Length

let valid pp =
  let n = 
    pp.pwd 
    |> Seq.filter ((=) pp.letter)
    |> Seq.length
  n >= pp.min && n <= pp.max

let numValid validity pwds =
  inputs
  |> Array.filter validity
  |> Array.length


let validV2 pp =
  let isA = pp.pwd.Length >= pp.min &&  pp.pwd.[pp.min - 1] = pp.letter
  let isB = pp.pwd.Length >= pp.max && pp.pwd.[pp.max - 1] = pp.letter
  isA <> isB  // same as xor