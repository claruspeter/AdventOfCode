module AOC1

open AOC_Common

let inputs = 
    (raw 1).Split([|'\r'; '\n'|])
    |> Array.choose parseInts

let  n = inputs.Length

type Pair = {a:int; b:int}
  with 
    member this.Sum = this.a + this.b
    member this.Product = this.a * this.b

type Trio = {a:int; b:int; c: int}
  with 
    member this.Sum = this.a + this.b + this.c
    member this.Product = this.a * this.b * this.c

let rec private pairup (numbers: int list ): Pair list =
  match numbers with 
  | [a; b] -> [{a=a; b=b}]
  | a :: tail -> 
    let allAs = tail |> List.map (fun i -> {a=a; b=i})
    let remainder = pairup tail
    allAs @ remainder
  | _ -> failwith "at least 2 members required"

let rec private trioup (numbers: int list ): Trio list =
  match numbers with 
  | [a; b; c] -> [{a=a; b=b; c=c}]
  | a :: tail -> 
    let allAs = 
      tail 
      |> pairup
      |> List.map (fun p -> {a=a; b=p.a; c=p.b})
    let remainder = trioup tail
    allAs @ remainder
  | _ -> failwith "at least 3 members required"

let pairs = 
  inputs 
  |> Array.toList
  |> pairup

let trios = 
  inputs 
  |> Array.toList
  |> trioup

let ``pairs that add to 2020``=
  pairs
  |> List.filter (fun x -> x.Sum = 2020)

let ``trios that add to 2020``=
  trios
  |> List.filter (fun x -> x.Sum = 2020)