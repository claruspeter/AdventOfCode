module AOC_Pairs
open System

type Pair = {a:Int64; b:Int64}
  with 
    member this.Sum = this.a + this.b
    member this.Product = this.a * this.b

type Trio = {a:Int64; b:Int64; c: Int64}
  with 
    member this.Sum = this.a + this.b + this.c
    member this.Product = this.a * this.b * this.c

let rec pairup (numbers: Int64 list ): Pair list =
  match numbers with 
  | [a; b] -> [{a=a; b=b}]
  | a :: tail -> 
    let allAs = tail |> List.map (fun i -> {a=a; b=i})
    let remainder = pairup tail
    allAs @ remainder
  | _ -> failwith "at least 2 members required"

let rec trioup (numbers: Int64 list ): Trio list =
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