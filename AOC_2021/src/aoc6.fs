module AOC2021.Day6

open System
open AOC2021.Common

let nextDay fish =
  fish
  |> Seq.collect (fun n -> 
      match n with 
      | 0 -> [6;8]
      | _ -> [n-1]
  )

type Fish = {
  countdowns : int64[]
}

let initFish (fishlist: int seq) = 
  let counts = 
    fishlist 
    |> Seq.countBy id
  { countdowns = 
    [|0..8|]
    |> Array.map (
        fun i ->
          counts 
          |> Seq.tryFind (fun (idx, _) -> idx = i) 
          |> Option.map (snd)
          |> Option.defaultValue 0
          |> int64 
      )
  }

let incDay (fish: Fish): Fish =
  /// Sort of a shift/rotate left plus injection of zeroes at #6
  let numZeroes = fish.countdowns.[0]
  {
    countdowns = [|
      fish.countdowns.[1] //--> 0
      fish.countdowns.[2] //--> 1
      fish.countdowns.[3] //--> 2
      fish.countdowns.[4] //--> 3
      fish.countdowns.[5] //--> 4
      fish.countdowns.[6] //--> 5
      fish.countdowns.[7] + numZeroes //--> 6
      fish.countdowns.[8] //--> 7
      numZeroes //--> 8
    |]
  }