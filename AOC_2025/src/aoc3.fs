module AOC2023.Day3

open System
open AOC2023.Common

let findMaxSingleJoltage  (tailSize: int) (indexed: (int * char) list) =
  indexed 
    |> Seq.take (indexed.Length - tailSize)
    |> Seq.maxBy snd


let findTwoBatteryJoltage (line:string) =
  let indexed = line |> Seq.indexed |> Seq.toList
  let maxVal = findMaxSingleJoltage 1 indexed
  let maxAfterMax = 
    indexed
    |> List.skip (fst maxVal |> (+) 1)
    |> findMaxSingleJoltage 0
  (snd maxVal |> charInt) * 10 + (snd maxAfterMax |> charInt)

type private JoltageSearch = {
  remaining: (int * char) list
  maxes: (int * char) list
}

let findTwelveBatteryJoltage (line:string) =
  let indexed = line |> Seq.indexed |> Seq.toList
  let found = 
    [11..-1..0]
    |> List.fold (
        fun acc i -> 
          let max = findMaxSingleJoltage i acc.remaining
          let result = 
            {
              remaining = indexed |> List.skip (fst max |> (+) 1); 
              maxes = acc.maxes @ [max] 
            }
          result

      ) {remaining=indexed; maxes=[] }
  let twelveSorted = found.maxes |> Seq.sortBy fst
  twelveSorted 
    |> Seq.map snd 
    |> Seq.toArray
    |> String
    |> Int64.Parse