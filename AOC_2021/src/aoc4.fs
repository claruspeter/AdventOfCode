module AOC2021.Day4

open System
open AOC2021.Common

type BingoCell = {
  value: int
  isCalled: bool
}

type BingoBoard =  BingoCell[][]

let parseBoard (boardLines: string[]) : BingoBoard=
  boardLines
    |> Array.skip 1
    |> Array.map (fun line -> 
      line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) 
      |> Array.map Int32.Parse
      |> Array.map (fun x -> {value=x; isCalled=false})
      )

let parseBoards (lines: string seq) = 
  lines
  |> Seq.chunkBySize 6
  |> Seq.map parseBoard

let printCell cell =
  if cell.isCalled then
    sprintf "[%2d]" cell.value
  else
    sprintf " %2d " cell.value

let printBoard board : string =
  board 
  |> Seq.map (fun row -> 
      row
      |> Array.map printCell
      |> fun x -> sprintf "%s %s %s %s %s" (x.[0]) (x.[1]) (x.[2]) (x.[3]) (x.[4])
  )
  |> Seq.toArray
  |> fun s -> String.Join("\r\n", s)

let call x board :BingoBoard =
  board
  |> Array.map (fun row ->
    row
    |> Array.map (fun cell -> 
      {cell with isCalled = if cell.value = x then true else cell.isCalled}
    )
  )

let numCalled (line:BingoCell[]) =
  line
  |> Array.filter (fun x -> x.isCalled)
  |> Array.length

let maxCalled (board:BingoBoard) =
  let byRow = 
    board
    |> Array.map numCalled
    |> Array.max
  let byCol = 
    board
    |> Array.transpose
    |> Array.map numCalled
    |> Array.max
  [byRow; byCol] |> Seq.max

let rec callTillBingo (boards: BingoBoard seq) (calls: int seq) =
  let n = calls |> Seq.head
  // printfn "CALL = %d" n
  let updated =
    boards
    |> Seq.map (call n)
  let maxed = 
    updated
    |> Seq.map (fun b -> (maxCalled b, b))
    |> Seq.sortByDescending fst
    // |> logseqf (fun x -> 
    //       (x |> fst |> sprintf "%d:\r\n") + (x |> snd |> printBoard)
    //   )
  if (maxed |> Seq.head |> fst) < 5 then 
    callTillBingo updated (calls |> Seq.tail)
  else
    {| board = maxed |> Seq.head |> snd; called = n |}

let filterBingo (predicate: BingoCell -> bool ) (board: BingoBoard) =
  board
  |> Array.collect (Array.filter predicate)