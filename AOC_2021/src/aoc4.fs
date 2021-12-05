module AOC2021.Day4

open System
open AOC2021.Common

type BingoCell = {
  value: int
  isCalled: bool
}

type BingoBoard = {
  id: int
  cells: BingoCell[][]
}

let parseBoard (i:int) (boardLines: string[]) : BingoBoard=
  boardLines
    |> Array.skip 1
    |> Array.map (fun line -> 
      line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) 
      |> Array.map Int32.Parse
      |> Array.map (fun x -> {value=x; isCalled=false})
      )
    |> fun x -> {id=i; cells = x}

let parseBoards (lines: string seq) = 
  lines
  |> Seq.chunkBySize 6
  |> Seq.mapi parseBoard

let printCell cell =
  if cell.isCalled then
    sprintf "[%2d]" cell.value
  else
    sprintf " %2d " cell.value

let printBoard board : string =
  board.cells
  |> Seq.map (fun row -> 
      row
      |> Array.map printCell
      |> fun x -> sprintf "%s %s %s %s %s" (x.[0]) (x.[1]) (x.[2]) (x.[3]) (x.[4])
  )
  |> Seq.toArray
  |> fun s -> String.Join("\r\n", s)

let call x board :BingoBoard =
  board.cells
  |> Array.map (fun row ->
    row
    |> Array.map (fun cell -> 
      {cell with isCalled = if cell.value = x then true else cell.isCalled}
    )
  )
  |> fun x -> {id=board.id; cells=x}

let numCalled (line:BingoCell[]) =
  line
  |> Array.filter (fun x -> x.isCalled)
  |> Array.length

let maxCalled (board:BingoBoard) =
  let byRow = 
    board.cells
    |> Array.map numCalled
    |> Array.max
  let byCol = 
    board.cells
    |> Array.transpose
    |> Array.map numCalled
    |> Array.max
  [byRow; byCol] |> Seq.max

type BingoResult = {
    board: BingoBoard seq
    lastCalled: int
    remainingCalls: int seq
    remainingBoards: BingoBoard list
  }


let rec callTillBingo (boards: BingoBoard seq) (calls: int seq) =
  if calls |> Seq.isEmpty || boards |> Seq.isEmpty then 
    // printfn "No calls."
    None
  else
    let n = calls |> Seq.head
    // printfn "call %d" n
    let remainingCalls = (calls |> Seq.tail)
    let updated =
      boards
      |> Seq.map (call n)
    let winners = 
      updated
      |> Seq.map (fun b -> (maxCalled b, b))
      |> Seq.filter (fun (n, b) -> n >= 5 )
      |> Seq.map snd
    if (winners |> Seq.isEmpty ) then 
      if remainingCalls |> Seq.isEmpty then 
        // printfn "No more calls remaining."
        None
      else
        callTillBingo updated remainingCalls
    else
      let winningIds = winners |> Seq.map (fun b -> b.id)
      let remaining = updated |> Seq.filter ( fun b -> winningIds |> Seq.contains b.id |> not)
      // printfn " !! BINGO: %A !!" winningIds
      {
        board = winners
        lastCalled = n
        remainingCalls = remainingCalls 
        remainingBoards = remaining |> Seq.toList
      }
      |> Some

let rec callTillLastBingo (boards: BingoBoard list) (calls: int seq) =
  match callTillBingo boards calls with 
  | None -> None
  | Some result ->
    if boards.Length = 1 then
      Some result
    else
      callTillLastBingo result.remainingBoards  result.remainingCalls
      |> Option.orElse (Some result)


let filterBingo (predicate: BingoCell -> bool ) (board: BingoBoard) =
  board.cells
  |> filterDoubleArray predicate