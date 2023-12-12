module AOC2023.Day10

open System
open AOC2023.Common

type Direction = {dx: int; dy: int}
let Top = {dx = 0; dy = -1}
let Bottom = {dx = 0; dy = 1}
let Left= {dx = -1; dy = 0}
let Right= {dx = 1; dy = 0}

type Tile = {
  display: char
  exits: Direction list
}
with 
  override this.ToString() =
    sprintf "%c" this.display

type Cell<'a> = {
  row: int
  col: int
  value: 'a
}

type XYBoard<'T> = {
  cells: 'T list list
}with  
  override this.ToString() =
    this.cells
    |> List.map ( fun line -> 
        line 
        |> Seq.map ( fun cell -> cell.ToString().Substring(0,1))
        |> Join ""
    )
    |> Join "\n"

let private flattenBoard board =
    board.cells
    |> List.mapi (fun row line -> 
      line
      |> List.mapi (fun col cell -> 
        { row=row; col=col; value=cell}
      )
    )
    |> List.collect id

let private compileBoard nRows nCols (defaultValue: 'a) (cells:Cell<'a> list) =
    [0..nRows]
    |> List.map (fun row -> 
        [0..nCols]
        |> List.map (fun col -> 
            match cells |> List.tryFind (fun x -> x.row=row && x.col=col) with 
            | Some cell -> cell.value
            | None -> defaultValue
        )
    )
    |> fun x -> {cells = x}
    |> log


let private parseBoard (transform: char -> 'a) (lines: string list) =
  lines 
  |> List.map ( fun line -> 
      line 
      |> Seq.map transform
      |> Seq.toList
  )
  |> fun x -> {cells = x}

let private findStart (board: XYBoard<Tile>) = 
  board
  |> flattenBoard
  |> List.find (fun x -> x.value.display = 'S')

let rec private findDistances row col n (board: XYBoard<Tile>) =
  match board.cells.[row].[col].display with 
  | 'S' -> [{row=row; col=col; value=0}]
  | _ -> [{row=row; col=col; value= -1}]

  |> compileBoard board.cells.Length board.cells.[0].Length -1

let findDistanceFromStart (lines: string list) =
  let board = 
    lines 
    |> parseBoard ( fun c -> 
        match c with 
        | '.' -> { display='.'; exits = []}
        | '|' -> { display='|'; exits = [Top; Bottom]}
        | '-' -> { display='-'; exits = [Left; Right]}
        | 'L' -> { display='L'; exits = [Top; Right]}
        | 'J' -> { display='J'; exits = [Top; Left]}
        | '7' -> { display='7'; exits = [Left; Bottom]}
        | 'F' -> { display='F'; exits = [Bottom; Right]}
        | 'S' -> { display='S'; exits = [Top;Left;Bottom;Right]}
        | x -> failwithf "Unknown tile: %A" x
    ) 
  let start = findStart board
  findDistances start.row start.col 0 board

