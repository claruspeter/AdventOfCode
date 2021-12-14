module AOC2021.Day13

open System
open AOC2021.Common
open AOC2021.Grids

type FoldAxis = FoldX | FoldY
type Fold = {
  axis: FoldAxis
  at: int
}

let parseFold (s:string) =
  match s with 
  | Regex @"fold along (.)=(\d+)" [_; axis; at] -> 
      Some {
        axis=if axis="x" then FoldX else FoldY
        at=Int32.Parse at
      }
  | _ -> None

let foldAlong (paper:Grid<bool>) (fold:Fold) =
  match fold.axis with 
  | FoldY -> 
      paper.values
      |> Array.map (fun (p, v) ->
          if p.y <= fold.at then
            (p, v)
          else
            ({p with y = fold.at - (p.y-fold.at)}, v)
      )
      |> Array.distinct
      |> fun v -> {values=v}
  | FoldX -> 
      paper.values
      |> Array.map (fun (p, v) ->
          if p.x <= fold.at then
            (p, v)
          else
            ({p with x = fold.at - (p.x-fold.at)}, v)
      )
      |> Array.distinct
      |> fun v -> {values=v}