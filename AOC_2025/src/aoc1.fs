module AOC2023.Day1

open System
open AOC2023.Common

type DialResult = {
  startingValue: int
  cmd: string
  finalValue: int
  isZero: bool
  negativeCrosses: int
  positiveCrosses: int
}with 
  member this.numberZeroTouches =
    (if this.isZero then 1 else 0)
      + this.negativeCrosses
      + this.positiveCrosses

let dial (current:DialResult) (cmd: string) =
  let direction = if cmd.[0] = 'L' then -1 else 1
  let amount = cmd.Substring(1) |> Int32.Parse
  let raw = current.finalValue + direction * amount
  let capped = raw % 100
  let looped = if capped < 0 then 100 + capped else capped
  let add1 = if current.finalValue > 0 then 1 else 0

  { 
    startingValue=current.finalValue
    cmd=cmd
    finalValue = looped
    isZero = looped = 0
    negativeCrosses = if raw < 0 then  Math.Abs(raw / 100) + add1 else 0
    positiveCrosses = if raw > 100 then (raw / 100) else 0
  }