module AOC2023.Day1

open System
open AOC2023.Common

type DialResult = {
  startingValue: int
  cmd: string
  finalValue: int
  isZero: bool      // finished on zero, exactly
  passedZero: int   // in either direction
  belowZero: bool   // hit or passed zero to the left
}with 
  member this.numberZeroTouches =
    (if this.belowZero then 1 else 0)
      + this.passedZero

let dial (current:DialResult) (cmd: string) =
  let direction = if cmd.[0] = 'L' then -1 else 1
  let amount = cmd.Substring(1) |> Int32.Parse
  let raw = current.finalValue + direction * amount
  let capped = raw % 100
  let looped = if capped < 0 then 100 + capped else capped

  { 
    startingValue=current.finalValue
    cmd=cmd
    finalValue = looped
    isZero = looped = 0
    passedZero = Math.Abs(raw) / 100
    belowZero = raw <= 0 && current.finalValue > 0
  }