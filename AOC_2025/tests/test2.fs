module Tests.Day2

open System
open Xunit
open FsUnit.Xunit
open AOC2023.Common
open AOC2023.Day2

let sample = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

[<Fact>]
let A_Sample () =
  let result = 
    sample
    |> parse
    |> List.map (findInvalid repeatsOnce)
  result
  |> should equal [
    {min=11L; max=22L; invalid=[11L; 22L]}
    {min=95; max=115; invalid=[99L]}
    {min=998; max=1012; invalid=[1010L]}
    {min=1188511880; max=1188511890; invalid=[1188511885L]}
    {min=222220; max=222224; invalid=[222222;]}
    {min=1698522; max=1698528; invalid=[]}
    {min=446443; max=446449; invalid=[446446L]}
    {min=38593856; max=38593862; invalid=[38593859L]}
    {min=565653; max=565659; invalid=[]}
    {min=824824821; max=824824827; invalid=[]}
    {min=2121212118; max=2121212124; invalid=[]}
  ]
  result
  |> List.collect _.invalid
  |> List.sum
  |> should equal 1227775554L
  
[<Fact>]
let A() =
  let result = 
    raw 2
    |> parse
    |> List.map (findInvalid repeatsOnce)
  result
  |> List.collect _.invalid
  |> List.sum
  |> should equal 30323879646L

[<Fact>]
let B_Sample () =
  let result = 
    sample
    |> parse
    |> List.map (findInvalid repeatsMany)
  result
  |> should equal [
    {min=11L; max=22L; invalid=[11L; 22L]}
    {min=95; max=115; invalid=[99L; 111L]}
    {min=998; max=1012; invalid=[999L; 1010L]}
    {min=1188511880; max=1188511890; invalid=[1188511885L]}
    {min=222220; max=222224; invalid=[222222;]}
    {min=1698522; max=1698528; invalid=[]}
    {min=446443; max=446449; invalid=[446446L]}
    {min=38593856; max=38593862; invalid=[38593859L]}
    {min=565653; max=565659; invalid=[565656L]}
    {min=824824821; max=824824827; invalid=[824824824L]}
    {min=2121212118; max=2121212124; invalid=[2121212121L]}
  ]
  result
  |> List.collect _.invalid
  |> List.sum
  |> should equal 4174379265L

[<Fact>]
let B() =
  let result = 
    raw 2
    |> parse
    |> List.map (findInvalid repeatsMany)
  result
  |> List.collect _.invalid
  |> List.sum
  |> should equal 43872163557L
