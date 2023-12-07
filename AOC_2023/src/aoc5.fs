module AOC2023.Day5

open System
open AOC2023.Common

type private AlmanacMapping = {
  source: int64
  destination: int64
  size: int64
}

let private almanacMapping m =
  fun input ->
    if input >= m.source && input < m.source + m.size then
      m.destination + (input - m.source) |> Some
    else
      None

let private reverseMapping m =
  fun output ->
    if output >= m.destination && output < m.destination + m.size then
      m.source + (output - m.destination) |> Some
    else
      None

type private AlmanacMap = {
  name: string
  maps: AlmanacMapping list
}with 
  member this.mapping = this.maps |> List.map almanacMapping
  member this.reverseMapping = this.maps |> List.map reverseMapping
  member this.map input =
    match input |> choose this.mapping with 
    | None -> input
    | Some x -> x
  member this.rev output =
    output 
    |> choose this.reverseMapping

let seed_soil = "seed-to-soil"
let soil_fertilizer = "soil-to-fertilizer"
let fertilizer_water = "fertilizer-to-water"
let water_light = "water-to-light"
let light_temperature = "light-to-temperature"
let temperature_humidity = "temperature-to-humidity"
let humidity_location = "humidity-to-location"

let private parseMapping (name: string) (mapLines:string list) = 
  let mapFactors =
      mapLines 
      |> List.map (
        fun line -> 
          line.Split([|' '|])
          |> Array.map int64
      )
  {
    name = name.Substring(0, name.Length - 5)
    maps = mapFactors |> List.map (fun parts -> {destination=parts.[0]; source=parts.[1]; size=parts.[2]})
  }

let private parseSeeds (line: string) = 
  line.Substring(7).Split([|' '|]) |> Array.map int64 |> Array.toList

type private SeedRange = {
  start: int64
  size: int64
}with 
  member this.contains input = input >= this.start && input < this.start + this.size

let private parseSeedRanges (line: string) = 
  line.Substring(7).Split([|' '|]) 
  |> Array.map int64 
  |> Seq.chunkBySize 2
  |> Seq.map (fun x -> {start=x.[0]; size=x.[1] })

let private parseMaps (mapLines:string list) =
  mapLines
    |> List.mapi (fun i line ->  if line.Length > 0 && ['a'..'z'] |> List.contains (line.[0]) then Some i else None )
    |> List.choose id
    |> fun x -> x @ [mapLines.Length]
    |> List.pairwise
    |> List.map ( fun (a, b) -> parseMapping mapLines.[a] (mapLines.[(a+1)..(b-2)]))
    |> List.map (fun x -> (x.name, x))
    |> dict

let findSeedLocations (lines: string list) =
  let seeds = parseSeeds lines.[0]
  let mapLines = 
    lines
    |> List.skip 2
  let maps = parseMaps mapLines
  let fullMap = 
    maps.[seed_soil].map
    >> maps.[soil_fertilizer].map
    >> maps.[fertilizer_water].map
    >> maps.[water_light].map
    >> maps.[light_temperature].map
    >> maps.[temperature_humidity].map
    >> maps.[humidity_location].map
  seeds
  |> List.map fullMap

let findClosestLocationFromSeedRanges (lines: string list) =
  let seeds = parseSeedRanges lines.[0]
  let mapLines = 
    lines
    |> List.skip 2
  let maps = parseMaps mapLines
  let loc_maps = maps[humidity_location].maps |> List.sortBy (fun x -> x.destination)
  let loc = loc_maps.Head
  let fullRevMap = 
    maps[humidity_location].rev
    >==> maps[temperature_humidity].rev


  let qqq = 
    seq [loc.destination..(loc.size + loc.destination - 1L)]
    |> Seq.map fullRevMap

  printfn "%A" qqq

  46