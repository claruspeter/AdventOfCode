module AOC4

open System
open AOC_Common
open System.Collections.Generic
open FSharp.Scanf.Scanf
open System.Text.RegularExpressions


let inputs = 
  raw 4 
  |> toMultiLineBlocks

let n = inputs.Length

type Passport = {
  byr: string option
  cid: string option
  ecl: string option
  eyr: string option
  hcl: string option
  hgt: string option
  iyr: string option
  pid: string option
}

type EyeColours = Amb = 1 | Blu = 2 | Brn = 3 | Gry = 4 | Grn = 5 | Hzl = 6 | Oth = 99

type CompletePassport = {
  byr: int
  cid: string option
  ecl: string
  eyr: int
  hcl: string
  hgt: Distance
  iyr: int
  pid: string
}

let private orOption (dict: IDictionary<'T, 'R>) (t: 'T): 'R option =
  match dict.ContainsKey t with
  | false -> None
  | true -> dict.[t] |> Some

let parsePassport (line:string) =
  let parts = line.Split(' ');
  let values = 
    parts 
    |> Array.map (fun p -> 
      let x = p.Split(':')
      (x.[0], x.[1])
    )
    |> dict
  let lookup = orOption values
  {
    Passport.byr=lookup "byr" 
    cid=lookup "cid" 
    ecl=lookup "ecl" 
    eyr=lookup "eyr" 
    hcl=lookup "hcl" 
    hgt=lookup "hgt" 
    iyr=lookup "iyr" 
    pid=lookup "pid" 
  }

let passports data =
  data
  |> Array.map parsePassport

let isPassportOrNorth (passport: Passport) =
  passport.byr.IsSome &&
  passport.ecl.IsSome &&
  passport.eyr.IsSome &&
  passport.hcl.IsSome &&
  passport.hgt.IsSome &&
  passport.iyr.IsSome &&
  passport.pid.IsSome

let asComplete (passport: Passport) =
  {
    CompletePassport.byr = passport.byr.Value |> Int32.Parse
    cid = passport.cid
    ecl = passport.ecl.Value
    eyr = passport.eyr.Value |> Int32.Parse
    hcl = passport.hcl.Value
    hgt = passport.hgt.Value |> Distance.Parse
    iyr = passport.iyr.Value |> Int32.Parse
    pid = passport.pid.Value
  }

let completePassports data= 
  passports data
  |> Array.filter isPassportOrNorth
  |> Array.map asComplete

let private between label min max v =
  match v >= min && v <= max with 
  | true -> None
  | false -> 
    sprintf "%s: BETWEEN %4d <= %4d <= %4d" label min v max
    |> Some

let private validHeight ht =
  match ht with  
  | InCms x -> x / 1<cm> |> between "height" 150 193
  | InInches x -> x / 1<inch> |> between "height" 59 76
  | _ -> Some (sprintf "Not a valid Height %A"  ht)

let private strLength label (n:int) (s:string) =
  match s.Length = n with 
  | true -> None
  | false -> sprintf "%s: String %A was not %d letters long" label s n |> Some

let private validRegex regex label s = 
  match Regex.IsMatch(s, regex) with 
  | false -> sprintf "Not a match for %s: %A" label s |> Some
  | true -> None

let private validEyeColour s =
  Enum.GetNames(typeof<EyeColours>)
  |> Array.map (fun x -> x.ToLowerInvariant())
  |> Array.contains s
  |>  function 
      | true -> None
      | false -> sprintf "Invalid eye colour: %s" s |> Some

let deref (p: 'A) (result: string option) =
  match result with 
  | None -> Ok p
  | Some s -> Error s

let isPassportValid (p: CompletePassport) =
  let rules : list<CompletePassport -> Result<CompletePassport, string>> = [
    fun x -> x.byr |> between "byr" 1920 2002 |> deref x
    fun x -> x.iyr |> between "iyr" 2010 2020 |> deref x
    fun x -> x.eyr |> between "eyr" 2020 2030 |> deref x
    fun x -> x.hgt |> validHeight |> deref x
    fun x -> x.hcl |> (strLength "Hair Colour" 7 >==> validRegex "\\#[0-9a-f]{6}" "Hair colour") |> deref x
    fun x -> x.ecl |> validEyeColour |> deref x
    fun x -> x.pid |> (strLength "Pid" 9 >==> validRegex "[0-9]{9}" "Pid") |> deref x
  ]
  let ruleset =  rules |> List.reduce (>=>)
  ruleset p
  

let isOk =
  function 
  | Ok _ -> true
  | _ -> false

let validPassports data = 
  completePassports data
  |> Array.filter (isPassportValid >> isOk)