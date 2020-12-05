namespace Tests

open System
open System.Linq
open Xunit
open AOC_Common

module Test4 = 
  open AOC4

  let processValidity s =
    s
    |> parsePassport
    |> asComplete
    |> isPassportValid

  [<Fact>]
  let ``Count the complete passports with cid optional`` () =
    Assert.Equal(285, n)
    Assert.Equal(285, passports inputs |> Array.length)
    Assert.Equal(208, completePassports inputs |> Array.length)


  [<Fact>]
  let ``Pre validate`` () =
    let a = "eyr:1972 cid:100 hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926" |> processValidity
    Assert.Equal(Error "eyr: BETWEEN 2020 <= 1972 <= 2030", a)
    let b = "iyr:2019 hcl:#602927 eyr:1967 hgt:170cm ecl:grn pid:012533040 byr:1946" |> processValidity
    Assert.Equal(Error "eyr: BETWEEN 2020 <= 1967 <= 2030", b)
    let c = "hcl:dab227 iyr:2012 ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277" |> processValidity
    Assert.Equal(Error "Hair Colour: String \"dab227\" was not 7 letters long", c)    
    let d = "hgt:59cm ecl:zzz eyr:2038 hcl:74454a iyr:2023 pid:3556412378 byr:2007" |> processValidity
    Assert.Equal(Error "byr: BETWEEN 1920 <= 2007 <= 2002", d)        
    Assert.True("pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f" |> processValidity |> isOk)
    Assert.True("eyr:2029 ecl:blu cid:129 byr:1989 iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm" |> processValidity |> isOk)
    Assert.True("hcl:#888785 hgt:164cm byr:2001 iyr:2015 cid:88 pid:545766238 ecl:hzl eyr:2022" |> processValidity |> isOk)
    Assert.True("iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719" |> processValidity |> isOk)

  [<Fact>]
  let ``Count the valid passports`` () =
    Assert.Equal(167, validPassports inputs |> Array.length)