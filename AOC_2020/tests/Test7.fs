namespace Tests

open System
open System.Linq
open Xunit
open AOC_Common

module Test7 = 
  open AOC7

  let pretest = 
    "light red bags contain 1 bright white bag, 2 muted yellow bags.
    dark orange bags contain 3 bright white bags, 4 muted yellow bags.
    bright white bags contain 1 shiny gold bag.
    muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
    shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
    dark olive bags contain 3 faded blue bags, 4 dotted black bags.
    vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
    faded blue bags contain no other bags.
    dotted black bags contain no other bags."


  let ridiculous = 
    "shiny gold bags contain 2 dark red bags.
    dark red bags contain 2 dark orange bags.
    dark orange bags contain 2 dark yellow bags.
    dark yellow bags contain 2 dark green bags.
    dark green bags contain 2 dark blue bags.
    dark blue bags contain 2 dark violet bags.
    dark violet bags contain no other bags."

  let preProcess (block:string) =
    block.Split('\n', StringSplitOptions.TrimEntries) 
    |> Array.toList

  [<Fact>]
  let ``prevalidate 7a parsing`` () =
    Assert.Equal(594, n)
    let line1Bags = pretest |> preProcess |> List.head |> parseBag
    Assert.Equal(2, line1Bags.Length)
    Assert.Equal("bright white", line1Bags.[0].inside)
    Assert.Equal("muted yellow", line1Bags.[1].inside)
    let allBags = pretest |> preProcess |> parseBags
    Assert.Equal(13, allBags.Length)

  [<Fact>]
  let ``prevalidate 7a searching`` () =
    let allBags = pretest |> preProcess |> parseBags
    let containing = containedWithin allBags "shiny gold"
    Assert.Equal(["bright white"; "dark orange"; "light red"; "muted yellow"], containing.AsEnumerable())

  [<Fact>]
  let ``counting bags that could contain shiny gold bag`` () =
    let allBags = inputs |> parseBags
    let containing = containedWithin allBags "shiny gold"
    Assert.Equal(300, containing.Count)


  [<Fact>]
  let ``prevalidate 7b contained bags in shiny gold bag`` () =
    let allBags = pretest |> preProcess |> parseBags
    let contained = countInBag allBags "shiny gold"
    Assert.Equal(32, contained)

  [<Fact>]
  let ``prevalidate 7b on ridiculousness`` () =
    let allBags = ridiculous |> preProcess |> parseBags
    let contained = countInBag allBags "shiny gold"
    Assert.Equal(126, contained)

  [<Fact>]
  let ``Count the contained bags in shiny gold bag`` () =
    let allBags = inputs |> parseBags
    let contained = countInBag allBags "shiny gold"
    Assert.Equal(8030, contained)