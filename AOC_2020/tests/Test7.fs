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
    dotted black bags contain no other bags.".Split('\n', StringSplitOptions.TrimEntries) |> Array.toList

  [<Fact>]
  let ``prevalidate 7a parsing`` () =
    Assert.Equal(594, n)
    let line1Bags = parseBag pretest.[0]
    Assert.Equal(2, line1Bags.Length)
    Assert.Equal("bright white", line1Bags.[0].inside)
    Assert.Equal("muted yellow", line1Bags.[1].inside)
    let allBags = pretest |> parseBags
    Assert.Equal(13, allBags.Length)

  [<Fact>]
  let ``prevalidate 7a searching`` () =
    let allBags = pretest |> parseBags
    let containing = containedWithin allBags "shiny gold"
    Assert.Equal(["bright white"; "dark orange"; "light red"; "muted yellow"], containing.AsEnumerable())

  [<Fact>]
  let ``Count bags that could contain shiny gold bag`` () =
    let allBags = inputs |> parseBags
    let containing = containedWithin allBags "shiny gold"
    Assert.Equal(300, containing.Count)


