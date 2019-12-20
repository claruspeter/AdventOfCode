module Tests

open System
open Xunit

[<Fact>]
let ``AOC1`` () =
    Assert.Equal(3297909, AOC1.data)
    Assert.Equal(4943994, AOC1.data_b)


[<Fact>]
let ``AOC2 tests`` () =
    Assert.Equal(0, AOC2.test1())
    Assert.Equal(0, AOC2.test2())
    Assert.Equal(0, AOC2.test3())
    Assert.Equal(0, AOC2.test4())

[<Fact>]
let ``AOC2 1202Error`` () =
    Assert.Equal(2842648, AOC2.generate1202Error())

[<Fact>]
let ``AOC2 Target`` () =
    let target = AOC2.findTarget()
    Assert.Equal(90, target.noun)
    Assert.Equal(74, target.verb)

[<Fact>]
let ``AOC3a tests`` () =
    Assert.Equal(6, AOC3.minDistance 0 1)
    Assert.Equal(146, AOC3.minDistance 2 3)
    Assert.Equal(135, AOC3.minDistance 4 5)

[<Fact>]
let ``AOC3a target`` () =    
    Assert.Equal(293, AOC3.minDistance 6 7)

[<Fact>]
let ``AOC3b tests`` () =
    Assert.Equal(30, AOC3.minPath 0 1)
    Assert.Equal(610, AOC3.minPath 2 3)
    Assert.Equal(410, AOC3.minPath 4 5)

[<Fact>]
let ``AOC3b target`` () =    
    Assert.Equal(27306, AOC3.minPath 6 7)

[<Fact>]
let ``AOC4a`` () =    
    Assert.Equal(544, AOC4.partA)

[<Fact>]
let ``AOC4b`` () =    
    Assert.Equal(334, AOC4.partB)

[<Fact>]
let ``AOC5a`` () =    
    Assert.Equal(42, AOC5.test1())