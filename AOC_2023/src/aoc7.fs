module AOC2023.Day7

open System
open System.Collections.Generic
open AOC2023.Common


let private histogram (input: 'a seq) =
  input
  |> Seq.fold (fun (acc: Dictionary<'a, int>) a -> 
      if acc.ContainsKey a then
        acc.[a] <- acc.[a] + 1
      else
        acc.[a] <- 1
      acc
    )
    (new Dictionary<'a, int>())

let CardValues = "23456789TJQKA"

type HandType = 
  | HighCard      = 1
  | OnePair       = 2
  | TwoPair       = 3
  | ThreeOfAKind  = 4
  | FullHouse     = 5
  | FourOfAKind   = 6
  | FiveOfAKind   = 7 

let private promoteHandTypeWithWildcards n handType =
  if n > 0  then 
    let simpleCountHands = [HandType.HighCard; HandType.OnePair; HandType.ThreeOfAKind; HandType.FourOfAKind; HandType.FiveOfAKind]
    let clamped = Math.Min(n, 4)
    match handType with
    | HandType.HighCard     -> simpleCountHands.[Math.Max(4, clamped)]
    | HandType.OnePair      -> simpleCountHands.[Math.Max(4, clamped+1)]
    | HandType.TwoPair      -> HandType.FullHouse
    | HandType.ThreeOfAKind -> simpleCountHands.[Math.Max(4, clamped+2)]
    | HandType.FullHouse    -> HandType.FullHouse
    | HandType.FourOfAKind  -> HandType.FiveOfAKind
    | HandType.FiveOfAKind  -> HandType.FiveOfAKind
    | x -> failwithf "Not Possible %A" x
  else
    handType

type Hand = {
  cards: string
  bid: int
  wildCard: char
}with 
  member hand.handType  =
    let hist = hand.cards |> histogram 
    let nWild = hist.OrDefault 0 hand.wildCard
    let histValues = 
      hist
      |> fun h -> 
          h.Remove(hand.wildCard) |> ignore
          h 
      |> fun x -> x.Values 
      |> Seq.sortDescending
      |> Seq.toList
    match histValues with 
    | [5] -> HandType.FiveOfAKind
    | 4::_ -> HandType.FourOfAKind
    | [3; 2] -> HandType.FullHouse
    | 3:: _ -> HandType.ThreeOfAKind
    | [2; 2]  
    | [2; 2; 1] -> HandType.TwoPair
    | 2::_ -> HandType.OnePair
    | _ -> HandType.HighCard
    |> promoteHandTypeWithWildcards nWild


let private handComparer (a:Hand) (b:Hand) =
  let cardValuesWithWildCard = sprintf "%c%s" a.wildCard CardValues
  match a.handType, b.handType with 
  | x, y when x <> y -> x.CompareTo(y)
  | _ -> 
      let aValues = a.cards |> Seq.map (cardValuesWithWildCard.IndexOf)
      let bValues = b.cards |> Seq.map (cardValuesWithWildCard.IndexOf)
      Seq.compareWith (fun (aV:int) (bV:int) -> aV.CompareTo(bV)) aValues bValues

let private parseHand wildCard (line: string) =
  match line.Split(' ')with 
  | [|cards; bid|] -> {cards=cards; bid=int bid; wildCard=wildCard} |> Some
  | _ -> None

let parseAndSortHands (lines: string list) =
  lines
  |> List.choose (parseHand '_')
  |> List.sortWith handComparer

let parseAndSortHandsWithJacksWild (lines: string list) =
  lines
  |> List.choose (parseHand 'J')
  |> List.sortWith handComparer