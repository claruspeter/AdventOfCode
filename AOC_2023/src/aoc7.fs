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

type Hand = {
  cards: string
  bid: int
}with 
  member hand.handType =
    let histoValues = 
      hand.cards
      |> histogram 
      |> fun x -> x.Values 
      |> Seq.sortDescending
      |> Seq.toList
    match histoValues with 
    | [5] -> HandType.FiveOfAKind
    | [4; 1] -> HandType.FourOfAKind
    | [3; 2] -> HandType.FullHouse
    | [3; 1; 1] -> HandType.ThreeOfAKind
    | [2; 2; 1] -> HandType.TwoPair
    | 2::_ -> HandType.OnePair
    | _ -> HandType.HighCard


let private handComparer (a:Hand) (b:Hand) =
  match a.handType, b.handType with 
  | x, y when x <> y -> x.CompareTo(y)
  | _ -> 
      let aValues = a.cards |> Seq.map (CardValues.IndexOf)
      let bValues = b.cards |> Seq.map (CardValues.IndexOf)
      Seq.compareWith (fun (aV:int) (bV:int) -> aV.CompareTo(bV)) aValues bValues

let private parseHand (line: string) =
  match line.Split(' ')with 
  | [|cards; bid|] -> {cards=cards; bid=int bid} |> Some
  | _ -> None

let parseAndSortHands (lines: string list) =
  lines
  |> List.choose parseHand
  |> List.sortWith handComparer