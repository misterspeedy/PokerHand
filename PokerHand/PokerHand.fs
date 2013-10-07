namespace Poker

module Tuple =
    /// Does the equivalent to Seq.map, but from a tuple (2) to another tuple (2).
    let Map2 (f : 'T -> 'U) (inputs : ('T * 'T)) =
        f (inputs |> fst),
        f (inputs |> snd)

module PokerHand =

    open System

    /// A card suit.
    type Suit = 
        | Club  | Diamond | Heart | Spade
        static member FromChar c =
            match c with
            | '♣' -> Club | '♦' -> Diamond | '♥' -> Heart | '♠' -> Spade
            | _ -> raise (ArgumentException(sprintf "Invalid suit character: %c" c))

    /// A card rank.
    type Rank = 
        | Ace | King | Queen | Jack | Value of int
        static member FromString s =
            match s with
            | "A" -> Ace | "K" -> King | "Q" -> Queen | "J" -> Jack 
            | "10" -> Value(10)
            | _ when s.Length = 1 && s.[0] >= '2' && s.[0] <= '9' -> Value(int(s.[0]) - int('0'))
            | _ -> raise (ArgumentException(sprintf "Invalid rank string: %s" s))
        /// The value of the rank for comparison purposes.
        member r. SortValue = 
            match r with
            | Ace -> 14 // Aces high - low-ace cases are handled elsewhere
            | King -> 13
            | Queen -> 12
            | Jack -> 11
            | Value v -> v      

    /// A playing card.
    type Card = { Rank : Rank; Suit : Suit } 
        with 
        static member FromString (s : String) =
            let rank, suit =
                match s.Length with
                | 2 -> s.Substring(0, 1), s.[1]
                  // The 10 case has three characters
                | 3 -> s.Substring(0, 2), s.[2]
                | _ -> raise (ArgumentException("Invalid card string: ", s))
            { Rank = Rank.FromString rank; Suit = Suit.FromChar suit}

    /// A hand of exactly five cards.
    type Hand(cards : Card[]) =
        do
            if cards.Length <> 5 then
                raise (ArgumentException("Invalid number of cards"))
        /// The cards in the hand, sorted in descending order of rank.
        member h.Cards = cards |> Array.sortBy (fun card -> 0 - card.Rank.SortValue)
        static member FromString (s : string) =
            let cards = 
                s.Split([|' '|])
                |> Array.map (fun s' -> Card.FromString s')
            Hand cards

    /// The possible outcomes when comparing two Poker hands.
    type Outcome = Win | Draw | Lose
        with 
        static member FromRanks (rank1 : Rank) (rank2 : Rank) =
            let sortValue1, sortValue2 = rank1.SortValue, rank2.SortValue
            if sortValue1 > sortValue2 then
                Win
            elif sortValue1 < sortValue2 then
                Lose
            else
                Draw
        override o.ToString() =
            match o with
            | Win -> "Win" | Draw -> "Draw" | Lose -> "Lose"

    /// True if all cards in the sequence are the same suit.
    let SameSuit cards =
        ( cards |> Seq.distinctBy (fun card -> card.Suit) |> Seq.length ) = 1

    /// True if all the cards in the sequence are consecutive by value.
    /// Includes both the high-Ace and the low-Ace cases.
    let Consecutive cards =
        cards
        |> Array.map (fun card -> card.Rank)
        |> Array.sortBy (fun rank -> 0 - rank.SortValue)
        |> Seq.pairwise
        |> Seq.map (fun (r1, r2) -> // Handle the low-Ace case:
                                    if r1 = Ace && r2 = Value(2) then
                                        1
                                    // All other cases including high-Ace:
                                    else
                                        r1.SortValue - r2.SortValue
                    )
        |> Seq.filter (fun diff -> diff <> 1)
        |> Seq.isEmpty

    /// Returns the ranks and rank-counts of cards in the array:
    let RanksWithCount n cards =
        cards
        |> Seq.ofArray
        |> Seq.groupBy (fun card -> card.Rank)
        |> Seq.filter (fun (_, cards) -> (Seq.length cards) = n)
        |> Seq.map (fun (rank, _) -> rank)
        |> Seq.sortBy (fun rank -> rank.SortValue)
        |> Array.ofSeq
        
    /// Returns the rank counts (without the ranks themselves) of cards in the array.
    let RankCounts cards =
        cards
        |> Seq.ofArray
        |> Seq.countBy (fun card -> card.Rank) 
        |> Seq.map (fun (_, count) -> count)
        |> Seq.sortBy (fun count -> 0 - count) 
        |> Array.ofSeq

    /// Returns an array of ranks which appear only once each in the array.
    let SoloRanks = RanksWithCount 1 

    /// Returns an array of ranks which appear exactly twice each in the array.
    let PairRanks = RanksWithCount 2

    /// Returns the most common rank in the array.
    let CommonestRank cards =
        cards
        |> Seq.ofArray
        |> Seq.countBy (fun card -> card.Rank) 
        |> Seq.sortBy (fun (_, count) -> 0 - count) 
        |> Seq.map (fun (rank, _) -> rank)
        |> Seq.nth 0

    /// Returns the suit counts (without the suits themselves) of cards in the array.
    let SuitCounts cards =
        cards
        |> Seq.ofArray
        |> Seq.countBy (fun card -> card.Suit) 
        |> Seq.map (fun (_, count) -> count)
        |> Seq.sortBy (fun count -> 0 - count) 
        |> Array.ofSeq

    let SortRanks (ranks : Rank[]) =
        ranks |> Array.sortBy (fun r -> r.SortValue)

    // Partial active patterns for each of the recognized Poker hands.  These need to be
    // matched in order as some of the later cases will otherwise have false hits.

    let (|RoyalFlush|_|) (hand : Hand) =
        if (hand.Cards.[0].Rank) = Ace && (Consecutive hand.Cards) && (SameSuit hand.Cards) then
            RoyalFlush |> Some
        else
            None

    let (|StraightFlush|_|) (hand : Hand) =
        if (SameSuit hand.Cards) && (Consecutive hand.Cards) then
            StraightFlush |> Some
        else
            None
        
    let (|FourOfAKind|_|) (hand : Hand) =
        if (RankCounts hand.Cards).[0] = 4 then
            FourOfAKind |> Some
        else
            None

    let (|FullHouse|_|) (hand : Hand) =
        if (RankCounts hand.Cards) = [|3; 2|] then
            FullHouse |> Some
        else
            None

    let (|Flush|_|) (hand : Hand) =
        if (SuitCounts hand.Cards) = [|5|] then
            Flush |> Some
        else
            None

    let (|Straight|_|) (hand : Hand) =
        if (Consecutive hand.Cards) then
            Straight |> Some
        else
            None

    let (|ThreeOfAKind|_|) (hand : Hand) =
        if (RankCounts hand.Cards).[0] = 3 then
            ThreeOfAKind |> Some
        else
            None

    let (|TwoPair|_|) (hand : Hand) =
        if (RankCounts hand.Cards).[0..1] = [|2; 2|] then
            TwoPair |> Some
        else
            None

    let (|OnePair|_|) (hand : Hand) =
        if (RankCounts hand.Cards).[0] = 2 then
            OnePair |> Some
        else
            None

    /// Get the name of a hand type.
    let HandTypeName (hand : Hand) =
        match hand with
        | RoyalFlush -> "Royal Flush"
        | StraightFlush -> "Straight Flush"
        | FourOfAKind -> "Four of a Kind"
        | FullHouse -> "Full House"
        | Flush -> "Flush"
        | Straight -> "Straight"
        | ThreeOfAKind -> "Three of a Kind"
        | TwoPair -> "Two Pair"
        | OnePair -> "One Pair"
        | _ -> "High Card"

    /// Compare two equal length arrays of kickers until one of them wins.
    let CompareKickers (kickers1 : Rank[]) (kickers2 : Rank[]) =
        if kickers1.Length <> kickers2.Length then
            raise (ArgumentException("Kicker arrays must be of equal length"))
        let k1, k2 = (kickers1, kickers2) |> Tuple.Map2 SortRanks
        let rec Compare index =
            let outcome = Outcome.FromRanks kickers1.[index] kickers2.[index]
            if outcome <> Draw then
                outcome
            else if index < kickers1.Length-1 then
                Compare (index+1)
            else
                Draw
        Compare 0

    /// Compare two hands, the first of which can Win, Draw or Lose against the second.
    let CompareHands (hand1 : Hand) (hand2 : Hand) =
        match hand1, hand2 with

        | RoyalFlush, RoyalFlush ->
            raise (ArgumentException("Impossible combination: two Royal Flushes"))
        | RoyalFlush, _ -> Win
        | _, RoyalFlush -> Lose

        | StraightFlush, StraightFlush ->
            let rank1, rank2 = hand1.Cards.[0].Rank, hand2.Cards.[0].Rank
            Outcome.FromRanks rank1 rank2
        | StraightFlush, _ -> Win
        | _, StraightFlush -> Lose

        | FourOfAKind, FourOfAKind 
        | FullHouse, FullHouse ->
            let rank1, rank2 = (hand1.Cards, hand2.Cards) |> Tuple.Map2 CommonestRank
            Outcome.FromRanks rank1 rank2
        | FourOfAKind, _ -> Win
        | _, FourOfAKind -> Lose
        | FullHouse, _ -> Win     
        | _, FullHouse -> Lose           

        | Flush, Flush ->
            // Although normally we would determine on highest rank card, tie breakers
            // may be needed right down to the lowest card so it's easiest to think 
            // of the flush as consisting entirely of kickers.
            let kickers1, kickers2 = (hand1.Cards, hand2.Cards) |> Tuple.Map2 SoloRanks
            CompareKickers kickers1 kickers2
        | Flush, _ -> Win
        | _, Flush -> Lose

        | Straight, Straight ->
            let rank1, rank2 = hand1.Cards.[0].Rank, hand2.Cards.[0].Rank
            Outcome.FromRanks rank1 rank2
        | Straight, _ -> Win
        | _, Straight -> Lose

        | ThreeOfAKind, ThreeOfAKind ->
            let rank1, rank2 = (hand1.Cards, hand2.Cards) |> Tuple.Map2 CommonestRank
            Outcome.FromRanks rank1 rank2
        | ThreeOfAKind, _ -> Win
        | _, ThreeOfAKind -> Lose

        | TwoPair, TwoPair ->
            let ranks1, ranks2 = (hand1.Cards, hand2.Cards) 
                                 |> Tuple.Map2 (PairRanks >> SortRanks)
            if ranks1 <> ranks2 then 
                let topRank1, topRank2 = ranks1.[0], ranks2.[0]
                let topPairOutcome = Outcome.FromRanks topRank1 topRank2
                if topPairOutcome <> Draw then
                    topPairOutcome
                else
                    let secondRank1, secondRank2 = ranks1.[1], ranks2.[1]
                    Outcome.FromRanks secondRank1 secondRank2
            else
                let kickers1, kickers2 = (hand1.Cards, hand2.Cards) |> Tuple.Map2 SoloRanks
                CompareKickers kickers1 kickers2
        | TwoPair, _ -> Win
        | _, TwoPair -> Lose

        | OnePair, OnePair ->
            let rank1, rank2 = (hand1.Cards |> PairRanks).[0], (hand2.Cards |> PairRanks).[0]
            if rank1 <> rank2 then 
                Outcome.FromRanks rank1 rank2
            else
                let kickers1, kickers2 = (hand1.Cards, hand2.Cards) |> Tuple.Map2 SoloRanks
                CompareKickers kickers1 kickers2
        | OnePair, _ -> Win
        | _, OnePair -> Lose

        | _, _ ->
            let kickers1, kickers2 = (hand1.Cards, hand2.Cards) |> Tuple.Map2 SoloRanks
            CompareKickers kickers1 kickers2

    /// The outcome of comparing all the hands in a showdown - either
    /// a win, or two or more hands drawing
    type Showdown = Winner of HandIndex:int | Drawers of HandIndexes:int[]
        with
        static member FromHands (hands : Hand[]) =
            let handCount = hands.Length
            if handCount < 2 then
                raise (ArgumentException("Must have at least two hands"))
            let handsi = hands |> Array.mapi (fun i hand -> i, hand)

            let outcomes = seq {
                                for hand1 in handsi do
                                    for hand2 in handsi do
                                        let i, h1 = hand1
                                        let j, h2 = hand2
                                        if i <> j then
                                            yield i, CompareHands h1 h2            
                           } |> Seq.groupBy (fun (i, outcome) -> i)

            // An outright-winning hand is one which beats all other hands:
            let winsNeeded = handCount - 1
            let winsAll = outcomes
                          |> Seq.filter (fun (_, ocs) -> let wins = ocs
                                                                    |> Seq.map (fun (i, oc) -> oc)
                                                                    |> Seq.filter (fun o -> o = Win)
                                                                    |> Seq.length
                                                         wins = winsNeeded           
                                        )
                          |> Seq.map fst
            if not (Seq.isEmpty winsAll) then
                Winner (winsAll |> Seq.nth 0)
            else
                // If no hand wins outright, it must have been a draw, and the hands which share the win are
                // those which have no defeats:
                let drawers = outcomes
                              |> Seq.filter (fun (_, ocs) -> let defeats = ocs
                                                                           |> Seq.map (fun (i, oc) -> oc)
                                                                           |> Seq.filter (fun o -> o = Lose)
                                                                           |> Seq.length
                                                             defeats = 0           
                                            )
                              |> Seq.map fst
                Drawers (drawers |> Array.ofSeq)

