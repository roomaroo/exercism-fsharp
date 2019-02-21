module Poker
    open System
    open System.Collections.Generic

    type Suit = 
        | Heart
        | Spade
        | Club
        | Diamond

    type Rank = 
        | LowAce = 1
        | Two = 2
        | Three = 3
        | Four = 4
        | Five = 5
        | Six = 6
        | Seven = 7
        | Eight = 8
        | Nine = 9
        | Ten = 10
        | Jack = 11
        | Queen = 12
        | King = 13
        | Ace = 14

    type Card = 
        {Rank : Rank; Suit : Suit}

    type Hand = Card list * string

    module Parsing = 
        let (|Number|_|) (s : string) = 
            match Int32.TryParse(s) with
            | true, n  when n >=2 && n <= 10 -> enum<Rank>(n) |> Some
            | _ -> None

        let parseRank s = 
            match s with
            | Number r -> r
            | "J" -> Rank.Jack
            | "Q" -> Rank.Queen
            | "K" -> Rank.King
            | "A" -> Rank.Ace
            | _ -> failwith (sprintf "Invalid rank - %s" s)

        let parseSuit c =
            match c with
            | "H" -> Heart
            | "S" -> Spade
            | "D" -> Diamond
            | "C" -> Club
            | _ -> failwith "Invalid suit"

        let parseCard (card : string) = 
            let r = card.Substring(0, card.Length - 1)
            let s = card.Substring(card.Length - 1)
            {Rank = parseRank r; Suit = parseSuit s}
            
        let parseHand (hand : string) = 
            match hand.Split(' ') |> List.ofArray with
            | l when l.Length = 5 -> (l |> List.map parseCard, hand)
            | _ -> failwith "Invalid hand"

    module Rules = 
        
        let groupByRank hand =
            hand
            |> fst
            |> List.groupBy (fun c -> c.Rank)

        let groups size (hand: Hand) =
            hand
            |> groupByRank
            |> List.filter (fun (_, cards) -> cards.Length = size)
            |> List.map (fun (rank, _) -> (rank, hand))

        let hasGroupOfSize size hand = 
            hand
            |> groups size
            |> List.tryHead

        let hasKicker hand = 
            hand
            |> groupByRank
            |> List.filter (fun (_, group) -> group.Length = 1)
            |> List.map (fun (rank, _) -> rank)
            |> fun l -> 
                match l with 
                | [k] -> Some (k, hand)
                | _ -> None

        let highestScored (scoredHands: ('a * Hand) list) =
            let highest = 
                scoredHands
                |> List.map (fun (r, _) -> r)
                |> List.max

            scoredHands
            |> List.filter (fun (r, _) -> r = highest)
            |> function
               | [(_, h)] -> Some h // Single hand has highest score
               | _ -> None // Multiple hands have highest score, use next rule

        let compareRule rule (hands: Hand list) =
            hands
            |> List.choose rule
            |> function
               | [] -> None // no hands have X of a kind
               | [(_, h)] -> Some h // one hand has X of a kind
               | hs -> highestScored hs
        
        let compareXOfAKind groupSize hands =
            hands
            |> compareRule (hasGroupOfSize groupSize)
            
        let compareFourOfAKind (hands: Hand list) = 
            hands
            |> List.choose (hasGroupOfSize 4)
            |> function 
                | [(_, singleHand)] -> Some singleHand
                | multipleHands -> 
                        multipleHands
                        |> List.map (fun (_, hand) -> hand)
                        |> compareRule hasKicker

        let pairs (hand: Hand) =
            hand
            |> fst
            |> List.groupBy (fun c -> c.Rank)
            |> List.filter (fun (r, cards) -> cards.Length = 2)
            |> List.map (fun (r, _) -> r)
            |> List.sortDescending
            |> fun ranks -> (ranks, hand)

        let compareKickers = 
            compareRule hasKicker

        let comparePairs pairCount (hands: Hand list) =
            hands
            |> List.map pairs
            |> List.filter (fun (ranks, _) -> ranks.Length = pairCount)
            |> List.groupBy (fun (ranks, _) -> ranks)
            |> List.sortByDescending (fun (ranks, _) -> ranks)
            |> List.tryHead
            |> function
               | None -> None // no pairs
               | Some (_, [(_, hand)]) -> Some hand // one 
               | Some (_, results) -> 
                    results 
                    |> List.map (fun (_, hand) -> hand) 
                    |> compareKickers // multiple hands have same pairs

        let ranks (hand: Hand) = 
                hand
                |> fst
                |> List.map (fun c -> c.Rank)
                |> List.sortDescending

        let compareHighestCards (hands : Hand list) =
            let highestRanks =
                hands
                |> List.map ranks
                |> List.max

            hands
            |> List.filter (fun h -> ranks h = highestRanks)

        let isStraight (hand: Hand) = 
            hand
            |> fst
            |> List.map (fun c -> c.Rank)
            |> List.sort
            |> fun ranks -> 
                    let numbers = List.map int ranks
                    if numbers = [(List.min numbers) .. (List.max numbers)] then
                        Some (ranks, hand)
                    else if ranks = [Rank.Two;Rank.Three;Rank.Four;Rank.Five;Rank.Ace] then
                        Some ([Rank.LowAce;Rank.Two;Rank.Three;Rank.Four;Rank.Five], hand)
                    else 
                        None

        let compareStraights =
            compareRule isStraight

        let allSameSuit hand = 
            hand
            |> fst
            |> List.distinctBy (fun c -> c.Suit)
            |> fun l -> l.Length = 1

        let isFlush (hand: Hand) = 
            if allSameSuit hand then
                hand
                |> ranks
                |> fun r -> Some (r, hand)
            else
                None

        let compareFlushes = compareRule isFlush

        let isStraightFlush (hand: Hand) = 
            isFlush hand 
            |> Option.map (fun (_, h) -> h)
            |> Option.bind isStraight
            
        let compareStraightFlush = 
            compareRule isStraightFlush

        let isFullHouse hand =
            let tripleRank = hand |> hasGroupOfSize 3 |> Option.map (fun (r, _) -> r)
            let pairRank = hand |> hasGroupOfSize 2 |> Option.map (fun (r, _) -> r)
            match (tripleRank, pairRank) with
            | Some t, Some p when t <> p -> Some ([t;p], hand)
            | _ -> None

        let compareFullHouse = compareRule isFullHouse

        let rules = [
            compareStraightFlush;
            compareFourOfAKind;
            compareFullHouse;
            compareFlushes;
            compareStraights;
            compareXOfAKind 3;
            comparePairs 2;
            comparePairs 1;
        ]

        let applyRules hands =
            rules
            |> Seq.choose (fun r -> r hands)
            |> Seq.tryHead
            |> function
                | None -> (compareHighestCards hands)
                | Some h -> List.singleton h

    let compareHands (hands : Hand list) = 
        Rules.applyRules hands

    let bestHands hands = 
        hands
        |> List.map Parsing.parseHand
        |> compareHands
        |> List.map snd
        

