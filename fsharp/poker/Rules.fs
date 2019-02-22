 module Rules
    open Types

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
    
    let compareThreeOfAKind hands =
        hands
        |> compareRule (hasGroupOfSize 3)
        
    let compareFourOfAKind (hands: Hand list) = 
        hands
        |> List.choose (hasGroupOfSize 4)
        |> List.sortByDescending (fun (r, _) -> r)
        |> List.groupBy (fun (r, _h) -> r)
        |> List.tryHead
        |> fun x -> x
        |> function 
            | None -> None
            | Some (_, [(_,singleHand)]) -> Some singleHand
            | Some (_, multipleHands) -> 
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

    let compareTwoPairs = comparePairs 2
    let compareSinglePairs = comparePairs 1

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