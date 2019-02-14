module Poker
    open System

    type Suit = 
        | Heart
        | Spade
        | Club
        | Diamond

    type Rank = 
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

    type Hand = Card list

    module Parsing = 
        let (|Number|_|) (c : char) = 
            match Int32.TryParse(string c) with
            | true, n  when n >=2 && n <= 10 -> enum<Rank>(n) |> Some
            | _ -> None

        let parseRank c = 
            match c with
            | Number r -> r
            | 'J' -> Rank.Jack
            | 'Q' -> Rank.Queen
            | 'K' -> Rank.King
            | _ -> failwith "Invalid rank"

        let parseSuit c =
            match c with
            | 'H' -> Heart
            | 'S' -> Spade
            | 'D' -> Diamond
            | 'C' -> Club
            | _ -> failwith "Invalid suit"

        let parseCard (card : string) = 
            match card.ToCharArray() |> List.ofArray with
            | [r; s] -> {Rank = parseRank r; Suit = parseSuit s}
            | _ -> failwith "Invalid card"

        let parseHand (hand : string) = 
            match hand.Split(' ') |> List.ofArray with
            | l when l.Length = 5 -> l |> List.map parseCard
            | _ -> failwith "Invalid hand"

    module Rules = 
        let highestCard hand = 
            hand 
            |> List.map (fun c -> c.Rank)
            |> List.max

        let compareHighestCards hands =
            hands
            |> List.sortByDescending highestCard
            |> List.tryHead

        let rules = [compareHighestCards]


    let compareHands hands = 
        Rules.rules
        |> Seq.choose (fun rule -> rule hands)
        |> Seq.head

    let bestHands hands = 
        hands
        |> List.map (fun hand -> (Parsing.parseHand hand, hand))
        |> compareHands
        

