 module Parsing
    open System
    open Types
    
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