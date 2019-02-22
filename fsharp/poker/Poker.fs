module Poker
    open Types
    open Rules  

    type Rule = Hand list -> Hand option

    let rules : Rule list = [
        compareStraightFlush;
        compareFourOfAKind;
        compareFullHouse;
        compareFlushes;
        compareStraights;
        compareThreeOfAKind;
        compareTwoPairs;
        compareSinglePairs;
    ]

    let applyRules hands =
        rules
        |> Seq.choose (fun r -> r hands)
        |> Seq.tryHead
        |> function
            | None -> (compareHighestCards hands)
            | Some h -> List.singleton h

    let bestHands hands = 
        hands
        |> List.map Parsing.parseHand
        |> applyRules
        |> List.map snd
        

