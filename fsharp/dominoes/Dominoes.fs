module Dominoes

let selectNthDomino dominoes n =
    let nth = List.item n dominoes
    let others =
        dominoes
        |> List.mapi (fun i d -> if i = n then None else Some d)
        |> List.choose id

    (nth, others)

let makeChain ((x1, y1), others) =
    match others with
    | (x2, y2)::xs when y1 = x2 -> (x1, y2)::xs |> Some
    | (x2, y2)::xs when x1 = x2 -> (y1, y2)::xs |> Some
    | _ -> None


let rec canChain input = 
    match input with
    | [] -> true
    | [(x, y)] when x = y -> true
    | _ -> 
        [0..(List.length input) - 1]
        |> List.map (selectNthDomino input)
        |> List.choose makeChain
        |> List.map canChain
        |> List.exists id
