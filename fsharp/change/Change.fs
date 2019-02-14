module Change

let largestCoin coins target = 
    coins |> List.tryFind (fun c -> c <= target)

let cons a b = 
    match (a, b) with
    | _, None -> None
    | a, Some b -> a :: b |> Some

let findFewestCoins coins target =
    let coins = List.rev coins
    let rec findFewestCoinsImpl target =
        if target = 0 then
            Some []
        else           
            match largestCoin coins target with
            | None -> None
            | Some c -> cons c (findFewestCoinsImpl (target - c))

    findFewestCoinsImpl target |> Option.map (List.rev)
