module AllYourBase

let parse inputBase digits = 
    digits
    |> List.rev
    |> List.mapi (fun i d -> d * pown inputBase i)
    |> List.sum

let toDigits base' n = 

    let rec toDigitsImpl base' digits n = 
        match (n / base',  n % base') with
        | 0, r -> r::digits
        | q, r -> toDigitsImpl base' (r::digits) q

    toDigitsImpl base' [] n
    
let rebase digits inputBase outputBase = 
    digits |> parse inputBase |> toDigits outputBase
