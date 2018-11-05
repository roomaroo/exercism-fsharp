module Bob

open System

let (|Question|_|) (input: string) =
    if input.TrimEnd().EndsWith("?") then Some Question else None

let (|Yell|_|) input = 
    let letters = Seq.filter Char.IsLetter input
    if Seq.length letters > 0 && Seq.forall Char.IsUpper letters then
        Some Yell
    else
        None

let (|Silence|_|) input = 
    if String.IsNullOrWhiteSpace(input) then Some Silence else None

let response input = 
    match input with
    | Yell & Question -> "Calm down, I know what I'm doing!"
    | Yell -> "Whoa, chill out!"
    | Question -> "Sure."
    | Silence -> "Fine. Be that way!"
    | _ -> "Whatever."