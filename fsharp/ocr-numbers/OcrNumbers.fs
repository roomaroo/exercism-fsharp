module OcrNumbers

let numbers = 
    Map.ofList [
        [" _ ";
         "| |";
         "|_|";
         "   "], "0";
        ["   ";
         "  |";
         "  |";
         "   "], "1";
        [" _ ";
         " _|";
         "|_ ";
         "   "], "2";
        [" _ ";
         " _|";
         " _|";
         "   "], "3";
        ["   ";
         "|_|";
         "  |";
         "   "], "4";
        [" _ ";
         "|_ ";
         " _|";
         "   "], "5";
        [" _ ";
         "|_ ";
         "|_|";
         "   "], "6";
        [" _ ";
         "  |";
         "  |";
         "   "], "7";
        [" _ ";
         "|_|";
         "|_|";
         "   "], "8";
        [" _ ";
         "|_|";
         " _|";
         "   "], "9";
    ]

let isCorrectSize input = 
    List.map Seq.length input = [3;3;3;3]
    
let charsToString (chars: char seq) = 
    chars |> System.String.Concat

let parseDigit input = 
    match Map.tryFind input numbers with
    | Some num -> Some num
    | None when isCorrectSize input -> Some "?"
    | _ -> None

let rec splitDigits (input: string list list) =
    match input with
    | [h1::t1;h2::t2;h3::t3;h4::t4] -> [h1;h2;h3;h4]::splitDigits [t1;t2;t3;t4]
    | _ -> []

let combineOptions f a b =
    match a, b with
    | Some a', Some b' -> Some(f a' b')
    | _ -> None 
    
let join a b = 
    if String.length a = 0 then 
        b
    else if String.length b = 0 then
        a
    else      
        System.String.Join(",", a, b)

let parseLine (line: string list) = 
    if List.length line = 4 then
        line 
        |> List.map (Seq.chunkBySize 3 >> Seq.map charsToString >> List.ofSeq)
        |> splitDigits
        |> List.map parseDigit
        |> List.fold (combineOptions (+)) (Some "")
    else
        None

let convert (lines: string list) = 
    if List.length lines % 4 = 0 then
        lines
        |> List.chunkBySize 4
        |> List.map parseLine
        |> List.fold (combineOptions join) (Some"")
    else
        None

    