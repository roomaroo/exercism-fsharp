module OcrNumbers

open System
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

let rec splitDigits lists = 
  match lists with
  | [h1::t1;h2::t2;h3::t3;h4::t4] -> [h1;h2;h3;h4]::(splitDigits [t1;t2;t3;t4])
  | _ -> []

let toString (chars: char seq) = 
  chars |> System.String.Concat

let isCorrectSize input = 
    List.map Seq.length input = [3;3;3;3]

let parse input = 
    match Map.tryFind input numbers with
    | Some number -> Some number
    | None when isCorrectSize input -> Some "?"
    | _ -> None

let combineOptions fn a b = 
    match a, b with
    | Some a', Some b' -> Some(fn a' b')
    | _ -> None
 
let join a b = 
    if String.length a = 0 then
        b
    else if String.length b = 0 then
        a
    else
        String.Join(",", a, b)

let convertLine rows = 
    if List.length rows = 4 then
        rows
        |> List.map (Seq.chunkBySize 3 >> List.ofSeq)
        |> splitDigits 
        |> List.map (List.map toString >> parse)
        |> List.fold (combineOptions (+)) (Some "")
    else
        None    

let convert rows = 
    rows
    |> List.chunkBySize 4
    |> List.map convertLine
    |> List.fold (combineOptions join) (Some "")    