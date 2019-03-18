module LargestSeriesProduct

open System

let parseChar c =
    match Int32.TryParse(string c) with
    | true, n -> Some n
    | _ -> None

let product input = 
    input
    |> Seq.fold (*) 1

let calcLargestProduct input seriesLength : int option = 
    input
    |> Seq.windowed seriesLength
    |> Seq.map product
    |> List.ofSeq
    |> function
    | [] -> None
    | s -> s |> Seq.max |> Some

let largestProduct (input: string) seriesLength : int option =

    let lift s = 
        if Seq.contains None s then None
        else Some (Seq.choose id s)

    let input' = 
        input
        |> Seq.map parseChar
        |> lift

    match input', seriesLength with
    | None, _ -> None
    | _, n when n < 0 -> None
    | _, 0 -> Some 1
    | Some numbers, n -> calcLargestProduct numbers n