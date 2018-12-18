module PigLatin

open System

let inList list s = List.contains s list

let isVowel (s: string) = 
    inList ["a";"e";"i";"o";"u"] s

let isConsonant (s: string) = 
    s.Length = 1 && Char.IsLetter (Seq.head s) && not (isVowel s)

let split (word: string) length = 
    if word.Length < length then
        (word, "")
    else
        word.Substring(0, length), word.Substring(length)

let (|InitialVowel|_|) (word:string) =
    if isVowel (word.Substring(0, 1)) then
        Some (word + "ay")
    else 
        None

let (|InitialConsonant|_|) (word:string) =
    match split word 1 with
    | (x, xs) when isConsonant x -> Some (xs + x + "ay")
    | _ -> None

let (|TwoCharPrefix|_|) (word:string) =
    match split word 2 with
    | (x,xs) when inList ["ch";"qu";"th"] x -> Some(xs + x + "ay")
    | _ -> None

let (|ThreeCharPrefix|_|) (word:string) =
    match split word 3 with
    | (x,xs) when inList ["sch";"thr"] x -> Some(xs + x + "ay")
    | _ -> None

let (|YtXrSpecialCase|_|) (word:string) =
    match split word 2 with
    | (x,_) when inList ["xr";"yt"] x -> Some(word + "ay")
    | _ -> None

let (|ConsonantQuSpecialCase|_|) (word:string) =
    let (x, xs) = split word 3
    match split x 1 with
    | (y, ys) when isConsonant y && ys = "qu" -> Some (xs + x + "ay")
    | _ -> None

let (|YAfterConsonants|_|) (word:string) =
    match Seq.tryFindIndex (fun c -> c = 'y') (word.ToLower()) with
    | Some index when index > 0 && word |> Seq.take index |> Seq.forall (string >> isConsonant) -> 
        let (x, xs) = split word index
        Some (xs + x + "ay")
    | _ -> None    

let translateWord (input:string) = 
    match input with
    | ThreeCharPrefix result -> result
    | ConsonantQuSpecialCase result -> result
    | YAfterConsonants result -> result
    | TwoCharPrefix result -> result
    | YtXrSpecialCase result -> result
    | InitialVowel result -> result
    | InitialConsonant result -> result
    | _ -> input

let translate (input:string) = 
    input.Split([|' '|])
    |> Array.map translateWord
    |> String.concat " "

 