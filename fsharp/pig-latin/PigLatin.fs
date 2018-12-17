module PigLatin

let translate (input:string) = 

    let toString (cs: char seq) = System.String.Concat cs

    let isVowel c = 
        List.contains c ['a';'e';'i';'o';'u']

    match List.ofSeq input with
    | x::_ when isVowel x -> input + "ay"
    | x::xs -> (toString xs) + (toString [x]) + "ay"
    | _ -> input
 