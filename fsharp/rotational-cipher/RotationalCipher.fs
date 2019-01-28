module RotationalCipher

open System

let letters = List.ofSeq "abcdefghijklmnopqrstuvwxyz"
let isLetter c = Char.IsLetter c
let isUpper c = Char.ToUpper(c) = c

let encode shiftKey c = 
    if isLetter c then
        letters 
        |> List.findIndex ((=) (Char.ToLower(c)))
        |> (+) shiftKey 
        |> fun n -> letters.[n % 26]
        |> fun x -> if isUpper c then Char.ToUpper(x) else x
    else
        c    

let rotate shiftKey text = 
    text
    |> Seq.map (encode shiftKey)
    |> String.Concat

    