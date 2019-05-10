module RomanNumerals

let repeat n c = String.init n (fun _ -> c)

let convert one five ten n = 
    match n with
    | 0 -> None
    | n when n < 4 -> repeat n one |> Some
    | 4 -> one + five |> Some
    | 5 -> Some five
    | 9 -> one + ten |> Some
    | n -> five + (repeat (n - 5) one) |> Some

let units = convert "I" "V" "X"
let tens = convert "X" "L" "C"
let hundreds = convert "C" "D" "M"
let thousands = convert "M" "" ""

let roman arabicNumeral = 
    [arabicNumeral / 1000 |> thousands
     (arabicNumeral % 1000) / 100 |> hundreds
     (arabicNumeral % 100) / 10 |> tens
     arabicNumeral % 10 |> units]
    |> List.choose id
    |> String.concat ""