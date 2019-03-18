module TwelveDays

open System

let ordinals = [
    "first"
    "second"
    "third"
    "fourth"
    "fifth"
    "sixth"
    "seventh"
    "eighth"
    "ninth"
    "tenth"
    "eleventh"
    "twelfth"]

let gifts = [
    "and a Partridge in a Pear Tree."
    "two Turtle Doves"
    "three French Hens"
    "four Calling Birds"
    "five Gold Rings"
    "six Geese-a-Laying"
    "seven Swans-a-Swimming"
    "eight Maids-a-Milking"
    "nine Ladies Dancing"
    "ten Lords-a-Leaping"
    "eleven Pipers Piping"
    "twelve Drummers Drumming"
]

let giftsForDay n = 
    match n with
    | 1 -> gifts.[0].Replace("and ", "")
    | _ -> gifts.[0..n-1] |> List.rev |> fun l -> String.Join(", ", l)

let reciteDay n =
    sprintf 
        "On the %s day of Christmas my true love gave to me: %s"
        ordinals.[n-1]
        (giftsForDay n)

let recite start stop =
    [start..stop] |> List.map reciteDay