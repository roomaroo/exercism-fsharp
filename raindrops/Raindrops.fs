module Raindrops

let raindropSounds = [(3, "Pling"); (5, "Plang"); (7, "Plong")]  

let convert number = 
    raindropSounds
    |> List.filter (fun (factor, _) -> number % factor = 0)
    |> List.map snd
    |> List.fold (+) ""
    |> function
       | "" -> string number
       | sounds -> sounds