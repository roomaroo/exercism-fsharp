module NucleotideCount

let nucleotideCounts (strand: string): Option<Map<char, int>> = 

    let types = ['A';'C';'G';'T']

    let count c = 
        strand |> Seq.filter ((=) c) |> Seq.length

    let isValid c = List.contains c types

    if String.forall isValid strand then 
        types
        |> List.map (fun t -> (t, count t))
        |> Map.ofList
        |> Some  

    else None    

    

    


