module KindergartenGarden

type Plant = 
    Radishes | Clover | Grass | Violets

let students = 
    ["Alice"; "Bob"; "Charlie"; "David";
     "Eve"; "Fred"; "Ginny"; "Harriet";
     "Ileana"; "Joseph"; "Kincaid"; "Larry"; ]

let getRows (diagram: string) = 
    diagram.Split('\n')

let plantsCodesForIndex index (row: string) = 
    let start = index * 2
    row.[start .. start + 1]

let decodePlant code = 
    match code with
    | 'R' -> Radishes
    | 'C' -> Clover
    | 'G' -> Grass
    | 'V' -> Violets
    | _ -> failwith "Unknown plant"

let plants diagram student = 
    let studentIndex = 
        match List.tryFindIndex (fun name -> name = student) students with
        | Some n -> n
        | None -> failwith "Unknown student"

    diagram
    |> getRows
    |> Seq.map (plantsCodesForIndex studentIndex)
    |> Seq.collect (Seq.map decodePlant)
    |> List.ofSeq