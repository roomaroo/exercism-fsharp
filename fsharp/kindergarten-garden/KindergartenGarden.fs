module KindergartenGarden

type Plant = Radishes | Clover | Grass | Violets

let students = 
    ["Alice"; "Bob"; "Charlie"; "David";
     "Eve"; "Fred"; "Ginny"; "Harriet";
     "Ileana"; "Joseph"; "Kincaid"; "Larry"; ]

let splitRows (diagram: string) = 
    diagram.Split('\n')

let codesAtIndex index (row: string) = 
    row
    |> Seq.chunkBySize 2
    |> Seq.item index

let decodePlant code = 
    match code with
    | 'R' -> Radishes
    | 'C' -> Clover
    | 'G' -> Grass
    | 'V' -> Violets
    | _ -> failwith "Unknown plant"

let plants diagram student = 
    let index = 
        match List.tryFindIndex (fun name -> name = student) students with
        | Some n -> n
        | None -> failwith "Unknown student"

    diagram 
    |> splitRows 
    |> Seq.collect (codesAtIndex index) 
    |> Seq.map decodePlant
    |> List.ofSeq