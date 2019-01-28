module RnaTranscription

let toRna (dna: string): string = 
    let complement = function
        | 'G' -> Some 'C'
        | 'C' -> Some 'G'
        | 'T' -> Some 'A'
        | 'A' -> Some 'U'
        | _ -> None

    dna |> Seq.choose complement |> System.String.Concat
