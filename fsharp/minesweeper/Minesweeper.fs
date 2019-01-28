module Minesweeper

let findMines input = 
    input 
    |> Seq.mapi (fun row line -> 
                    line
                    |> Seq.mapi (fun col c ->
                                    if c = '*' then Some (row, col) else None))                      
    |> Seq.collect (Seq.choose id)
    |> List.ofSeq


let annotate input = 
    let mines = findMines input
    let isMine coord = List.exists ((=) coord) mines

    let surroundingCells (row, col) =
        [
            (row - 1, col - 1);
            (row - 1, col);
            (row - 1, col + 1);
            (row, col - 1);
            (row, col + 1);
            (row + 1, col - 1);
            (row + 1, col);
            (row + 1, col + 1);
        ]

    let countMines coord = 
        surroundingCells coord
        |> List.filter isMine
        |> List.length

    let annotateCell coord = 
        if isMine coord then
            "*"
        else 
            let count = countMines coord 
            if count > 0 then string count else " "

    input 
    |> List.mapi (fun i row -> 
                        row
                        |> Seq.mapi (fun j _ -> annotateCell (i, j))  
                        |> String.concat "")



