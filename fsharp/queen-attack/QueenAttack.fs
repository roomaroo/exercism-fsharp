module QueenAttack

let create (row, col) = 
    let boardSize = 8
    let validCoord x = 
        x >=0 && x < boardSize
        
    validCoord row && validCoord col

let canAttack (row1, col1) (row2, col2) = 
    row1 = row2
    || col1 = col2
    || abs(row1 - row2) = abs(col1 - col2)

