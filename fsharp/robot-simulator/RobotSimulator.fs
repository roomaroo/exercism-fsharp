module RobotSimulator

type Direction = 
    North = 0
    | East = 1
    | South = 2
    | West = 3

type Robot = {Position: int * int; Direction: Direction}

let create direction position = 
    {Direction = direction; Position = position}
    
let turn robot = 
    let newDirection = (int robot.Direction + 1) % 4 |> enum
    {robot with Direction = newDirection}

let advance robot = 
    let addVector (x, y) (x', y') = (x + x', y + y')
    let fn = addVector robot.Position

    {robot with Position = 
                match robot.Direction with
                | Direction.North -> fn (0, 1)
                | Direction.East -> fn (1, 0)
                | Direction.South -> fn (0, -1)
                | Direction.West -> fn (-1, 0)
                | _ -> robot.Position}

let move (commands: string) robot =
    let singleMove r command =
        match command with
        | 'R' -> r |> turn
        | 'L' -> r |> (turn >> turn >> turn)
        | 'A' -> advance r
        | _ -> r

    Seq.fold singleMove robot commands