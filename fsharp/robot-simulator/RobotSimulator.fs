module RobotSimulator

type Direction = North | East | South | West
type Position = int * int

type Robot = { direction: Direction; position: Position }

let create direction position = 
    {direction = direction; position = position}

let turnLeft robot =
    match robot.direction with
    | North -> {robot with direction = West}
    | East -> {robot with direction = North}
    | South -> {robot with direction = East}
    | West -> {robot with direction = South}

let turnRight robot =
    match robot.direction with
    | North -> {robot with direction = East}
    | East -> {robot with direction = South}
    | South -> {robot with direction = West}
    | West -> {robot with direction = North}

let addVectors (x1, y1) (x2, y2) = 
    (x1 + x2, y1 + y2)

let advance robot =
    let move = 
        match robot.direction with
        | North -> (0, 1)
        | East -> (1, 0)
        | South -> (0, -1)
        | West -> (-1, 0)

    {robot with position = addVectors robot.position move}  

let executeInstruction robot instruction = 
    match instruction with
    | 'R' -> turnRight robot
    | 'L' -> turnLeft robot
    | 'A' -> advance robot
    | _ -> failwith (sprintf "Unknown instruction '%c'" instruction)

let instructions (instructions': string) robot = 
    Seq.fold executeInstruction robot instructions'         