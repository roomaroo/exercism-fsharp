module CircularBuffer

type CircularBuffer =
    {Items: int list; Capacity: int}

let isFull buffer =
    buffer.Items |> List.length = buffer.Capacity

let mkCircularBuffer size =
    {Items = List.empty; Capacity = size}

let clear buffer = {buffer with Items = List.empty}

let internal writeImpl value force buffer =
    match (isFull buffer), force with
    | false, _    -> {buffer with Items = buffer.Items @ [value]}
    | true, true  -> match buffer.Items with
                     | _::xs -> {buffer with Items = xs @ [value]}
                     | [] -> buffer // Zero capacity
    | true, false -> failwith "Buffer is full"

let write value buffer = writeImpl value false buffer

let forceWrite value buffer = writeImpl value true buffer

let read buffer =
    match buffer.Items with
    | [] -> failwith "Cannot read from empty buffer"
    | x::xs -> (x, {buffer with Items = xs})
