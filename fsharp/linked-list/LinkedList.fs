module LinkedList

type Node(value) =
    member val Value = value
    member val Prev: Node option = None with get, set
    member val Next: Node option = None with get, set

type LinkedList() = 
    member val Head: Node option = None with get, set

    member this.Last() =
        let rec lastImpl (node: Node) =
            match node.Next with
            | Some n -> lastImpl n
            | None -> node

        Option.map lastImpl this.Head        

let mkLinkedList () = LinkedList()

let addToEmpty newValue (linkedList: LinkedList) = 
    linkedList.Head <- Node(newValue) |> Some

let pop (linkedList: LinkedList) = 
    match linkedList.Last() with
    | Some node -> 
        Option.map (fun (p: Node) -> p.Next <- None) node.Prev |> ignore
        node.Value
    | None -> failwith "Can't pop an empty list"

let shift (linkedList: LinkedList) = 
    match linkedList.Head with
    | Some node -> 
        Option.map (fun (n: Node) -> n.Prev <- None) node.Next |> ignore
        linkedList.Head <- node.Next
        node.Value
    | None -> failwith "Can't shift an empty list"

let push newValue (linkedList: LinkedList) = 
    match linkedList.Last() with
    | Some n ->
        let newNode = Node(newValue, Prev = Some n) |> Some
        n.Next <- newNode
    | None -> addToEmpty newValue linkedList    

let unshift newValue (linkedList: LinkedList) =
    match linkedList.Head with
    | Some n ->
        let newNode = Node(newValue, Next = Some n) |> Some
        n.Prev <- newNode
        linkedList.Head <- newNode
    | None -> addToEmpty newValue linkedList   