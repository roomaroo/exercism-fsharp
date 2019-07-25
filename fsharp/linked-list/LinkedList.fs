module LinkedList

type LinkedList() =
    member val InnerList: int list = List.empty with get, set

let mkLinkedList () = LinkedList()

let getFirstOrLast listfn (linkedList: LinkedList) =
    linkedList.InnerList
    |> listfn
    |> function
    | [] -> failwith "List is empty"
    | x::xs -> 
        linkedList.InnerList <- listfn xs
        x

let pop: LinkedList -> int = getFirstOrLast List.rev
    
let shift: LinkedList -> int = getFirstOrLast id
    
let addItem listfn newValue (linkedList: LinkedList) = 
    linkedList.InnerList <-
        listfn (newValue::listfn linkedList.InnerList)
    
let push: int -> LinkedList -> unit = addItem List.rev 
    
let unshift: int -> LinkedList -> unit = addItem id
