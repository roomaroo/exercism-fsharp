module TreeBuilding

type Record = { RecordId: int; ParentId: int }
type Tree =
    | Branch of int * Tree list
    | Leaf of int

let recordId t =
    match t with
    | Branch (id, c) -> id
    | Leaf id -> id

let isBranch t =
    match t with
    | Branch (id, c) -> true
    | Leaf id -> false

let children t =
    match t with
    | Branch (id, c) -> c
    | Leaf id -> []

let buildTree records =
    if List.isEmpty records then failwith "Empty input"
    if List.exists (fun r -> r.ParentId > r.RecordId) records then failwith "Invalid parent"
    
    let sorted = List.sortBy (fun r -> r.RecordId) records

    let grouped =
        sorted
        |> List.filter (fun r -> r.RecordId <> r.ParentId)
        |> List.groupBy (fun r -> r.ParentId)
        |> Map.ofList

    let rec buildSubTree root =
        match Map.tryFind root.RecordId grouped with
        | Some children -> Branch(root.RecordId, List.map buildSubTree children)
        | None -> Leaf root.RecordId

    let getRoot rs =
        rs
        |> List.filter (fun r -> r.RecordId = r.ParentId)
        |> function
        | [] -> failwith "No root node"
        | [x] -> x
        | _ -> failwith "More than one root node"

    let checkContinuous records = 
        let ids = records |> List.map (fun r -> r.RecordId)
        if [0..((List.length ids) - 1)] = ids then 
            records 
        else 
            failwith "Non-continuous"

    sorted
    |> checkContinuous
    |> getRoot
    |> buildSubTree
