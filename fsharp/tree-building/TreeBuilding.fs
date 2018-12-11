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

let buildTreeX records = 
    let records' = List.sortBy (fun x -> x.RecordId) records

    if List.isEmpty records' then failwith "Empty input"
    else
        let root = records'.[0]
        if (root.ParentId = 0 |> not) then
            failwith "Root node is invalid"
        else
            if (root.RecordId = 0 |> not) then failwith "Root node is invalid"
            else
                let mutable prev = -1
                let mutable leafs = []

                for r in records' do
                    if (r.RecordId <> 0 && (r.ParentId > r.RecordId || r.ParentId = r.RecordId)) then
                        failwith "Nodes with invalid parents"
                    else
                        if r.RecordId <> prev + 1 then
                            failwith "Non-continuous list"
                        else                            
                            prev <- r.RecordId
                            if (r.RecordId = 0) then
                                leafs <- (-1, r.RecordId) :: leafs
                            else
                                leafs <- (r.ParentId, r.RecordId) :: leafs

                leafs <- List.rev leafs 
                let root = leafs.[0]

                let grouped = leafs |> List.groupBy fst |> List.map (fun (x, y) -> (x, List.map snd y))
                let parens = List.map fst grouped
                let map = grouped |> Map.ofSeq

                let rec helper key =
                    if Map.containsKey key map then
                        Branch (key, List.map (fun i -> helper i) (Map.find key map))
                    else
                        Leaf key                    

                let root = helper 0
                root

let filterChildren id records = 
    records |> Seq.filter (fun r -> r.ParentId = id) 

let buildTree records = 
    if List.isEmpty records then failwith "Empty input"
    if List.exists (fun r -> r.ParentId > r.RecordId) records then failwith "Invalid parent"

    let grouped = 
        records
        |> List.filter (fun r -> r.RecordId <> r.ParentId)
        |> List.sortBy (fun r -> r.RecordId)
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
        | x::[] -> x
        | _ -> failwith "More than one root node"


    records
    |> getRoot
    |> buildSubTree
