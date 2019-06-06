module RestApi

open Newtonsoft.Json

type User = {
    name: string
    owes: Map<string, float>
    owed_by: Map<string, float>
    balance: float
}

type AddUserRequest = {
    user: string
}

type GetUserRequest = {
    users: string list
}

type UsersResponse = {
    users: User list
}

type IouRequest = {
    lender: string
    borrower: string
    amount: float
}

type Database = {
    users: User list
}

let deserialize<'a>(json: string) = 
    JsonConvert.DeserializeObject<'a>(json)

let serialize x = JsonConvert.SerializeObject(x)

type RestApi(database : string) =

    let db = deserialize<Database>(database)

    let addUser request = 
        let newUser = {
            name = request.user
            owes = Map.empty
            owed_by = Map.empty
            balance = 0.0 }
            
        newUser

    let getUserByName name = 
        db.users 
        |> List.find (fun u -> u.name = name)

    let getUsers (request: GetUserRequest) = 
        db.users
        |> List.filter (fun u -> Seq.contains u.name request.users)
        |> fun uu -> {users = uu}
        
    let addTransaction name amount transactions = 
        let amount = 
            Map.tryFind name transactions 
            |> Option.map (fun x -> x + amount)
            |> Option.defaultValue amount

        match amount with
        | x when x <= 0.0 -> Map.remove name transactions
        | x -> Map.add name x transactions

    let applyIou (request: IouRequest) : UsersResponse =
        let lender = getUserByName request.lender
        let borrower = getUserByName request.borrower

        let lender = 
            {lender with 
                owes = addTransaction borrower.name (-1.0 * request.amount) lender.owes
                owed_by = addTransaction borrower.name request.amount lender.owed_by
                balance = lender.balance + request.amount}

        let borrower = 
            {borrower with 
                owes = addTransaction lender.name request.amount borrower.owes
                owed_by = addTransaction lender.name (-1.0 * request.amount) borrower.owed_by
                balance = borrower.balance - request.amount}

        {users = [borrower; lender] |> List.sortBy (fun u -> u.name)}

    member this.Get(url: string) =
        serialize db

    member this.Get(url: string, payload: string) =
        match url with
        | "/users" -> 
                payload 
                |> deserialize<GetUserRequest>
                |> getUsers
                |> serialize
        | _ -> failwith "404"            

    member this.Post(url: string, payload: string)  =
        match url with
        | "/add" -> 
                payload 
                |> deserialize<AddUserRequest>
                |> addUser
                |> serialize
        | "/iou" ->
                payload
                |> deserialize<IouRequest>
                |> applyIou
                |> serialize

        | _ -> failwith "404"
