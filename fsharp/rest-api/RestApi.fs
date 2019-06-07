module RestApi

open Newtonsoft.Json

let deserialize<'a>(json: string) = 
    JsonConvert.DeserializeObject<'a>(json)

let serialize x = JsonConvert.SerializeObject(x)

type Loan = {
    name: string
    amount: decimal
}

type User = {
    name: string
    transactions : Loan list
    } with 
    member this.OwedBy 
        with get() = 
            this.transactions
            |> List.groupBy (fun t -> t.name)
            |> List.map (fun (name, transactions) -> (name, transactions |> List.sumBy (fun t -> t.amount)))
            |> List.filter (fun (_, amount) -> amount < 0m)
            |> List.map (fun (name, amount) -> (name, -1m * amount))
            |> Map.ofList

    member this.Owes 
        with get() = 
            this.transactions
            |> List.groupBy (fun t -> t.name)
            |> List.map (fun (name, transactions) -> (name, transactions |> List.sumBy (fun t -> t.amount)))
            |> List.filter (fun (_, amount) -> amount > 0m)
            |> Map.ofList     

    member this.Balance 
        with get() = 
            this.transactions
            |> List.sumBy (fun t -> -1m * t.amount)


type UserDto = {
    name: string
    owes: Map<string, decimal>
    owed_by: Map<string, decimal>
    balance: decimal
}

let fromDto (userDto: UserDto) : User = 
    let borrowed = 
        userDto.owes
        |> Map.toList
        |> List.map (fun (name, amount) -> {name=name; amount=amount})

    let lent = 
        userDto.owed_by
        |> Map.toList
        |> List.map (fun (name, amount) -> {name=name; amount= -1m * amount})

    {name = userDto.name; transactions = borrowed @ lent }

let toDto (user: User) : UserDto = 
    {
        name = user.name
        owes = user.Owes
        owed_by = user.OwedBy
        balance = user.Balance
    }

type AddUserRequest = {
    user: string
}

type GetUserRequest = {
    users: string list
}

type IouRequest = {
    lender: string
    borrower: string
    amount: decimal
}

type Database = {
    users: User list
}

type DatabaseDto = {
    [<JsonProperty("users")>]
    userDtos: UserDto list
}

type RestApi(database : string) =

    let db = 
        deserialize<DatabaseDto>(database) 
        |> fun dto -> {users = List.map fromDto dto.userDtos}

    let addUser request = 
        { name = request.user
          owes = Map.empty
          owed_by = Map.empty
          balance = 0m }

    let getUserByName name = 
        db.users 
        |> List.find (fun u -> u.name = name)

    let getUsers (request: GetUserRequest) : DatabaseDto = 
        db.users
        |> List.filter (fun u -> Seq.contains u.name request.users)
        |> List.map toDto
        |> fun dtos -> {userDtos = dtos}
        
    let applyIou (request: IouRequest) : DatabaseDto =
        let lender = getUserByName request.lender
        let borrower = getUserByName request.borrower

        let lender = {lender with transactions = {name = borrower.name; amount = (-1m * request.amount)}::lender.transactions}
        let borrower = {borrower with transactions = {name = lender.name; amount = request.amount}::borrower.transactions}

        {userDtos = [borrower; lender] |> List.map toDto |> List.sortBy (fun u -> u.name)}

    member this.Get(url: string) =
        {userDtos = db.users |> List.map toDto} |> serialize

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
