module BankAccount

open System.Collections.Concurrent

type AccountState = Open | Closed
type Transaction = { Amount: decimal }
type Account = {State: AccountState; Transactions: ConcurrentBag<Transaction>}

let mkBankAccount() = {State = Closed; Transactions = ConcurrentBag<_>()}

let openAccount account = {account with State = Open}

let closeAccount account = {account with State = Closed}

let getBalance account = 
    match account.State with
    | Open -> account.Transactions |> Seq.sumBy (fun t -> t.Amount) |> Some
    | Closed -> None

let updateBalance change account = 
    account.Transactions.Add({Amount = change})
    account