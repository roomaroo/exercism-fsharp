module BankAccount
open System.Threading
open System

type Account = {mutable Balance: decimal option; Mutex: obj}

let lock (f: Account -> unit) account = 
    if Monitor.TryEnter(account.Mutex, TimeSpan.FromMilliseconds(100.0)) then
        f account
        Monitor.Exit account.Mutex
        account
    else
        failwith "Failed to lock account"

let mkBankAccount() = {Balance = None; Mutex = obj()}

let openAccount account =
    lock (fun a -> a.Balance <- defaultArg account.Balance 0.0m |> Some) account
    
let closeAccount account = 
    lock (fun a -> a.Balance <- None) account

let getBalance account = 
    account.Balance

let updateBalance change account = 
    let update account = 
        match account.Balance with
        | Some x -> account.Balance <- Some(x + change)
        | None -> ()

    lock update account