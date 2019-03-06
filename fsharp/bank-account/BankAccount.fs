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

let openAccount =
    lock (fun a -> a.Balance <- defaultArg a.Balance 0.0m |> Some)
    
let closeAccount = 
    lock (fun a -> a.Balance <- None)

let getBalance account = 
    account.Balance

let updateBalance change = 
    lock (fun a -> a.Balance <- Option.map ((+) change) a.Balance)