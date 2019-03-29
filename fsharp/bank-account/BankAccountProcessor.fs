module BankAccountProcessor

type Balance = decimal option

type Command = 
    | Open
    | Close
    | Deposit of decimal
    | GetBalance of AsyncReplyChannel<Balance>


type Account () = 

    let agent = MailboxProcessor.Start(fun inbox -> 
        let rec messageLoop balance = async {
            let! cmd = inbox.Receive()
            
            return!
                match cmd with
                | Open -> messageLoop (Some 0m)
                | Close -> messageLoop None
                | Deposit amount -> Option.map ((+) amount) balance |> messageLoop
                | GetBalance replyChannel -> 
                    replyChannel.Reply balance
                    messageLoop balance
        }

        messageLoop None
    )

    member __.Open () = agent.Post Open  
    member __.Close () = agent.Post Close
    member __.Deposit amount = agent.Post (Deposit amount)
    member __.GetBalance () = agent.PostAndAsyncReply GetBalance

let mkBankAccount() = Account()

let openAccount (a: Account) = 
    a.Open()
    a

let closeAccount (a: Account) = 
    a.Close()
    a

let getBalance (a: Account) = 
    a.GetBalance() |> Async.RunSynchronously

let updateBalance change (a: Account) = 
    a.Deposit change
    a