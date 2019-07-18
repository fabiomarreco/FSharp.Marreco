(* 
 * Simple Example of CQRS in F# using Free Monads
 * ==============================================
 *   The goal is to create simple basic commands that define an algebra 
 * for our data type; More complex commands will be built by composing these
 * base commands much like lego.
 *
 * Domain:  
 *    We will work with a very simple domain of a bank account, in which the
 *  following actions can be peformed:
 * 
 *  - Accounts can be created, associated with a client 
 *  - A client can have multiple accounts, but only one market as "Prefered"
 *  - A deposit can be made to any account
 *  - The account balance cannot become negative (overdraft)
 *)

type Client = string
type Money = float
type CmdError = string
type Account = { Client : Client; Balance : Money }


type DepositResult = 
    | Success | AccountDoesNotExists

type AccountCreationResult = 
    | Success | AccountAlreadyExists

type CommandF<'a> = 
    | CreateAccount of Client * (AccountCreationResult -> 'a)
    //| SetDepositLimit of Money * (unit -> 'a)
    | Deposit of Money * (DepositResult -> 'a)

module CommandF = 
    let map f = function 
        | Deposit (value, next) ->  Deposit (value, next >> f)
        | CreateAccount (qtd, next) -> CreateAccount (qtd, next >> f)

type Command<'a> = 
    | Pure of 'a
    | Free of CommandF<Command<'a>>


module Command = 
    let retrn a = Pure a
    let rec map f = function 
        | Pure a -> f a |> Pure
        | Free c -> CommandF.map (map f) c |> Free
    let rec bind f = function 
        | Pure a -> f a 
        | Free c -> CommandF.map (bind f) c |> Free

type CommandBuilder() = 
    member __.ReturnFrom (a) = a
    member __.Zero() = Pure ()
    member __.Return(a) = Command.retrn a
    member __.Bind (a, f) = Command.bind f a

let command = CommandBuilder()

let stop = Pure
let createAccount client = CreateAccount(client, stop) |> Free
let deposit money = Deposit(money, stop) |> Free
let depositCreatingAccount client money = command { 
    let! depositResult = deposit money
    return! match (depositResult) with 
            | AccountDoesNotExists -> command {
                   // I am ignoring createAccount possible error in the sample
                   // since I have a single possible error that was already treated
                   do! (createAccount client |> Command.map ignore)
                   do! (deposit money |> Command.map ignore)
               }
            | DepositResult.Success -> stop()
}
  
type Event = 
    | AccountCreated of Client
    | MoneyDeposited of Money

//===================================================

module Account = 
    let create client = function 
         | Some _ -> AccountAlreadyExists
         | None -> AccountCreationResult.Success

    let deposit money = function
        | Some account -> DepositResult.Success
        | None -> AccountDoesNotExists


module Event = 
    let apply account = function
        | AccountCreated client -> { Client = client; Balance = 0.}
        | MoneyDeposited money -> { account with Balance = account.Balance + money }


let rec interpretAsEventList (events, account) = function
    | Pure a -> events |> List.rev
    | Free (CreateAccount (client, next)) -> 
        let res = Account.create client account
        let cont = next res
        match res with 
        | AccountCreationResult.Success -> 
            let event = AccountCreated client
            let account' = Event.apply ({ Client = ""; Balance = 0.}) event |> Some // o option aqui nao esta legal
            interpretAsEventList (event::events, account') cont
        | AccountAlreadyExists -> 
            interpretAsEventList (events, account) cont

    | Free (Deposit (value, next)) -> 
        let res = Account.deposit value account
        let cont = next res
        match res with 
        | DepositResult.Success -> 
            let event = MoneyDeposited value
            let account' = Event.apply (account.Value) event |> Some
            interpretAsEventList (event::events, account') cont
        | AccountDoesNotExists -> 
            interpretAsEventList (events, account) cont

    
createAccount "Marreco" |> interpretAsEventList ([], None) 
deposit 10. |> interpretAsEventList ([], None) 

depositCreatingAccount "Marreco" 10. |> interpretAsEventList ([], None) 


