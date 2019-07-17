type Client = string
type Money = float
type CmdError = string


type DepositResult = 
    | Success | AccountDoesNotExists

type CreateAccountResult = 
    | Success | AccountAlreadyExisted

type CommandF<'a> = 
    | CreateAccount of Client * (CreateAccountResult -> 'a)
    //| SetDepositLimit of Money * (unit -> 'a)
    | Deposit of Money * (DepositResult -> 'a)

module CommandF = 
    let map f = function 
        | Deposit (name, next) ->  Deposit (name, next >> f)
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

type Accounts = Map<Client, Money>

module Account = 
    let create client accounts = if (Map.containsKey client) then AccountAlreadyExisted | 
    let deposit money account  = { account with Balance = account.Balance + money}


let interpretAsEventList events = function
    | Pure a -> events
    | Free (CreateAccount (client, next)) -> 
        


