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

let const' a _ = a

type Client = string
type Money = float
type CmdError = string
type Account = { Client : Client; Balance : Money }
type CreditCard = string
type Product = string


type CommandResult<'err> = Success | Fail of 'err

type AddFunds = AddFundsParameters -> AddFundsResult
    and AddFundsParameters = (CreditCard * Money)
    and AddFundsResult = FundsAdded | CreditCardRejected

type Purchase = PurchaseParameters -> PurchaseResult
    and PurchaseParameters = (Product * Money)
    and PurchaseResult = 
        | ProductPurchased
        | NotEnougthFunds of AvailableFunds 
    and AvailableFunds = Money


//-----------------------------------------------------------

//Functor
type CommandF<'a> = 
    | Purchase of PurchaseParameters * (PurchaseResult -> 'a)
    | AddFunds of AddFundsParameters * (AddFundsResult -> 'a)

module CommandF = 
    let map f = function 
        | Purchase (input, next) -> Purchase (input, next >> f)
        | AddFunds (input, next) -> AddFunds (input, next >> f)

//Monad
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

//======================================

//base functions
let stop = Pure
let addFunds input = AddFunds (input, stop) |> Free
let purchase input = Purchase (input, stop) |> Free

// Custom implementation: 
type PurchaseWithAutoRechargeResult = 
    | ProductPurchased
    | CreditCardRejected


//usar o result monad aqui
let purchaseWithAutoRecharge (creditCard, product, price) = command { 
        match! purchase (product, price) with 
        | PurchaseResult.ProductPurchased -> return ProductPurchased
        | NotEnougthFunds available -> 
            match! addFunds (creditCard, price - available) with 
            | AddFundsResult.CreditCardRejected -> 
                return CreditCardRejected
            | AddFundsResult.FundsAdded -> 
                let! _ = purchase (product, price)
                return ProductPurchased
}


///======================= CONTINUAR A PARTIR DAQUI!
type Event = 
    | AccountCreated of Client
    | MoneyDeposited of Money

//===================================================

module Account = 
    let create client = function 
         | Some _ -> AccountCreationResult.CreationError AccountAlreadyExists 
         | None -> AccountCreationResult.Success

    let deposit money = function
        | Some account -> DepositResult.Success
        | None -> DepositError AccountDoesNotExists


module Event = 
    let apply account = function
        | AccountCreated client -> { Client = client; Balance = 0.}
        | MoneyDeposited money -> { account with Balance = account.Balance + money }


//===========================================
let rec interpret fCreateAccount fDeposit fStop command =
    let loop = interpret fCreateAccount fDeposit fStop 
    match command with
    | Pure a -> fStop a
    | Free (CreateAccount (client, next)) -> fCreateAccount client |> next |> loop
    | Free (Deposit (client, next)) -> fDeposit client |> next |> loop


// Normalizing the command errors
type CommandExecutionError = 
    | CreationError of CreationError
    | DepositError of DepositError


let interpretAsEventListResult account command = 
    let rec loop  (events, account, error) command =
        match command with 
        | Pure a -> match error with | Some err -> Error err | None -> Ok (events |> List.rev)

        | Free (CreateAccount (client, next)) -> 
            let res = Account.create client account
            let cont = next res
            match res with 
            | AccountCreationResult.Success -> 
                let event = AccountCreated client
                let account' = Event.apply ({ Client = ""; Balance = 0.}) event |> Some // o option aqui nao esta legal
                let events' = event::events
                loop (events', account', None) cont

            | AccountCreationResult.CreationError AccountAlreadyExists -> 
                loop (events, account, Some (CreationError AccountAlreadyExists))  cont

        | Free (Deposit (value, next)) -> 
            let res = Account.deposit value account
            let cont = next res
            match res with 
            | DepositResult.Success -> 
                let event = MoneyDeposited value
                let account' = Event.apply (account.Value) event |> Some
                
                let events' = event::events
                loop (events', account', None)  cont
            | DepositResult.DepositError AccountDoesNotExists -> 
                loop (events, account, Some (DepositError AccountDoesNotExists)) cont

    in loop ([], account, None) command

let interp cmd = interpretAsEventListResult None cmd
    
createAccount "Marreco" |> interp
depositCreatingAccount "Marreco" 10. |> interp

deposit 10. |> interp


