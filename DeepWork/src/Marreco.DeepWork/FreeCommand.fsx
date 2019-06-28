module FreeCommands

type Item = string

type RepoF<'a> = 
    | Get of int * (Item -> 'a)
    | Set of Item * (Unit -> 'a)

module RepoF = 
    let map = () //todo functor para repof


type Program<'a> = 
    | Pure of 'a
    | Free of Program<RepoF<'a>>


// Fazer monad com program....

//===========================
type State = Set<int>

type Event = 
    | ItemAdded of int
    | ItemDeleted of int

type EventF<'a> = 
    | Apply of State * Event * (State -> 'a) 


module EventF = 
    let map f = function  | Apply (state, event, next) -> Apply (state, event, next >> f) 

type Command<'a> = 
    | Pure of 'a
    | Free of EventF<Command<'a>>


module Command = 
    let retrn = Pure
    let rec map f = function 
        | Pure a -> Pure <| f a
        | Free event -> Free <| EventF.map (map f) event

    let rec bind f = function 
        | Pure a -> f a
        | Free event -> Free <| EventF.map (bind f) event



    
let add : Command<