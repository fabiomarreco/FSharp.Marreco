//module FreeCommands

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


#load "Shared.fs"
#load "Time.fs"
#load "Work.fs"
#load "Schedule.fs"
open Work
open Marreco.DeepWork
open Slot


type Conflicts = Slot * Work list
type EventF<'a> = 
    | UnassignWork of Work * Slot * (unit -> 'a)
    | AssignWork of Work * Slot * (Conflicts -> 'a)
    | PlanWork of Work * (unit -> 'a)


module EventF = 
    let map f = function  
        | UnassignWork (work, slot, next) -> UnassignWork (work, slot, next >> f )
        | AssignWork (work, slot, next) -> AssignWork (work, slot, next >> f)
        | PlanWork (work, next) -> PlanWork (work, next >> f )
    //let map f = function  | Apply (state, event, next) -> Apply (state, event, next >> f) 

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


type CommandBuilder() = 
    member x.Zero() = Pure ()
    member x.Return(a) = Command.retrn a
    member x.Bind (a, f) = Command.bind f a
    member x.While (guard, body) = if (!guard()) then x.Zero()
                                   else x.Bind (body(), x.While (guard, body))

    member x.For(coll:seq<_>, func) = 
        let en = coll.GetEnumerator()
        let mn = en.MoveNext
        x.While (mn, func en.Current)



let command = new CommandBuilder()

let stop = fun a -> Pure a
let unassignWork work slot = Free <| UnassignWork (work, slot, stop)
let assignWork work slot = Free <| AssignWork (work, slot, stop)
let planWork work = Free <| PlanWork (work, stop)

let assignAndPlanConflicts work slot = command {
        let! conflicts = assignWork work slot

        for conflict in conflicts do
            do! unassignWork work conflict
            //do! planWork work conflict
    }

//-------------------

//Algebra

type Command = Undefined
type CommandError = Undefined
type CommandHandler = Command -> State -> Result<Event list, CommandError>
type Apply = State -> Event -> State

    
let add : Command<