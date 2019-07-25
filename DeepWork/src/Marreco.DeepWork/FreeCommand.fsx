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
open Scheduling

type Conflicts = Slot * Work list
type CommandF<'a> = 
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
    | Free of CommandF<Command<'a>>


module Command = 
    let retrn = Pure
    let rec map f = function 
        | Pure a -> Pure <| f a
        | Free event -> Free <| EventF.map (map f) event

    let rec bind f = function 
        | Pure a -> f a
        | Free event -> Free <| EventF.map (bind f) event


type CommandBuilder() = 
    member __.Zero() = Pure ()
    member __.Return(a) = Command.retrn a
    member __.Bind (a, f) = Command.bind f a
    member x.While (guard, body) = if (not <| guard()) then x.Zero()
                                   else x.Bind (body, (fun _ -> x.While (guard, body)))

    member x.For(coll:seq<_>, func) = 
        let en = coll.GetEnumerator()
        let mn = en.MoveNext
        x.While (mn, func en.Current)




let command = CommandBuilder()

let stop = Pure
let unassignWork work slot = Free <| UnassignWork (work, slot, stop)
let assignWork work slot = Free <| AssignWork (work, slot, stop)
let planWork work = Free <| PlanWork (work, stop)

let assignAndPlanConflicts work slot = command {
        let! (slot, workConflicts)  = assignWork work slot

        for work in workConflicts do
            do! unassignWork work slot
            do! planWork work
    }


type Events = 
    | WorkPlanned of Work 
    | WorkAssigned of Work * Slot 
    | WorkUnassigned of Work * Slot 


let rec interpretAsEvents state command = 
    let rec recurse cmd events state =
        match cmd with
        | Pure a -> events
        | Free (PlanWork (work, next)) -> (WorkPlanned work)::events |> recurse (next())
        | Free (AssignWork (work, slot, next)) -> 




//Algebra

type Command = Undefined
type CommandError = Undefined
type CommandHandler = Command -> State -> Result<Event list, CommandError>
type Apply = State -> Event -> State
