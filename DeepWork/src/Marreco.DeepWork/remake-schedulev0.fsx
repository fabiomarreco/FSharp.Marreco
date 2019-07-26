#load "../.paket/load/netcoreapp2.2/main.group.fsx"
#load "Shared.fs"
#load "Time.fs"
#load "Work.fs"
#load "Schedule.fs"

//open Marreco.DeepWork
open Work
open Scheduling



//planwork
//unplan work
//assign slot 
//unassign slot
//type Conflicts = Slot * Work list

type SlotIdNotFound = SlotIdNotFound
type Success = Success

type Predicate<'a> = 'a -> bool
type Update<'a>  = 'a -> 'a

type CommandF<'a> = 
    | GetSchedule of unit * (Schedule -> 'a)
    | PlanWork of Work * (Success -> 'a)
    | UnplanWork of Work * (Success -> 'a)
    | SetSlotAssignment of (Predicate<Slot> * Update<Slot>) * (Success -> 'a)

(*
    | PlanWork of Work * (unit -> 'a)
    | UnassignWork of Work * Slot * (unit -> 'a)
    | AssignWork of Work * Slot * (Conflicts -> 'a)
*)


// Mechanical.....
module CommandF = 
    let map f = function 
        | GetSchedule (_, next) -> GetSchedule ((), next >> f)
        | PlanWork (input, next) -> PlanWork (input, next >> f)
        | UnplanWork (input, next) -> UnplanWork (input, next >> f)
        | SetSlotAssignment (input, next) -> SetSlotAssignment (input, next >> f)


type Command<'a> = 
    | Pure of 'a 
    | Free of CommandF<Command<'a>>


module Command = 
    let retrn a = Pure a
    let rec map f = function 
        | Pure a -> f a  |> Pure
        | Free c -> CommandF.map (map f) c |> Free
    let rec bind f = function 
        | Pure a -> f a 
        | Free c -> CommandF.map (bind f) c |> Free

type CommandBuilder() = 
    member __.ReturnFrom (a) = a
    member __.Zero() = Pure ()
    member __.Return(a) = Command.retrn a
    member __.Bind (a, f) = Command.bind f a
    member x.While (guard, body) = if (not <| guard()) then x.Zero()
                                   else x.Bind (body, (fun _ -> x.While (guard, body)))

    member x.For(coll:seq<_>, func) = 
        let en = coll.GetEnumerator()
        x.While (en.MoveNext, func en.Current)

let command = CommandBuilder()
//================ end mechanical

//base commands:....
let stop = Pure
let getSchedule = Free <| GetSchedule ((), stop)
let planWork work = Free <| PlanWork (work, stop)
let unplanWork work = Free <| UnplanWork (work, stop)
let setSlotAssignment f u = Free <| SetSlotAssignment ((f, u), stop)

//other....
let mapSchedule f = Command.map f getSchedule
let slotById slotId = Schedule.findSlotById slotId |> mapSchedule
let slotsInPeriod period = Schedule.slotsInPeriod period |> mapSchedule

let assignWorkToSlot work slot = command {
    let filter s = s.Id == slot.Id
    let update s = Slot.assignWork work s
    let set = setSlotAssignment filter
    let p slot = command { 
        let! a = set slot 
        a.
    }

    
   
}

//success | error (conflitos)
let assignWork period work = command {
    let setSlot = Slot.assignWork work >> Result.map (fun s-> s.Engagement)


    let setSlotEngagement slot =
        match slot with 
        | Conflicts work _ -> Error slot.Id
        | _ -> Slot.assignWork


        match    
    let! slots = slotsInPeriod period 
    let conflictingSlots = slots |> List.filter (function | Conflicts work _ ->  true | _ -> false)
    if (List.isEmpty conflictingSlots) then 
        for slot in slots do
             Slot.assignWork work slot
            let! _ = setSlotAssignment (fun s -> s.Id = slot.Id) (newSlot.)


}

