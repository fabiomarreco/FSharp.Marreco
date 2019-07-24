#load "Shared.fs"
#load "Time.fs"
#load "Work.fs"
#load "Schedule.fs"
open Work
open Marreco.DeepWork
open Scheduling


//planwork
//unplan work
//assign slot 
//unassign slot
//type Conflicts = Slot * Work list

type SlotIdNotFound = SlotIdNotFound
type Success = Success


type CommandF<'a> = 
    | GetSchedule of unit * (Schedule -> 'a)
    | PlanWork of Work * (Success -> 'a)
    | UnplanWork of Work * (Success -> 'a)
    | SetSlotAssignment of (SlotId * Engagement option) * (Result<Success, SlotIdNotFound> -> 'a)

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
let setSlotAssignment slotId engagement = Free <| SetSlotAssignment ((slotId,engagement), stop)

//other....
let getSlotById slotId = command { 
    let! schedule = getSchedule
    return Schedule.findSlotById slotId schedule
}

type ShallowWorkAssgimentError = 
    | SlotIdNotFound
    | ShallowWorkConflict of Work


let assignShallowWorkToSlot slotId swork = command {
    let result = 
        match! getSlotById slotId with 
        | None -> Error SlotIdNotFound
        | Some slot -> 
            match slot.Engagement with 
            | None 
            | Some (Shallow _) -> Ok Success
            | Some (Deep d) -> Error (ShallowWorkConflict (DeepWork d))
    return result
}




let assignWorkToSlot slotId work = command { 
    let! schedule = getSchedule
    let result = 
        match Schedule.findSlotById slotId with 
        | None -> Error SlotIdNotFound
        | Some slot -> 
            match slot with 
            | Conflicting existingEngagement


}

let assignAndPlanConflicts work slot = command {
        let! (slot, workConflicts)  = assignSlot work slot
        for work in workConflicts do
            do! unassignWork work slot
            do! planWork work
    }