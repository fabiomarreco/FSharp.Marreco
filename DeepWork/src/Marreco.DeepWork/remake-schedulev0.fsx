#load "../.paket/load/netcoreapp2.2/main.group.fsx"
#load "Shared.fs"
#load "Time.fs"
#load "Work.fs"
#load "Schedule.fs"

//open Marreco.DeepWork
open Work
open Scheduling
open Shared


//planwork
//unplan work
//assign slot 
//unassign slot
//type Conflicts = Slot * Work list

type SlotIdNotFound = SlotIdNotFound
type WorkAlreadyPlanned = WorkAlreadyPlanned
type WorkNotPlanned = WorkNotPlanned

type Success = Success

type CommandResult<'a> = 
    | CommandSuccess 
    | CommandFailure of 'a

module CommandResult = 
    let fromResult = function | Ok _ -> CommandSuccess | Error err -> CommandFailure err
//type Predicate<'a> = 


type CommandF<'a> = 
    | GetSchedule of (Schedule -> 'a)
//    | GetConflicts of (SlotId * Work) * (ConflictingWork option -> 'a)
    | PlanWork of Work * (CommandResult<WorkAlreadyPlanned> -> 'a)
    | UnplanWork of Work * (CommandResult<WorkNotPlanned> ->'a)
    | SetSlotEngagement of (SlotId * Engagement option) * (CommandResult<SlotIdNotFound> -> 'a)

(*
    | PlanWork of Work * (unit -> 'a)
    | UnassignWork of Work * Slot * (unit -> 'a)
    | AssignWork of Work * Slot * (Conflicts -> 'a)
*)


// Mechanical.....
module CommandF = 
    let map f = function 
        | GetSchedule next -> GetSchedule (next >> f)
        | PlanWork (input, next) -> PlanWork (input, next >> f)
        | UnplanWork (input, next) -> UnplanWork (input, next >> f)
//        | GetConflicts (input, next) -> GetConflicts (input, next >> f)
        | SetSlotEngagement (input, next) -> SetSlotEngagement (input, next >> f)

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

    let kleisli f g = f >> bind g 

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

let inline (<!>) a f = Command.map f a
let inline (>>=) a f = Command.bind f a
let inline (>=>) f g = Command.kleisli f g

//================ end mechanical

//base commands:....
let stop = Pure
let getSchedule = Free <| GetSchedule (stop)
let planWork work = Free <| PlanWork (work, stop)
let unplanWork work = Free <| UnplanWork (work, stop)
//let getConflicts slotId work = Free <| GetConflicts ((slotId, work), stop)
let setSlotEngagement slotId engagement = Free <| SetSlotEngagement ((slotId, engagement), stop)

//get commands
let mapSchedule f = Command.map f getSchedule
let slotById slotId = Schedule.findSlotById slotId |> mapSchedule
let slotsInPeriod period = Schedule.slotsInPeriod period |> mapSchedule


//target: 
type AssignWorkToSlotIdError = 
    Choice<SlotIdNotFound, ConflictingWork>

let assignWorkToSlotId slotId work   = 
    slotById slotId 
    <!> Option.toResult id (Choice1Of2 SlotIdNotFound)
    <!> Result.bind (Slot.assignWork work >> Result.mapError Choice2Of2)
    <!> CommandResult.fromResult

let assignWorkToSlotsInPeriod period work   = 
    slotsInPeriod period 
    <!> List.map (fun slot -> slot, Slot.assignWork work slot)
    <!> List.fold (fun acc (slot, n) -> 
                        match acc, n with 
                        | Ok slots, Ok slot -> Ok (slot::slots)
                        | Error es, Error e -> Error ((slot,e)::es)
                        | Error es, _ -> Error es
                        | _, Error e -> Error [slot, e]) 
        (Ok [])
    <!> CommandResult.fromResult

//========================== interpret
//========== events
type ScheduleEvent =
    | WorkPlanned of Work
    | WorkUnplanned of Work 
    | SlotEngagementAssigned of Slot


let applyEvent schedule event = 
    match event with 
    | WorkPlanned work -> Schedule.plan work schedule
    | WorkUnplanned work -> Schedule.unplan work schedule
    | SlotEngagementAssigned slot -> 
        Schedule.setSlotEngagement (slot.Id) (Slot.engagement slot) schedule
        |> Option.defaultValue schedule

//========== handlers
type CommandErrors = 
    | CmdSlotIdNotFound


type InterpretedAsEvents<'a> =  { 
    State : Schedule
    Events : ScheduleEvent list
    Result : 'a
}

let interpretAsEvents program schedule = 
    let rec loop events schedule program = 
        match program with 
        | Pure a -> match a with 
                    | CommandSuccess  -> Ok events 
                    | CommandFailure err -> Error err
        | Free (GetSchedule next) -> next schedule |> loop events schedule 
        | Free (PlanWork (work, next)) -> 
            if (Schedule.isPlanned work schedule) then 
                loop (events) schedule (next <| CommandFailure WorkAlreadyPlanned)
            else 
                let event = WorkPlanned work
                let schedule' = applyEvent schedule event
                loop (event::events) schedule' (next CommandSuccess) 
        
        | Free (UnplanWork (work, next)) -> 
            if (not <| Schedule.isPlanned work schedule) then
                loop events schedule (next <| CommandFailure WorkNotPlanned)
            else
                let event = WorkUnplanned work
                let schedule' = applyEvent schedule event
                loop (event::events) schedule' (next CommandSuccess) 
        
        | Free (SetSlotEngagement ((slotId, engagement), next)) -> 
            match Schedule.findSlotById slotId schedule with
            | None -> loop events schedule (next <| CommandFailure SlotIdNotFound)

            | Some slot -> 
                let slot' = Slot.withEngagement engagement slot
                let event = SlotEngagementAssigned slot'
                let schedule' = applyEvent schedule event
                loop (event::events) schedule' (next CommandSuccess)
    loop [] schedule program            


