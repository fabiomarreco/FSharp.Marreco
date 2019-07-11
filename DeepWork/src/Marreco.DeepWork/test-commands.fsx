//-------------- EXPERIMENT
#if INTERACTIVE
#load "Shared.fs"
#load "Time.fs"
#load "Work.fs"
#load "Schedule.fs"
#endif


(*
TODO: 
- [ ] Implementar o free monad para os comandos
- [ ] Criar um comando para dar assign e plan dos conflitos (force?)
- [ ] Criar um interpreter que gera eventos ?

 *)
open Time
open Work
open Scheduling


type CommandF<'a> = 
    | PlanWork of Work * (unit -> 'a)
    | AssignSlot of (Period * Engagement) * (Result<SlotId list, Schedule.SlotAssignmentError> -> 'a)
    | UnassignSlot of SlotId * (Engagement option -> 'a)

module CommandF = 
    let map f = function
        | PlanWork (w, next) -> PlanWork (w, next >> f)
        | AssignSlot (w, next) -> AssignSlot (w, next >> f)
        | UnassignSlot (id, next) -> UnassignSlot (id, next >> f)

type Command<'a> = 
    | Pure of 'a
    | Free of CommandF<Command<'a>>


module Command = 
    let retrn = Pure
    let rec map f = function | Pure a -> Pure (f a) 
                             | Free c -> CommandF.map (map f) c |> Free
    let rec bind f = function | Pure a -> f a 
                              | Free c -> CommandF.map (bind f) c |> Free


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

// END OF MONAD

module Commands = 
    let stop = Pure
    let planWork work = PlanWork (work, stop) |> Free
    let assignSlot period engagement = AssignSlot ((period, engagement), stop) |> Free
    let unassignSlot id  = UnassignSlot (id, stop) |> Free
    let unassignAndPlan id = command {
        match! unassignSlot id with
        | Some (Deep w) -> do! planWork (DeepWork w)
        | Some (Shallow ws) -> for w in ws do 
                                do! planWork (ShallowWork w)
        | _ -> do ignore()
    }

                              

/// END OF COMMAND DEFINITION





type Event = 
    | SlotsAssigned of Engagement * SlotId list
    | SlotUnassigned of SlotId * Engagement option
    | WorkPlanned of Work

module TestHandlers = 
    module Handlers = 
        let planWork work = WorkPlanned work
        let unassignSlot (schedule:Schedule) slotId = 
            schedule |> Schedule.findSlotById slotId |> Option.bind (Slot.engagement) 
        let assignSlot schedule (period, engagement) = 
            Schedule.tryAssignEngagement period engagement schedule

    let rec interpretAsEvents schedule  = function
        | Pure a -> a
        | Free (PlanWork (w, next)) -> (Handlers.planWork w)::(interpretAsEvents schedule (next()))
        | Free (UnassignSlot (id, next)) ->  
            let eg = Handlers.unassignSlot schedule id 
            SlotUnassigned (id, eg)::(interpretAsEvents schedule (next(eg)))
        | Free (AssignSlot (input, next)) -> 
            let res = Handlers.assignSlot schedule input
            match res with 
            | Ok ids -> (SlotsAssigned ((snd input), ids))::(interpretAsEvents schedule (next (Ok ids)))
            | Error err -> (interpretAsEvents schedule (next (Error err)))

module TestHandlers2 = 
    type CommandError = Undefined
    module Handlers = 
        type CommandHandler<'input> = 'input -> Schedule -> Result<Event list, CommandError>
        let planWork work = WorkPlanned work
        let unassignSlot (schedule:Schedule) slotId = 
            schedule |> Schedule.findSlotById slotId |> Option.bind (Slot.engagement) 
        let assignSlot schedule (period, engagement) = 
            Schedule.tryAssignEngagement period engagement schedule

    let rec interpretAsEvents schedule  = function
        | Pure a -> a
        | Free (PlanWork (w, next)) -> (Handlers.planWork w)::(interpretAsEvents schedule (next()))
        | Free (UnassignSlot (id, next)) ->  
            let eg = Handlers.unassignSlot schedule id 
            SlotUnassigned (id, eg)::(interpretAsEvents schedule (next(eg)))
        | Free (AssignSlot (input, next)) -> 
            let res = Handlers.assignSlot schedule input
            match res with 
            | Ok ids -> (SlotsAssigned ((snd input), ids))::(interpretAsEvents schedule (next (Ok ids)))
            | Error err -> (interpretAsEvents schedule (next (Error err)))


(*
    let assign what when' schedule = 
        let rec getevents slots = 
            match slots with
            | [] -> Error NoSlotsAvailable
            | h::t -> match h.Period with       
                      | Before when' -> getevents t
                      | After when' -> Error NoSlotsAvailable
                      | InsideOf when' -> match h.Assignment with
                                          | None -> 
*)

    // type ScheduleDuration =
    //     | Duration of Duration
    //     | Slots of int


    // type ScheduleFailures =
    //     | StartTimeDoesNotMatchSlot

    // let schedule work period day =
    //     let start = period.Start
    //     let end' = period.End
    //     let rec apply =
    //         function
    //         | [] -> Ok []
    //         | h::t when inRange(h) ->
    //             { h with }




