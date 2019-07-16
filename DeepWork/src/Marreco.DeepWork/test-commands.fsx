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

module Event = 
    let apply schedule event = 
        match event with 
        | WorkPlanned work -> Schedule.plan work schedule
        | SlotUnassigned (id, _) -> schedule |> Schedule.setSlotEngagement (fun s -> s.Id = id) None  
        | SlotsAssigned (engagement, ids) ->
             schedule |> Schedule.setSlotEngagement (fun s -> List.contains s.Id ids) (Some engagement)

module TestHandlers = 
    module Handlers = 
        let planWork work = (WorkPlanned work)
        let unassignSlot (schedule:Schedule) slotId = 
            schedule |> Schedule.findSlotById slotId |> Option.bind (Slot.engagement) 
        let assignSlot schedule (period, engagement) = 
            Schedule.tryAssignEngagement period engagement schedule

    let rec interpretAsEvents schedule command = 
        let continue' event cont = match event with 
                                   | Some x -> let schedule' = Event.apply schedule x 
                                               x::(interpretAsEvents schedule' cont)
                                   | None -> interpretAsEvents schedule cont
        match command with 
        | Pure a -> a
        | Free (PlanWork (w, next)) -> 
            continue' (Handlers.planWork w |> Some) (next())
        | Free (UnassignSlot (id, next)) ->  
            let res = Handlers.unassignSlot schedule id 
            continue' (SlotUnassigned (id, res) |> Some) (next(res))
        | Free (AssignSlot (input, next)) -> 
            let res = Handlers.assignSlot schedule input
            match res with 
            | Ok ids -> continue' (SlotsAssigned ((snd input), ids) |> Some) (next (Ok ids))
            | Error err -> continue' None (next (Error err))


    open Schedule
    let rec interpretAsEvents2 schedule command current : Result<Event list, SlotAssignmentError> = 
        // let continue' slot cont = match slot with 
        //                           | Some x -> let schedule' = Event.apply schedule x 
        //                                       x::(interpretAsEvents schedule' cont)
        //                           | None -> interpretAsEvents schedule cont
        match command with 
        | Pure a -> a
        | Free (PlanWork (w, next)) -> 
            match current with 
            | Ok (schedule, events) -> 
                let event' = WorkPlanned w 
                let schedule' = Event.apply schedule event'
                (schedule', event'::events) |> Ok |> interpretAsEvents2 schedule' (next()) 
            | Error e -> Error e
        | Free (UnassignSlot (id, next)) ->  
            match current with 
            | Ok (schedule, events) -> 
                let res = Handlers.unassignSlot schedule id 
                let event' = SlotUnassigned (id, res) 
                let schedule' = Event.apply schedule event'
                (schedule', event'::events) |> Ok |> interpretAsEvents2 schedule' (next res) 
            | Error e -> Error e
        | Free (AssignSlot ((period, engagement), next)) -> 
            match current with 
            | Ok (schedule, events) -> 
                let res = Handlers.assignSlot schedule (period, engagement)
                match res with 
                | Ok ids -> 
                    let event' = SlotsAssigned (engagement, ids) 
                    let schedule' = Event.apply schedule event'
                    (schedule', event'::events) |> Ok |> interpretAsEvents2 schedule' (next res) 
                | Error e -> Error e |> interpretAsEvents2 schedule (next res)
            | Error e -> Error e


    //-----------------------------
    let schedule = Schedule.create (Date.today) (Duration.fromMinutes 30)
    let ev = interpretAsEvents schedule

    type Plan = Work -> unit
    type Unasssign = SlotId -> Engagement option
    type Assign = (Period * Engagement) -> Result<SlotId, Schedule.SlotAssignmentError>
    type FAppend<'State, 'a> = 'State -> 'a -> 'State

    let rec cata fPure fPlan fAssign fUnassign  command = 
        let recurse = cata fPure fPlan fAssign fUnassign 
        match command with 
        | Pure a -> fPure a
        | Free (PlanWork (w, next)) -> fPlan w (recurse (next()))
        | Free (AssignSlot (w, next)) -> fAssign w (next >> recurse)
        | Free (UnassignSlot (w, next)) -> fUnassign w (next >> recurse)

    let fpure = id
    let fplan work (schedule, eventList) = 
        let event' = (WorkPlanned work)
        let schedule' = Event.apply schedule event'
        schedule', event'::eventList

    let fassign (period, engagement) cont'  = 
        let res = Handlers.assignSlot schedule (period, engagement)
        let (schedule, eventList) = cont' res
        match res with 
        | Ok ids -> 
            let event' = SlotsAssigned (engagement, ids)
            let schedule' = Event.apply schedule event' 
            schedule', event'::eventList
        | Error err -> 






    let rec fold fplan funassign fassign state = function
        | Pure s -> s
        | Free (PlanWork (i, next)) -> 
            let state' = fplan state i
            fold fplan funassign fassign (state') (next())
        | Free (UnassignSlot (i, next)) -> 
            let state' = funassign state i
            fold fplan funassign fassign (state') 


    let rec fold' f state = function 
        | [] -> state
        | h::t -> fold f (f state h) t


    
    