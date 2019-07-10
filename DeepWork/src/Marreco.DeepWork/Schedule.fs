module Scheduling

open Time
open Work

type SlotId = Period
type Slot = 
    private { Period  : Period; Engagement : Engagement option }
        with 
            member x.Id : SlotId = x.Period
        // with  static member Task_ = (fun s -> s.Task), (fun t s -> { s with Task = t})

module Slot =
    let createEmpty period = { Period = period;  Engagement = None }
    let createSlotsForDay duration = Period.splitDayInPeriodsOf duration >> List.map createEmpty

let (|Conflicting|_|) engagement slot  = 
    match engagement, slot.Engagement with 
    | _, None -> None
    | Shallow _, Some (Shallow _) -> None
    | Shallow _, d -> d
    | Deep _, x -> x
    | Offwork _, x -> x
     

type DailySchedule = private {
    Date   : Date
    Planned: Work list
    Slots  : Slot list
} 

module Schedule =
    let create date duration = 
        { Date = date; Planned = []; Slots = Slot.createSlotsForDay duration date }

    let slotsInPeriod period schedule = 
        List.choose (fun s -> match s.Period with | PeriodTouching period -> Some s | _ -> None) schedule.Slots

    // algebra

    let plan work schedule = { schedule with Planned = work::schedule.Planned }

    type SlotConflicts = (SlotId * Engagement) list
    type SlotAssignmentError = 
        | NoMatchingSlots
        | SlotsConflicted of SlotConflicts

    (*
     * tries to assign an engagement to slots inside a period, it returns either
     *   - Success with matching slots 
     *   - Failure stating that either no slot was found or there were conflicts
     *)
    let tryAssignEngagement period engagement schedule = 
        let matchingSlots = slotsInPeriod period schedule
        if (matchingSlots.IsEmpty) then Error NoMatchingSlots
        else 
            let conflicts = 
                matchingSlots 
                |> List.fold (fun acc next -> 
                                match next with
                                | Conflicting engagement c -> (next.Id, c)::acc
                                | _ -> acc
                             ) []
            let matchingSlotsId = matchingSlots |> List.map (fun x -> x.Id)
            if (conflicts.IsEmpty) then Ok matchingSlotsId
            else Error (SlotsConflicted conflicts)



//-------------- EXPERIMENT
#if INTERACTIVE
#load "Shared.fs"
#load "Time.fs"
#load "Work.fs"
#endif


(*
TODO: 
- [ ] Implementar o free monad para os comandos
- [ ] Criar um comando para dar assign e plan dos conflitos (force?)
- [ ] Criar um interpreter que gera eventos ?

 *)


type Events = 
    | SlotsAssigned of Engagement * SlotId list
    | SlotUnassigned of SlotId
    | WorkPlanned of Work

type CommandF<'a> = 
    | PlanWork of Work * (unit -> 'a)
    | AssignSlot of (Period * Engagement) * (Result<SlotId list, Schedule.SlotAssignmentError> -> 'a)
    | UnassignSlot of SlotId * (Engagement option -> 'a)

let map f = function
    | PlanWork (w, next) -> PlanWork (w, next >> f)
    | AssignSlot (w, next) -> AssignSlot (w, next >> f)
    | UnassignSlot (w, next) -> UnassignSlot (w, next >> f)

type Command<'F,'a> = 
    | Pure of 'a
    | Free of 'F

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




