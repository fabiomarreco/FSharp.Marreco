module Scheduling

open Time
open Work

type Slot = 
    private { Period  : Period; Task : Task option } with  
        static member Task_ = (fun s -> s.Task), (fun t s -> { s with Task = t})

module Slot =
    let createEmpty period = { Period = period;  Task = None }
    let createSlotsForDay duration = Period.splitDayInPeriodsOf duration >> List.map createEmpty


type DailySchedule = private {
    Date   : Date
    Planned: Work list
    Slots  : Slot list
} 

module Schedule =
    open Slot

    //  period -> task -> slot list -> (slot list, task list)

    let create date duration = 
        { Date = date; Planned = []; Slots = Slot.createSlotsForDay duration date }

    let assignTask period task schedule = 
        let (newSlots, tasks) = List.fold (fun (slots, tasks) nextSlot -> 
                                            match nextSlot.Period with 
                                            | PeriodTouching period 
                                                nextSlot::slots, Option.fold (fun ts t -> t::ts) tasks nextSlot.Task
                                            | _ -> (slots, tasks)) ([], [])
                                <| schedule.Slots                                

        { schedule with Slots = newSlots}                            

    type ScheduleEvents = 
        | SlotAssigned of Slot
        | WorkPlanned of Work

    type ScheduleEventErrors = 
        | NoSlotsAvailable

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




