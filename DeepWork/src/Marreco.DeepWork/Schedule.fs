namespace Marreco.DeepWork

open Time
open Work

module Slot =
    type Slot = private {
        Period    : Period
        Assignment: Assignment option
    }

    let createEmpty period = { Period = period;  Assignment = None }
    let createSlotsForDay duration ssdad = Period.splitDayInPeriodsOf duration >> List.map createEmpty

module Schedule =
    open Slot
    type DailySchedule = private {
        Date   : Date
        Planned: Work list
        Slots  : Slot list
    }

    let createEmpty date duration = 
        { Date = date; Planned = []; Slots = Slot.createSlotsForDay duration date }

    type ScheduleEvents = 
        | SlotAssigned of Slot
        | WorkPlanned of Work

    type ScheduleEventErrors = 
        | NoSlotsAvailable

    let assign what when' schedule = 
        let rec getevents slots = 
            match slots with
            | [] -> Error NoSlotsAvailable
            | h::t -> match h.Period with       
                      | Before when' -> getevents t
                      | After when' -> Error NoSlotsAvailable
                      | InsideOf when' -> match h.Assignment with
                                          | None -> 


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




