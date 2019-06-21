namespace Marreco.DeepWork

open Time
open Work

module Slot =
    type Slot = private {
        Period    : Period
        Assignment: Assignment option
    }

    let createEmpty period = { Period = period;  Assignment = None }
    let createSlotsForDay duration = Period.splitDayInPeriodsOf duration >> List.map createEmpty

module Schedule =
    open Slot
    type DailySchedule = private {
        Date   : Date
        Planned: Work list
        Slots  : Slot list
    }

    let createEmpty date p = { Date = date; Planned = []; Slots = Slot.createEmpty p }

    type ScheduleDuration =
        | Duration of Duration
        | Slots of int


    type ScheduleFailures =
        | StartTimeDoesNotMatchSlot

    let schedule work period day =
        let start = period.Start
        let end' = period.End
        let rec apply =
            function
            | [] -> Ok []
            | h::t when inRange(h) ->
                { h with }




