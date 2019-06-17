namespace Marreco.DeepWork
open System
type TBD = Undefined // To be defined...

type DeepWork = TBD
type ShallowWork = TBD
type Offwork = TBD

type Work =
    | Deep    of DeepWork
    | Shallow of ShallowWork

type Assignment =
    | Deep    of DeepWork
    | Shallow of ShallowWork list
    | Offwork of Offwork

open Time

module Slot =
    type Slot = private {
        Period    : Period
        Assignment: Assignment option
    }

    let createEmpty period = { Period = period;  Assignment = None }
    let createSlotsForDay duration day= Period.splitDayIn duration day |> List.map createEmpty

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




