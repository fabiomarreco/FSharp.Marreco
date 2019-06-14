namespace Marreco.DeepWork
open System
type TBD = Undefined // To be defined...

type Date = System.DateTime
type Time = System.TimeSpan
type Duration = System.TimeSpan

type IsInsideOfPeriod = 
    | Inside
    | Before
    | After
    | PartiallyInside

type Period = { Start : Time; Duration : Duration } with 
    member x.End : Time = x.Start.Add(x.Duration)
    member x.InBetween t = (t >= x.Start) && (t <= x.End)
    member x.IsInsideOf p = 
        if (x.End < p.Start) then Before
        else if (x.Start > p.End) then After
        else if (p.Start <= x.Start && p.End >= x.End) then Inside
        else PartiallyInside

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


module Slot = 
    type Slot = private { 
        Period    : Period
        Assignment: Assignment option
    }

    let createEmpty (p:Duration) = 
        let nSlots = TimeSpan.FromDays(1.).TotalSeconds / p.TotalSeconds |> int
        [1..(nSlots-1)] 
            |> Seq.scan (fun (last:TimeSpan) _ ->  last.Add (p)) TimeSpan.Zero
            |> Seq.cast
            |> Seq.map (fun s -> { Period = { Start = s; Duration = p };  Assignment = None })
            |> Seq.toList
    
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




