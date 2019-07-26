module Scheduling

open Time
open Work
open Shared


type SlotId = Period
type Slot = 
    private { Period  : Period; Engagement : Engagement option }
        with 
            member x.Id : SlotId = x.Period
            static member Engagement' = (fun s -> s.Engagement), (fun e s -> { s with Engagement = Some e } )
        // with  static member Task_ = (fun s -> s.Task), (fun t s -> { s with Task = t})

module Slot =
    let period { Period = p; Engagement = _} = p
    let engagement { Period = _; Engagement = e} = e

    let createEmpty period = { Period = period;  Engagement = None }
    let createSlotsForDay duration = Period.splitDayInPeriodsOf duration >> List.map createEmpty

    type ConflictingWork = 
        | ConflictsWithWork of Work list
        | ConflictsWithOffWork of Offwork

    let assignShallowWork work slot = 
        match slot.Engagement with
        | None -> Ok { slot with Engagement = Some (Shallow [work])}
        | Some (Shallow l) -> Ok { slot with Engagement = Some (Shallow (work::l)) }
        | Some (Deep d) -> Error (ConflictsWithWork [DeepWork d])
        | Some (Offwork o) -> Error (ConflictsWithOffWork o)

    let assignDeepWork work slot = 
        match slot.Engagement with 
        | None -> Ok { slot with Engagement = Some (Deep work)}
        | Some (Shallow l) -> l |> List.map (ShallowWork) |> ConflictsWithWork |> Error
        | Some (Deep d) -> Error <| ConflictsWithWork [DeepWork d]
        | Some (Offwork o) -> Error (ConflictsWithOffWork o)


    let assignWork work = 
        match work with 
        | ShallowWork w -> assignShallowWork w
        | DeepWork d -> assignDeepWork d

let (|Conflicts|_|) work slot = 
    match slot.Engagement, work with 
    | None, _ -> None 
    | Some (Deep d), _ -> Some ([DeepWork d])
    | Some (Shallow ws), (DeepWork _) -> Some (ws |> List.map ShallowWork)
    | Some (Shallow _), ShallowWork _ -> None
    | Some (Offwork _), _ -> None //deveria bloquear o offwork?

//================================================================

type Schedule = private {
    Date   : Date
    Planned: Work list
    Slots  : Slot list
} 

module Schedule =
    let create date duration = 
        { Date = date; Planned = []; Slots = Slot.createSlotsForDay duration date }

    let mapSlotsInPeriod fInPeriod fOutPeriod period schedule = 
        List.map (fun s -> match s.Period with | PeriodTouching period -> fInPeriod s | _ -> fOutPeriod s) schedule.Slots

    let slotsInPeriod period schedule = 
        mapSlotsInPeriod Some (const' None) period schedule |> List.choose id

    let findSlotById id schedule = schedule.Slots |> List.tryFind (fun x-> x.Id = id)

    let setSlotEngagement predicate engagement schedule = 
        //usar lensing...
        let replaceEngagement (slot:Slot) = if (predicate slot) then {slot with Engagement = engagement} else slot
        { schedule with Slots = List.map replaceEngagement schedule.Slots }
        

    // algebra

    let plan work schedule = { schedule with Planned = work::schedule.Planned }

    type ConflictingSlots = ConflictingSlots of Slot list

    let assignWork  period work schedule = 
        let inPeriod' slot = Slot.assignWork work slot |> Result.mapError (const' slot)
        mapSlotsInPeriod inPeriod' Ok period schedule
        |> List.fold(fun acc n -> match acc, n with 
                                  | Error eacc, Error s -> Error (s::eacc)
                                  | Error eacc, Ok _ -> Error (eacc)
                                  | Ok _, Error s -> Error [s]
                                  | Ok oacc, Ok an -> Ok (an::oacc)) (Ok [])
        |> Result.map (fun slots -> { schedule with Slots = slots })
        |> Result.mapError (ConflictingSlots)
//

