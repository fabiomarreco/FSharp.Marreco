module Scheduling

open Time
open Work


type SlotId = Period
type Slot = 
    private { Period  : Period; Engagement : Engagement option }
        with 
            member x.Id : SlotId = x.Period
            static member Engagement_ = ()
        // with  static member Task_ = (fun s -> s.Task), (fun t s -> { s with Task = t})

module Slot =
    let period { Period = p; Engagement = _} = p
    let engagement { Period = _; Engagement = e} = e

    let createEmpty period = { Period = period;  Engagement = None }
    let createSlotsForDay duration = Period.splitDayInPeriodsOf duration >> List.map createEmpty

let (|Conflicts|_|) work slot = 
    match work, slot.Engagement with 
    | _, None -> None
    | (ShallowWork _), (Some (Shallow _)) -> None 
    | _, (Some d) -> Some d

let (|Conflicting|_|) engagement slot  = 
    match engagement, slot.Engagement with 
    | _, None -> None
    | Shallow _, Some (Shallow _) -> None
    | Shallow _, d -> d
    | Deep _, x -> x
    | Offwork _, x -> x
     

type Schedule = private {
    Date   : Date
    Planned: Work list
    Slots  : Slot list
} 

module Schedule =
    let create date duration = 
        { Date = date; Planned = []; Slots = Slot.createSlotsForDay duration date }

    let slotsInPeriod period schedule = 
        List.choose (fun s -> match s.Period with | PeriodTouching period -> Some s | _ -> None) schedule.Slots

    let findSlotById id schedule = schedule.Slots |> List.tryFind (fun x-> x.Id = id)

    let setSlotEngagement predicate engagement schedule = 
        //usar lensing...
        let replaceEngagement (slot:Slot) = if (predicate slot) then {slot with Engagement = engagement} else slot
        { schedule with Slots = List.map replaceEngagement schedule.Slots }
        

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

