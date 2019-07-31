module Commands
open Scheduling
open Time
open Work
open Shared
open Slot 


//error
type SlotIdNotFound = SlotId

(*
 * Base commands
*)

// Work plan work
type WorkPlanned = WorkPlanned of Work
type WorkPlannedError = | WorkAlreadyPlanned
let planWork work schedule = if (Schedule.isPlanned work schedule) then 
                                Ok <| WorkPlanned work 
                             else Error WorkAlreadyPlanned

// let planWork work schedule = if (Schedule.isPlanned work schedule) then 
//                                 Ok <| WorkPlanned work 
//                              else Error WorkAlreadyPlanned

// Unplan work
type WorkUnplaned = WorkUnplaned of Work
type WorkUnplannedError = PlannedWorkNotFound
let unplanWork work schedule = if (not <| Schedule.isPlanned work schedule) then 
                                    Ok <| WorkUnplaned work 
                               else Error PlannedWorkNotFound


// Assign slot engagement
type SlotAssigned = SlotAssigned of SlotId * Engagement option 
type SlotAssignmentError = SlotIdNotFound of SlotIdNotFound
let assignSlotEngagement slotId engagement schedule = 
    Schedule.findSlotById slotId schedule 
    |> (function | Some _ -> Ok <| SlotAssigned (slotId, engagement)
                 | None -> Error (SlotIdNotFound slotId))
    

(*
 * Composed commands
*)


type ConflictingSlot = SlotId * ConflictingWork

// work to slot
type AssignWorkToSlotError = 
    | ConflictingSlot of ConflictingSlot
    | SlotIdNotFound of SlotIdNotFound
let assignWorkToSlot slotId work schedule = 
    Schedule.findSlotById slotId schedule
    |> (function | Some slot -> match Slot.assignWork work slot  with 
                                | Ok s      -> Ok    <| SlotAssigned (s.Id, Slot.engagement s)
                                | Error ws  -> Error <| ConflictingSlot (slot.Id, ws)
                 | None      -> Error <| SlotIdNotFound slotId)


let assignWorkToPeriod period work schedule = 
    Schedule.slotsInPeriod period schedule
    |> List.map (Slot.assignWork work >> Result.map (fun sa -> SlotAssigned (sa.Id, Slot.engagement sa)))
    |> List.fold Result.biListFold (Ok[])

    

                             



    // let assignWorkToSlot work slotId  : Command = 
    //     fun schedule -> Schedule.findSlotById slotId schedule
    //                     |> Option.map (fun slot -> 
    //                                        Slot.assignWork work slot
    //                                        |> Result.map (fun s-> [SlotAssigned (s.Id, Slot.engagement s)])
    //                                        |> Result.mapError (fun ws -> Conflicts (slot.Id, ws)))
    //                     |> Option.defaultValue (Error SlotIdNotFound)
                        


type Event = 
    | WorkPlanned of WorkPlanned
    | WorkUnplaned of WorkUnplaned
    | SlotAssigned of SlotAssigned

type CommandError = 
    | SlotIdNotFound
    | Conflicts of SlotId * ConflictingWork

type CommandResult = Result<Event list, CommandError>

type Command = Schedule -> CommandResult


module Command = 
    let foldCommands (cs: CommandResult list) : CommandResult = 
            List.fold (fun c n -> 
                        match c, n with 
                        | Ok l, Ok ev -> Ok (ev@l)
                        | Error e, _ -> Error e
                        | _, Error e -> Error e)
                      (Ok []) cs


    let planWork work : Command = 
        fun schedule -> if (Schedule.isPlanned work schedule) then Ok []
                        else Ok [WorkPlanned work]

    let unplanWork work : Command = 
        fun schedule -> if (Schedule.isPlanned work schedule) then Ok [WorkUnplaned work]
                        else Ok []

    let assignSlot engagement slotId : Command = 
        fun schedule -> if (Option.isSome <| Schedule.findSlotById slotId schedule) 
                            then Ok [SlotAssigned (slotId, engagement)]
                        else Error SlotIdNotFound


    let assignWorkToSlot work slotId  : Command = 
        fun schedule -> Schedule.findSlotById slotId schedule
                        |> Option.map (fun slot -> 
                                           Slot.assignWork work slot
                                           |> Result.map (fun s-> [SlotAssigned (s.Id, Slot.engagement s)])
                                           |> Result.mapError (fun ws -> Conflicts (slot.Id, ws)))
                        |> Option.defaultValue (Error SlotIdNotFound)
                        

    let assignWorkToPeriod work period = 
        fun schedule -> Schedule.mapSlotsInPeriod 
                            (fun s -> assignWorkToSlot work s.Id schedule)
                            (fun _ -> Ok [])
                            period
                            schedule
                        |> foldCommands


    let unassignSlot slotId  : Command = 
        fun schedule -> Schedule.findSlotById slotId schedule
                        |> Option.map (fun s -> 
                                        if (Option.isSome (Slot.engagement s)) then (Ok [SlotAssigned (slotId, None)])
                                        else Ok [])
                        |> Option.defaultValue (Error SlotIdNotFound)


    let assignWorkAndPlanConflicts period work = 
        fun schedule -> Schedule.mapSlotsInPeriod 
                            (fun s -> assignWorkToSlot work s.Id schedule)
                            (fun _ -> Ok [])
                            period
                            schedule
                        |> List.collect
                            (function 
                             | Error (Conflicts (slotId, ConflictsWithWork ws)) -> 
                                let unassign = unassignSlot slotId schedule
                                let plans = List.map (fun w -> planWork w schedule) ws
                                // faltando o evento de assignment...
                                
                                unassign::plans
                             | x -> [x])
                        |> foldCommands