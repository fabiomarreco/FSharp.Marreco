#load "../.paket/load/netcoreapp2.2/main.group.fsx"

open Aether


type Work = string
type Engagement = 
    | Shallow of Work list
    | Deep of Work
    with 
        static member Shallow' = (function | Shallow ws -> Some ws | _ -> None ), 
                                 (fun s -> function | Shallow _ -> Shallow s | e -> e)

        static member Deep' = (function | Deep ws -> Some ws | _ -> None ), 
                              (fun s -> function | Deep _ -> Deep s | e -> e)


type Slot = { Period : int; Eng: Engagement option}
    with static member Eng' = (fun s -> s.Eng), (fun e s -> { s with Eng = Some e})



open Aether.Operators

let getDeepWork = Slot.Eng' >?> Engagement.Deep'