open NodaTime
open NodaTime
open System
open NodaTime
#r "paket: //
    nuget NodaTime"

#load ".fake/Playground.fsx/intellisense.fsx"
open NodaTime
open System

let instant = Instant.FromDateTimeOffset(DateTimeOffset.Now)

let i = Interval(instant, instant.Plus(Duration.FromSeconds(15.)))


let (|First|NO|_|) a (h::t) = 
    if (h = a) then Some(First) else Some(NO)


let (|A|_|) (x:int) (p:int) = 
    Some A
   


match ([1;2;3;4]) with | First 1 -> "asa" | _ -> "no"