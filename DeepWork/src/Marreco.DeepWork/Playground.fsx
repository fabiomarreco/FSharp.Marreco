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

i.ToString()