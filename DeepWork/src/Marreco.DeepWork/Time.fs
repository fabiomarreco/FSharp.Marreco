namespace Marreco.DeepWork

module Time = 
    open System
    type Date = private Date of DateTime
        with member x.ToDateTime() = (fun (Date d) -> d) <| x
             override x.ToString() = x.ToDateTime().ToString("yyyy-MM-dd")
    module Date = 
        let create y m d = Date <| DateTime(y, m, d)
        let createFromDate (d:DateTime) = Date <| d.Date

    type Duration = Duration of TimeSpan
    module Duration = 
        let fromMinutes = float >> TimeSpan.FromMinutes >> Duration


    type TimeOfDay = private TimeOfDay of System.TimeSpan
    module TimeOfDay = 
        let create (ts:TimeSpan) = 
            if (ts.TotalDays < 0. || ts.TotalDays > 1.) then None
            else Some <| TimeOfDay ts 


    type Moment = Moment of DateTime

    module Moment = 
        let add (Duration d) (Moment m) = m.Add(d) |> Moment

    type Period = { Start : Moment; Duration : Duration } with
        member x.End = Moment.add x.Duration x.Start
        member x.InBetween t = (t >= x.Start) && (t <= x.End)

    let (|InsideOf|Before|After|PartiallyInsideOf|) (x:Period) p = 
        if (x.End < p.Start) then Before
        else if (x.Start > p.End) then After
        else if (p.Start <= x.Start && p.End >= x.End) then InsideOf
        else PartiallyInsideOf

    module Period = 
        let splitDayIn (Duration d) (Date day)= 
            let nSlots = TimeSpan.FromDays(1.).TotalMilliseconds / d.TotalMilliseconds |> int
            [1..nSlots] 
                |> Seq.scan (fun (last:TimeSpan) _ ->  last.Add (d)) TimeSpan.Zero
                |> Seq.map (day.Add >> Moment >> (fun x-> { Start = x; Duration = Duration d}))
                |> Seq.toList
