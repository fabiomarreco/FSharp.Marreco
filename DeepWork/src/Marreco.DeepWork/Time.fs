namespace Marreco.DeepWork

module Time = 
    open System
    //Date
    type Date = private Date of DateTime
        with member x.ToDateTime() = (fun (Date d) -> d) <| x
             override x.ToString() = x.ToDateTime().ToString("yyyy-MM-dd")
             member x.DayOfWeek = x.DayOfWeek

    module Date = 
        let today = DateTime.Today |> Date
        let create y m d = Date <| DateTime(y, m, d)
        let createFromDate (d:DateTime) = Date <| d.Date
        let previous (Date d) = d.AddDays(-1.) |> Date
        let next (Date d) = d.AddDays(1.) |> Date
        let rec find selector predicate date : Date = if (predicate date) then date else find selector predicate  (selector date)


    //Duration
    type Duration = Duration of TimeSpan
        with static member (/) (Duration d1, Duration d2) = d1.TotalMilliseconds / d2.TotalMilliseconds 
    module Duration = 
        let fromMinutes = float >> TimeSpan.FromMinutes >> Duration
        let fullDay = TimeSpan.FromDays (1.) |> Duration


    //TimeOfDay
    type TimeOfDay = private TimeOfDay of System.TimeSpan
    module TimeOfDay = 
        let Midnight = TimeSpan.Zero
        let fromTimeSpan (ts:TimeSpan) = 
            if (ts.TotalDays < 0. || ts.TotalDays > 1.) then None
            else Some <| TimeOfDay ts 


    //Moment
    type Moment = Moment of DateTime
    module Moment = 
        let StartOfDay (Date d) = d |> Moment
        let add (Duration d) (Moment m) = m.Add(d) |> Moment
        let fromDateAndTime (Date d) (TimeOfDay t) = Moment <| d.Add(t)

    //Period
    type Period = { Start : Moment; Duration : Duration } with
        member x.End = Moment.add x.Duration x.Start
        member x.ContainsMoment t = (t >= x.Start) && (t <= x.End)

    let (|InsideOf|Before|After|PartiallyInsideOf|) (x:Period) p = 
        if (x.End < p.Start) then Before
        else if (x.Start > p.End) then After
        else if (p.Start <= x.Start && p.End >= x.End) then InsideOf
        else PartiallyInsideOf

    module Period = 
        let splitDayInPeriodsOf duration day = 
            let nPeriods = Duration.fullDay / duration |> int
            [1..nPeriods] 
                |> Seq.scan (fun m _ ->  Moment.add duration m) (Moment.StartOfDay day)
                |> Seq.map (fun x-> { Start = x; Duration = duration})
                |> Seq.toList


    type Week = Week of StartDate : Date
