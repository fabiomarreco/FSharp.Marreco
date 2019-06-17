namespace Marreco.DeepWork

type Date = System.DateTime
type Time = System.TimeSpan
type Duration = System.TimeSpan

type Period = { Start : Time; Duration : Duration } with
    member x.End : Time = x.Start.Add(x.Duration)
    member x.InBetween t = (t >= x.Start) && (t <= x.End)

module Period = 
    let (|InsideOf|Before|After|PartiallyInsideOf|) (x:Period) p = 
        if (x.End < p.Start) then Before
        else if (x.Start > p.End) then After
        else if (p.Start <= x.Start && p.End >= x.End) then InsideOf
        else PartiallyInsideOf
