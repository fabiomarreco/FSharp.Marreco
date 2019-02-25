let m = [1..10]

type Monoid = Monoid
with 
    static member inline mempty : 'a list= []
    static member inline mappend (a, b) = List.append a b
    static member inline mempty : string = ""
    static member inline mappend : string -> string -> string = fun a b -> a + b

type List<'a> with 
    static member mempty : 'a list= []
    static member mappend (a, b) = List.append a b


type System.String with 
    static member inline mempty : string = ""
    static member inline mappend : string -> string -> string = fun a b -> a + b



//---------------------

type Team  = { TeamName : string; Members : int }
with
    static member mempty : Team = { TeamName = ""; Members = 0}
    static member mappend (t1, t2) = { TeamName = t1.TeamName + t2.TeamName; Members = t1.Members + t2.Members }
//------------

let inline myFold (m:^Monoid seq) = 
    let empty  = (^Monoid : (static member mempty : ^Monoid) ())
    let append = fun a b -> (^Monoid : (static member mappend : ^Monoid -> ^Monoid -> ^Monoid) a, b)
    //let append = fun a b -> (^Monoid : (static member  mappend : (^Monoid -> ^Monoid -> ^Monoid)) a, b)
    Seq.fold append empty m




let team1 = { TeamName = "Alfa"; Members = 3}
let team2 = { TeamName = "Beta"; Members = 2}

myFold [ team1; team2]


myFold [ [1..2]; [3..4] ]


type MonoidalString =  { str : string}
with
    static member inline mempty : MonoidalString = {str = ""}
    static member inline mappend : MonoidalString -> MonoidalString -> MonoidalString = fun a b -> { str =a.str + b.str }


