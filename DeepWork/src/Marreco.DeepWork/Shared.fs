module Shared
open System

// shared
let const' a _ = a


//result
[<AutoOpen>]
module ResultOperators = 
    let inline (<!>) r f = Result.map f r
    let inline (<*!>) r f = Result.mapError f r


module Option = 
    let toResult fSuccess error opt = Option.fold (fun _ a -> Ok (fSuccess a)) (Error error) opt





//------------

module Optics = 
    type Lens<'a, 'b> = ('a -> 'b) * ('b -> 'a -> 'a)
    type Prism<'a, 'b> = ('a -> 'b option) * ('b -> 'a -> 'a)


type TBD = Undefined // To be defined...

[<Obsolete("Undefined Implementation")>]
let NotImplemented _ = raise (NotImplementedException())


//Apply the events to the current state, bringing the aggregate state Up to Date
type ApplyEvents<'state, 'event> = 'state -> 'event -> 'state

type CommandHandler<'state, 'command, 'event, 'error> = 'state -> 'command -> Result<'event list, 'error>


