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

module List = 
    let cons h t = h::t

module Result = 
    let flatten = function  
        | Ok (Ok s) -> Ok s
        | Error e -> Error e
        | Ok (Error e) -> Error e

    let cata fSuccess fFailure = function | Ok a -> fSuccess a | Error e -> fFailure e
    let bimap f g = cata (Ok << f) (Error << g)
    let comp : ('s1 -> 's2 -> 's) -> ('e -> 'e -> 'e) -> Result<'s1, 'e> -> Result<'s2, 'e> -> Result<'s, 'e> = 
        fun f g r1 r2 -> match r1, r2 with 
                         | Ok s1, Ok s2       -> Ok (f s1 s2)
                         | Error e1, Error e2 -> Error (g e1 e2)
                         | Error e1, _        -> Error (e1)
                         | _, Error e2        -> Error e2

    let biListFold racc rnext = 
        let rnext' = rnext |> Result.mapError (List.singleton) |> Result.map (List.singleton)
        comp (List.append) (List.append) racc rnext'



module Tuple2 = 
    let mapFst f (a, b) = (f a, b)
    let mapSnd f (a, b) = (a, f b)
    let kkk f a = a, f a
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


