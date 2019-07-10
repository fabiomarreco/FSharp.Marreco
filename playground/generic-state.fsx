
type State<'s, 'a> = State of ('s -> ('a * 's))

module State = 
    let rtrn a = (fun s -> (a, s)) |> State
    let flatten (State sa) = (fun s -> sa s |> (fun ((State sa'), s2) -> sa' s2)) |> State 
    let map  f (State sa) = (fun s ->  sa s |> (fun (a, s2) -> f a, s2) ) |> State
    let bind f sa = map f sa |> flatten
    let run (State st) s = st s 


type StateBuilder() = 
    member x.Return (a) = State.rtrn a
    member x.ReturnFrom a = a
    member x.Bind (s, f) = State.bind f s

let state = new StateBuilder()

let lexec (f: 'm -> 'm) = State (fun m -> (), f m)
Map.find

let lget (f: 'm -> 'b) = State(fun m -> f m, m)

let madd a b= Map.add a b |> lexec
let mget = Map.find >> lget

let test = state {
    do! madd "fabio" 1
    do! madd "catunda" 2
    do! madd "catunda" 3

    let! total1 = Map.fold(fun acc _ v -> acc + v) 0 |> lget

    let! id = mget "fabio"

    do! madd "fabio2" id
    let! total2 = Map.fold(fun acc _ v -> acc + v) 0 |> lget

    return total1, total2
} 


Map.add