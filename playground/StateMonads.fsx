type Reader<'s, 'a> = Reader of ('s -> 'a) //reader monad


module Reader =
    let rtrn a = Reader (fun _ -> a)
    let flatten (Reader f) = (fun s -> f s |> (fun (Reader g) -> g s)) |> Reader
    let bind f (Reader ma) : Reader<'s, 'b> = ma  >> f |> Reader |> flatten
    let map f (Reader ra) = (fun s -> ra s |> f) |> Reader

type Reader<'s, 'a> with
    static member rtn a = Reader.rtrn a
    static member bind f m = Reader.bind f m
    static member map f m= Reader.map f m


//----------------------

module Monoid 
    type MonoidalList  =
        static member inline mempty : ^a = List<^a>.Empty
        static member inline mappend list1 list2  : 'a list = List.append list1 list2


type Writer<'w, 'a> =  Writer of ('a * 'w list) // f# nao permite declarao de um monoid generico ?

module Writer = 
    let rtrn a = Writer (a, [])
    let bind f (Writer (a, w))  = 
        let (Writer (b, w': 'w list)) = f a
        Writer (b, List.append w w')

    let map f (Writer (a, w)) = Writer (f a, w)

//--------------


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




//------------------------


//Teste do state monad

type Estado = { Usuario: string; Idade : int }

let get f = State (fun s -> f s, s) 
let pegaUsuario = State(fun (s:Estado) -> s.Usuario, s)
let pegaIdade = State(fun (s:Estado) -> s.Idade, s)
let mudaUsuario nomeUsuario = State(fun estado -> (), { estado with Usuario = nomeUsuario } )


let concatenaNome = 
    let usuario, idade = get (fun s-> s.Usuario), get (fun s-> s.Idade)
    let novoNome = State.map (fun n -> n + " FIM!") usuario
    State.bind mudaUsuario novoNome


let e0 = { Usuario = "fabio marreco"; Idade = 39}


State.run concatenaNome e0



let x = state { 
    let! usuario = get (fun s-> s.Usuario)
    let novoNome = usuario + "FIM!"
    do! mudaUsuario novoNome
}

State.run x e0

