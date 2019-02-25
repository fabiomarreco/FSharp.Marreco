type Reader<'s, 'a> = Reader of ('s -> 'a) //reader monad
with
    static member rtrn (a:'a) : Reader<'s, 'a> = Reader(fun _ -> a)

module Reader =
    let rtrn a = Reader (fun _ -> a)
    let flatten (Reader f) = (fun s -> f s |> (fun (Reader g) -> g s)) |> Reader
    let bind f (Reader ma) : Reader<'s, 'b> = ma  >> f |> Reader |> flatten
    let map f (Reader ra) = (fun s -> ra s |> f) |> Reader

type Reader<'s, 'a> with
    static member rtn = Reader.rtrn
    static member bind = Reader.bind
    static member map = Reader.map



