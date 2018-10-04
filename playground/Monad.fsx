type Result<'a, 'err> with
     static member Map f a = Result.map f a
     static member Return a = Result.Ok a
     static member Bind f a = Result.bind 


module Monad = 
    let inline map (f:^f) (mA:^mA) : ^mB= 
        (^mA : (static member Map : ^f -> ^mA -> ^mB) f, mA)

    let inline retn (a: ^a) : ^m= 
        (^m : (static member Return : ^a -> ^m)  a)

    let inline bind (f: ^f) (mA:^mA) : ^mB = 
        (^mB : (static member Bind : ^f -> ^mA -> ^mB)  f, mA)

    let inline apply f a = bind (fun f' -> map f' a) f

    let inline kleisli f g = f >> bind g


    module Operators = 
        let inline (<!>) f a = map f a
        let inline (>>=) a f = bind f a 
        let inline (<*>) f a = apply f a
        let inline (>=>) f g = kleisli f g

        let inline (!+) a = retn a

