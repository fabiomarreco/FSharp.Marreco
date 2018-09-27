
type ParserResult<'a> = 
    | Success of 'a * list<char>
    | Failure


type Parser<'a> = list<char> -> ParserResult<'a>

module Parsers = 
    let private uncurry f = fun x y -> f (x,y)
    let Parse (str:string) (p:Parser<'a>) = str.ToCharArray() |> List.ofArray |> p
    let CharParser (c: char): Parser<char> = 
        function
        | h::t when c = h -> Success (h, t)
        | _ -> Failure

    let Either (p1: Parser<'a>) (p2: Parser<'a>) : Parser<'a> = 
        let p stream = 
            match p1 stream with
            | Failure -> p2 stream
            | s -> s
        in p

    let (<|>) p1 p2 = Either p1 p2

    let Bind (p: Parser<'a>) (f: 'a -> Parser<'b>) : Parser<'b> = 
        let q stream =
            match p stream with 
            | Success (x, rest) -> (f x) rest
            | Failure -> Failure
        in q

    let (>>=) = Bind

    let Kleisli (f: 'a -> Parser<'b>) (g: 'b -> Parser<'c>) : ('a -> Parser<'c>) = 
        f >> (fun p -> Bind p g)

    let (>=>) = Kleisli

    let CharParserCI (c: char): Parser<char> = 
        [ System.Char.ToLowerInvariant(c); System.Char.ToUpperInvariant(c) ]
        |> List.map CharParser
        |> List.reduce Either


    let Apply (p: Parser<'a>) (f: 'a -> 'b) : Parser<'b> =
        let q stream =
            match p stream with
            | Success(x, rest) -> Success(f x, rest)
            | Failure -> Failure
        in q

    let map f p = Apply p f

    let DigitParser = ['0'..'9'] |> List.map CharParser |> List.reduce Either

    let DigitParserInt = Apply DigitParser (fun c -> (int c) - (int '0'))

    let rec ZeroOrMore (p: Parser<'a>) : Parser<List<'a>> = 
        let q stream = 
            match p stream with 
            | Failure -> Success([], stream); 
            | Success (x, rest) -> Apply (ZeroOrMore p) (fun t -> x::t) <| rest
        in q


    let AtLeastOne (p :Parser<'a>) : Parser<List<'a>> = 
        let q stream = 
            match p stream with
            | Failure -> Failure
            | Success (x, rest) -> Apply (ZeroOrMore p) (fun t -> x::t) <| rest
        in q
        
    let UnsignedIntegerParser : Parser<int> = Apply (AtLeastOne DigitParserInt) (List.reduce (fun x y -> x * 10 + y))

    let pSuccess x : Parser<'a> = 
        let p stream =  Success (x, stream)
        in p

    let pFail x : Parser<'a> = 
        let p stream = Failure
        in p


    let Combine (f:'a -> 'b -> 'c) (p1:Parser<'a>) (p2:Parser<'b>) : Parser<'c> = 
        p1 >>= (fun a -> Apply p2 (fun b -> f a b))

    let IntParser : Parser<int> = 
        let sign = CharParser '-' <|> CharParser '+' <|> pSuccess '+' |> map (function | '-' -> -1 | _ -> 1)
        Combine (*) sign UnsignedIntegerParser


    let MatchBothGetLeft (p1:Parser<'a>) (p2:Parser<'b>) : Parser<'a> = 
        Combine (fun x _ -> x) p1 p2

    let MatchBothGetRight (p1:Parser<'a>) (p2:Parser<'b>) : Parser<'b> = 
        Combine (fun _ y -> y) p1 p2

    let MatchBothGetBoth (p1:Parser<'a>) (p2:Parser<'b>) : Parser<'a*'b> = 
        Combine (fun x1 x2 -> x1, x2) p1 p2
    let (.>>) = MatchBothGetLeft
    let (>>.) = MatchBothGetRight
    let (.>>.) = MatchBothGetBoth

    let FloatParser = 
        let prepend x l = x::l
        let decParser  = CharParser '.' >>. Apply (ZeroOrMore DigitParserInt) (prepend 0 >> List.map float >> List.reduceBack (fun n acc -> acc / 10. + n))
        Combine (+) (IntParser |> map float) decParser

    let DecimalParser = 
        let prepend x l = x::l
        let decParser  = CharParser '.' >>. Apply (ZeroOrMore DigitParserInt) (prepend 0 >> List.map decimal >> List.reduceBack (fun n acc -> acc / 10m + n))
        Combine (+) (IntParser |> map decimal) decParser

    let (>>%) (p:Parser<'a>) (x:'b) = Apply p (fun _ -> x)

    let WhiteSpaceParser = CharParser ' ' |> AtLeastOne |> map List.length

    let LetterParser = ['a'..'z'] |> List.map CharParserCI |> List.reduce Either

    let WordParser = AtLeastOne LetterParser |> map (fun xs -> new System.String(xs |> List.toArray))

    let rec ListParser (p:'a -> Parser<'a>) (cs: 'a list) : Parser<'a list> = 
        match cs with 
        | [] -> pSuccess []
        | h::t -> p h >>= (fun c -> ListParser p t |> map (fun t' -> c::t'))

    let CharListParser = ListParser CharParser
    let CharListParserCI = ListParser CharParserCI

    let StringParser (s:string) = Seq.toList s |> CharListParser |> map (Array.ofList >> System.String >> string)
    let StringParserCI (s:string) = Seq.toList s |> CharListParserCI |> map (Array.ofList >> System.String >> string)



open Parsers


StringParserCI "Hello" |> Parse "helloWorld"

let strToCharList (str:string) =  str.ToCharArray() |> Array.toList

//let parser = Parsers.IntParser .>> Parsers.WhiteSpaceParser .>>. WordParser

let parser = StringParser "-112"
let str = @"-1123 abc"

str.ToCharArray() |> Array.toList |> parser

