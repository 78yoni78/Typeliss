/// Contains helpers for creating libraries and contains the standard library
module Typeliss.Lib

open Number
open Exporting
open Helper


/// A type constraint is an f# object describing a set of Typeliss values.
/// 
/// A value can be bound by a type constraint in 2 ways in the value workflow:
/// 
/// value { let! x = typeConstraint, someValue in ... } (Taken immidiatly)
/// 
/// value { let! x = typeConstraint in ... } (Taken as a parameter to a function)

/// A choosing is describes a constraint on an object. Like a ('a -> 'b option).
/// 
/// A choosing is a monad and can be used 
type Choosing<'a, 'b> = 
    | Choosing of string * ('a -> 'b option)
    static member inline Zero(): Choosing<'a, 'b> = Choosing("nothing", fun _ -> None)
    static member inline Unit (x: 'b): Choosing<'a, 'b> = Choosing("anything", fun _ -> Some x)
    member inline t.Bind (f: _ -> Choosing<'a, 'c>): Choosing<'a, 'c> =
        let (Choosing (name1, func1): Choosing<'a, 'b>) = t
        Choosing (name1, fun inp -> 
            func1 inp |> Option.bind (fun hid -> 
                let (Choosing (_, func2)) = f hid
                func2 inp))
        
    static member inline Compose (Choosing (name1, func1): Choosing<'a, 'b>, Choosing (name2, func2)) =
        Choosing (sprintf "%s %s" name2 name1, func1 >> Option.bind func2)
    static member inline (>=>) (x, y) = Choosing.Compose (x, y)
    // static member inline (|>>) (Choosing (name, func): Choosing<'a,'b>, (nameFormat, f: 'b -> 'c)) = 
    //     Choosing (nameFormat name, func >> Option.map f)
    // static member inline (|>>) (Choosing (name, func): Choosing<'a,'b>, f: 'b -> 'c) = 
    //     Choosing (name, func >> Option.map f)

module Choosing =

    let inline func (Choosing (_, func)) x = func x
    let inline name (Choosing (name, _)) = name
    let inline mapName f (Choosing (name, func))  = Choosing (f name, func)

    let inline chooseAll (Choosing (name, func): Choosing<'a, 'b>): Choosing<'a list, 'b list> =
        let rec visit acc = function
            | [] -> Some (List.rev acc)
            | x :: rest -> 
                match func x with
                | Some x -> visit (x :: acc) rest
                | None -> None
        Choosing (name, visit [])

    let inline union (Choosing (name1, func1): Choosing<'a, 'b>) (Choosing (name2, func2): Choosing<'a, 'b>) = 
        Choosing 
            (sprintf "%s or %s" name1 name2,
             fun value -> Option.orElseWith (fun () -> func2 value) (func1 value))
    
    let inline intersect (Choosing (name1, func1): Choosing<'a, 'b>) (Choosing (name2, func2): Choosing<'a, 'c>) =  
        Choosing 
            (sprintf "%s and %s" name1 name2,
             fun value -> Option.bind (fun x -> Option.map (fun y -> x, y) (func2 value)) (func1 value))

    let inline not (Choosing (name, func)) = 
        Choosing 
            (sprintf "not %s" name,
             fun value -> match func value with Some _ -> None | None -> Some ())


type VBInternal = VBInternal of Value option
type VBDelayed = VBDelayed of (unit -> VBInternal)

type ValueBuilder() =
    member inline _.Zero () = VBInternal None
    member inline _.ReturnFrom x = VBInternal (Some x)
    member inline t.Return() = Mono |> t.ReturnFrom
    member inline t.Return x = NumberValue x |> t.ReturnFrom
    member inline t.Return x = StringValue x |> t.ReturnFrom
    member inline t.Return x = ErrorValue x |> t.ReturnFrom
    member inline t.Return x = Function x |> t.ReturnFrom
    member inline t.Return x = Tuple x |> t.ReturnFrom
    member inline t.Return x = (match x with Some x -> Wrapper("some", x) | None -> Wrapper("none", Mono)) |> t.ReturnFrom
    member inline t.Return x = (match x with Some () -> Wrapper("some", Mono) | None -> Wrapper("none", Mono)) |> t.ReturnFrom
    member inline _.Delay (f: unit -> VBInternal) = VBDelayed f
    member inline _.Run (VBDelayed f): Value = 
        let (VBInternal opt) = f()
        Option.defaultValue Mono opt 

    member _.Combine (first: VBInternal, VBDelayed second): VBInternal =
        match first with
        | VBInternal (Some _) -> first
        | VBInternal None -> second()

    member t.Bind ((Choosing (name, func), value: Value), f: 'a -> VBInternal) = 
        match func value with
        | Some a -> f a
        | None -> t.ReturnFrom (ErrorValue (TypeError([value], sprintf "Argument must be %s" name)))
    member t.Bind (cons: Choosing<Value, 'a>, f: 'a -> VBInternal) =
        Function (fun value -> (fun() -> t.Bind((cons, value), f)) |> t.Delay |> t.Run)
        |> t.ReturnFrom

    member t.While (cond: unit -> bool, VBDelayed body) =
        while cond() do
            body() 
            |> ignore
        t.Zero()
let value = ValueBuilder()


open Choosing

let anyVal: Choosing<Value, Value> = Choosing ("anything", Some)
let noVal: Choosing<Value, unit> = Choosing ("nothing", fun _ -> None)

let monoVal: Choosing<Value, unit> = Choosing ("()", function Mono -> Some () | _ -> None) 

let numberVal = Choosing ("number", function NumberValue n -> Some n | _ -> None)

let stringVal = Choosing ("string", function StringValue str -> Some str | _ -> None) 

let iterableVal = Choosing ("iterable", function 
    | Tuple list -> Some (list :> Value seq)
    | StringValue str -> Some (str |> Seq.map (string >> StringValue))
    | _ -> None)

let iterableOf inner = 
    iterableVal |>> List.ofSeq >=> chooseAll inner

let tupleVal = Choosing ("tuple", function 
    | Tuple values -> Some values
    | _ -> None)

let tupleOf inner = 
    tupleVal >=> chooseAll inner

let even (inner: Choosing<_,_>) = 
    let tryPairs lst = 
        if List.length lst % 2 = 1 then None else
            lst
            |> List.chunkBySize 2
            |> List.map (function [x; y] -> x, y | _ -> failwithf "Bad chunk at even Choosing!")
            |> Some
    choose tryPairs inner

let applicableVal = Choosing ("applicable", function
    | Applicable f -> Some f
    | _ -> None)

let errorVal = Choosing ("error", function
    | ErrorValue e -> Some e
    | _ -> None)

let undefinedErrorVal = Choosing ("undefined error", function
    | ErrorValue (UndefinedError name) -> Some name
    | _ -> None)

let wrapperVal = Choosing ("wrapper", function
    | Wrapper (name, value) -> Some (name, value)
    | _ -> None)
let namedWrapperVal name = Choosing (name, function
    | Wrapper (name', value) when name = name' -> Some value
    | _ -> None)
    
let someVal = namedWrapperVal "some"
let noneVal = namedWrapperVal "none" |>> ignore
let optVal = union (someVal |>> Some) (noneVal |>> fun () -> None)


type ReflectionHelper() = inherit ReflectionHelperBase()


let addableVal = Choosing ("addable", function
    | NumberValue n -> Some (Choice1Of2 n)
    | StringValue s -> Some (Choice2Of2 s)
    | _ -> None)
let [<Export("+")>] vadd = value { 
    let! x = addableVal
    match x with
    | Choice1Of2 i1 ->
        let! i2 = numberVal
        return i1 + i2
    | Choice2Of2 s1 ->
        let! s2 = stringVal
        return s1 + s2
}

let inline numberBinaryOperator (f: Number->Number->Number) = 
    value { let! n1 = numberVal in let! n2 = numberVal in return f n1 n2 }
let [<Export("-")>] vsub = numberBinaryOperator(-)
let [<Export("*")>] vmul = numberBinaryOperator( * )
let [<Export("/")>] vdiv = numberBinaryOperator(/)
let [<Export("pow")>] vpow = numberBinaryOperator( ** )
let [<Export>] pi = value { return Number.FromFloat System.Math.PI }
let [<Export>] e = value { return Number.FromFloat System.Math.E }

let [<Export>] ``for`` = value { 
    let! n = numberVal in let! init = anyVal in let! f = applicableVal in
    return! Seq.fold (fun s () -> f s) init (Seq.replicate (int n.BigInt) ())
}
let [<Export>] ``do`` = (let rec f v = (apply v Mono |> ignore; Function f) in Function f)
let [<Export>] ``;`` = Function (fun _ -> Function id)
let mappableVal = Choosing ("mappable", function
    | Tuple lst -> Some (fun f -> List.map f lst |> Tuple)
    | StringValue str -> Some (fun f -> String.collect (string >> StringValue >> f >> string) str |> StringValue)
    | _ -> None)
let [<Export("map")>] vmap = value {
    let! mapping = applicableVal in let! mapFunc = mappableVal
    return! mapFunc mapping
}
let [<Export>] fold = value {
    let! state = anyVal in let! stater = applicableVal in let! sequence = iterableVal
    return! Seq.fold (fun s t -> apply (stater s) t) state sequence
}
let [<Export("|>")>] vpipeLeft = value { let! v = anyVal in let! f = applicableVal in return! f v}

let [<Export("=")>] vequal = value { let! rhs = anyVal in let! lhs = anyVal in return if rhs = lhs then Some Mono else None}
let [<Export("!=")>] vnequal = value { let! rhs = anyVal in let! lhs = anyVal in return if rhs <> lhs then Some Mono else None}

// let [<Export>] ``,`` = Function (fun v1 -> Function(fun v2 ->  TupleCons(v1, v2)))
// let [<Export>] ``.`` = Function (fun v -> 
//         let rec getList = function
//             | TupleCons (vStart, vEnd) ->
//                 getList vStart @ [vEnd]
//             | v -> [v]
//         Tuple (getList v))

let [<Export("id")>] vid = Function id
let [<Export>] ``always`` = Function id

let patternVal = intersect (namedWrapperVal "pattern") applicableVal |>> snd 
let isPattern (predicate: Value -> bool) = Wrapper("pattern", value { let! x = anyVal in return! value {return if predicate x then Some x else None}})
let isChar (chars: char seq) = isPattern (function StringValue str when str.Length = 1 && Seq.contains str.[0] chars -> true | _ -> false)

let [<Export>] ``is-mono`` = isPattern (function Mono -> true | _ -> false)
let [<Export>] ``is-number`` = isPattern (function NumberValue _ -> true | _ -> false)
let [<Export>] ``is-string`` = isPattern (function StringValue _ -> true | _ -> false)
let [<Export>] ``is-tuple`` = isPattern (function Tuple _ -> true | _ -> false)
let [<Export>] ``is-applicable`` = isPattern (function Applicable _ -> true | _ -> false)
let [<Export>] ``is-digit`` = isChar ['0'..'9']
let [<Export>] ``is-letter`` = isChar ['a'..'z']
let [<Export>] ``is-hletter`` = isChar (['א'..'ת'] @ ['ם'; 'ף'; 'ץ'; 'ך'; 'ן'])
let optOrPatternParam = union (optVal |>> Choice1Of2) (patternVal |>> Choice2Of2)
let [<Export>] ``or`` = value { 
    let! x = optOrPatternParam 
    let! y = optOrPatternParam  
    match x, y with
    | Choice1Of2 x, Choice1Of2 y -> return x |> Option.orElse y
    | Choice1Of2 x, Choice2Of2 y -> return! value { let! arg = anyVal in return! x |> Option.defaultWith (fun () -> y arg) }
    | Choice2Of2 x, Choice1Of2 y -> return! value { let! arg = anyVal in let! x = optVal, x arg in return x |> Option.orElse y }
    | Choice2Of2 x, Choice2Of2 y -> return! value { let! arg = anyVal in let! x = optVal, x arg in return! x |> Option.defaultWith (fun () -> y arg) }
}

let [<Export>] ``match`` = value {
    let! arg = anyVal
    let! cases = even (tupleOf patternVal)
    return! Seq.tryPick (fun (pattern, body) -> match pattern arg with Wrapper ("none", Mono) -> None | x -> Some (x, body)) cases
            |> function
            | Some (Wrapper ("some", value), body) 
            | Some (value, body) ->
                body value
            | None ->
                ErrorValue (NoMatch arg)
}

let [<Export>] ``if`` = value { 
    let! opt = optVal 
    let! trueBody = applicableVal
    let! falseBody = applicableVal 
    return! match opt with Some x -> trueBody x | None -> falseBody Mono
}

let alternativeNames = Map.ofList [
    "pow", ["חזקה"; "בחזקת"; "לכח"; "כח"; "מסדר"]
    "pi", ["פאי"; "π"; "עוגה"]
    "e", ["אי"]
    "for", ["לכל"; "loop"; "לופ"; "לולאה"]
    "do", ["עשה"]
    "map", ["מאפ"; "מפה"; "מיפוי"; "למפות"]
    "fold", ["קיפול"; "קפל"; "לקפל"; "העבר"] 
]

let addAlternatives vars alternativeNames = 
    let addNames map name otherNames = List.fold (fun map name' -> Map.add name' (map.[name]) map) map otherNames
    Map.fold addNames vars alternativeNames

let makeVars (args: obj seq) (modu: Module) = 
    let args = Array.ofSeq args
    Seq.map (fun (Exported (name, _, _, getter)) -> name, getter args :?> Value) (exported modu)
    |> Map.ofSeq

let startCon = { Vars = addAlternatives (makeVars [] getModule<ReflectionHelper>) alternativeNames }
