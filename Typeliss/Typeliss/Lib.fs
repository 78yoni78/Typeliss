/// Contains helpers for creating libraries and contains the standard library
module Typeliss.Lib

open Number
open Exporting
open Helper
open Choosing


/// A type constraint is an f# object describing a set of Typeliss values.
///
/// A value can be bound by a type constraint in 2 ways in the value workflow:
///
/// value { let! x = typeConstraint, someValue in ... } (Taken immidiatly)
///
/// value { let! x = typeConstraint in ... } (Taken as a parameter to a function)

type VBInternal = VBInternal of Value option
type VBDelayed = VBDelayed of (unit -> VBInternal)

type ValueBuilder() =
    member inline _.Zero() = VBInternal None
    member inline _.ReturnFrom x = VBInternal(Some x)
    member inline t.Return() = Mono |> t.ReturnFrom
    member inline t.Return x = NumberValue x |> t.ReturnFrom
    member inline t.Return x = StringValue x |> t.ReturnFrom
    member inline t.Return x = ErrorValue x |> t.ReturnFrom
    member inline t.Return x = Function x |> t.ReturnFrom
    member inline t.Return x = Tuple x |> t.ReturnFrom

    member inline t.Return x =
        (match x with
         | Some x -> Wrapper("some", x)
         | None -> Wrapper("none", Mono))
        |> t.ReturnFrom

    member inline t.Return x =
        (match x with
         | Some () -> Wrapper("some", Mono)
         | None -> Wrapper("none", Mono))
        |> t.ReturnFrom

    member inline _.Delay(f: unit -> VBInternal) = VBDelayed f

    member inline _.Run(VBDelayed f): Value =
        let (VBInternal opt) = f ()
        Option.defaultValue Mono opt

    member inline _.Combine(first: VBInternal, VBDelayed second): VBInternal =
        match first with
        | VBInternal (Some _) -> first
        | VBInternal None -> second ()

    member inline t.Bind((Choosing(name, func), value: Value), f: 'a -> VBInternal) =
        match func value with
        | Some a -> f a
        | None -> t.ReturnFrom(ErrorValue(TypeError([ value ], sprintf "Argument must be %s" name)))

    member inline t.Bind(cons: Choosing<Value, 'a>, f: 'a -> VBInternal): VBInternal =
        let funcBody arg () = (fun () -> f arg) |> t.Delay |> t.Run

        Function(cons |>> funcBody) |> t.ReturnFrom

    member inline t.While(cond: unit -> bool, VBDelayed body) =
        while cond () do
            body () |> ignore
        t.Zero()

let value = ValueBuilder()


let anyVal: Choosing<Value, Value> = anything
let noVal: Choosing<Value, unit> = nothing

let monoVal: Choosing<Value, unit> =
    choosing "()" (function
        | Mono -> Some()
        | _ -> None)

let numberVal =
    choosing "number" (function
        | NumberValue n -> Some n
        | _ -> None)

let stringVal =
    choosing "string" (function
        | StringValue str -> Some str
        | _ -> None)

let iterableVal =
    choosing ("iterable") (function
        | Tuple list -> Some(list :> Value seq)
        | StringValue str -> Some(str |> Seq.map (string >> StringValue))
        | _ -> None)

let iterableOf inner =
    iterableVal |>> List.ofSeq >~> many inner

let tupleVal =
    choosing "tuple" (function
        | Tuple values -> Some values
        | _ -> None)

let tupleOf inner = tupleVal >~> many inner

let even (inner: Choosing<_, _>) =
    let tryPairs lst =
        if List.length lst % 2 = 1 then
            None
        else
            lst
            |> List.chunkBySize 2
            |> List.map (function
                | [ x; y ] -> x, y
                | _ -> failwithf "Bad chunk at even Choosing!")
            |> Some

    choose tryPairs inner

let applicableVal =
    choosing ("applicable") (function
        | Applicable f -> Some f
        | _ -> None)

let errorVal =
    choosing ("error") (function
        | ErrorValue e -> Some e
        | _ -> None)

let undefinedErrorVal =
    choosing ("undefined error") (function
        | ErrorValue (UndefinedError name) -> Some name
        | _ -> None)

let wrapperVal =
    choosing ("wrapper") (function
        | Wrapper (name, value) -> Some(name, value)
        | _ -> None)

let namedWrapperVal name =
    choosing (name) (function
        | Wrapper (name', value) when name = name' -> Some value
        | _ -> None)

let someVal = namedWrapperVal "some"
let noneVal = namedWrapperVal "none" |>> ignore

let optVal =
    union
        (someVal |>> Some)
        (noneVal
         |>> fun () -> None)


type ReflectionHelper() =
    inherit ReflectionHelperBase()


let addableVal =
    choosing "addable" (function
        | NumberValue n -> Some(Choice1Of2 n)
        | StringValue s -> Some(Choice2Of2 s)
        | _ -> None)

[<Export("+")>]
let vadd =
    value {
        let! x = addableVal

        match x with
        | Choice1Of2 i1 ->
            let! i2 = numberVal
            return i1 + i2
        | Choice2Of2 s1 ->
            let! s2 = stringVal
            return s1 + s2
    }

let inline numberBinaryOperator (f: Number -> Number -> Number) =
    value {
        let! n1 = numberVal
        let! n2 = numberVal
        return f n1 n2
    }

[<Export("-")>]
let vsub = numberBinaryOperator (-)

[<Export("*")>]
let vmul = numberBinaryOperator (*)

[<Export("/")>]
let vdiv = numberBinaryOperator (/)

[<Export("pow")>]
let vpow = numberBinaryOperator (**)

[<Export>]
let pi =
    value { return Number.FromFloat System.Math.PI }

[<Export>]
let e =
    value { return Number.FromFloat System.Math.E }

//[<Export>]
//let ``for`` =
//    value {
//        let! n = numberVal
//        let! init = anyVal
//        let! f = applicableVal
//        return! Seq.fold (fun s () -> f s) init (Seq.replicate (int n.BigInt) ())
//    }

//[<Export>]
//let ``do`` =
//    (
//     let rec f v =
//         (apply v Mono |> ignore
//          Function f)
//      in Function f)

//[<Export>]
//let ``;`` = Function(fun _ -> Function id)

//let mappableVal =
//    Choosing
//        ("mappable",
//         function
//         | Tuple lst -> Some(fun f -> List.map f lst |> Tuple)
//         | StringValue str ->
//             Some(fun f ->
//                 String.collect (string >> StringValue >> f >> string) str
//                 |> StringValue)
//         | _ -> None)

//[<Export("map")>]
//let vmap =
//    value {
//        let! mapping = applicableVal
//        let! mapFunc = mappableVal
//        return! mapFunc mapping
//    }

//[<Export>]
//let fold =
//    value {
//        let! state = anyVal
//        let! stater = applicableVal
//        let! sequence = iterableVal
//        return! Seq.fold (fun s t -> apply (stater s) t) state sequence
//    }

//[<Export("|>")>]
//let vpipeLeft =
//    value {
//        let! v = anyVal
//        let! f = applicableVal
//        return! f v
//    }

//[<Export("=")>]
//let vequal =
//    value {
//        let! rhs = anyVal
//        let! lhs = anyVal
//        return if rhs = lhs then Some Mono else None
//    }

//[<Export("!=")>]
//let vnequal =
//    value {
//        let! rhs = anyVal
//        let! lhs = anyVal
//        return if rhs <> lhs then Some Mono else None
//    }

//// let [<Export>] ``,`` = Function (fun v1 -> Function(fun v2 ->  TupleCons(v1, v2)))
//// let [<Export>] ``.`` = Function (fun v ->
////         let rec getList = function
////             | TupleCons (vStart, vEnd) ->
////                 getList vStart @ [vEnd]
////             | v -> [v]
////         Tuple (getList v))

//[<Export("id")>]
//let vid = Function id

//[<Export>]
//let always = Function id

//let patternVal =
//    intersect (namedWrapperVal "pattern") applicableVal
//    |>> snd

//let isPattern (predicate: Value -> bool) =
//    Wrapper
//        ("pattern",
//         value {
//             let! x = anyVal
//             return! value { return if predicate x then Some x else None }
//         })

//let isChar (chars: char seq) =
//    isPattern (function
//        | StringValue str when str.Length = 1 && Seq.contains str.[0] chars -> true
//        | _ -> false)

//[<Export>]
//let ``is-mono`` =
//    isPattern (function
//        | Mono -> true
//        | _ -> false)

//[<Export>]
//let ``is-number`` =
//    isPattern (function
//        | NumberValue _ -> true
//        | _ -> false)

//[<Export>]
//let ``is-string`` =
//    isPattern (function
//        | StringValue _ -> true
//        | _ -> false)

//[<Export>]
//let ``is-tuple`` =
//    isPattern (function
//        | Tuple _ -> true
//        | _ -> false)

//[<Export>]
//let ``is-applicable`` =
//    isPattern (function
//        | Applicable _ -> true
//        | _ -> false)

//[<Export>]
//let ``is-digit`` = isChar [ '0' .. '9' ]

//[<Export>]
//let ``is-letter`` = isChar [ 'a' .. 'z' ]

//[<Export>]
//let ``is-hletter`` =
//    isChar
//        (@ [ 'א' .. 'ת' ] [
//            'ם'
//            'ף'
//            'ץ'
//            'ך'
//            'ן'
//         ])

//let optOrPatternParam =
//    union (optVal |>> Choice1Of2) (patternVal |>> Choice2Of2)

//[<Export>]
//let ``or`` =
//    value {
//        let! x = optOrPatternParam
//        let! y = optOrPatternParam

//        match x, y with
//        | Choice1Of2 x, Choice1Of2 y -> return x |> Option.orElse y
//        | Choice1Of2 x, Choice2Of2 y ->
//            return! value {
//                        let! arg = anyVal
//                        return! x |> Option.defaultWith (fun () -> y arg)
//                    }
//        | Choice2Of2 x, Choice1Of2 y ->
//            return! value {
//                        let! arg = anyVal
//                        let! x = optVal, x arg
//                        return x |> Option.orElse y
//                    }
//        | Choice2Of2 x, Choice2Of2 y ->
//            return! value {
//                        let! arg = anyVal
//                        let! x = optVal, x arg
//                        return! x |> Option.defaultWith (fun () -> y arg)
//                    }
//    }

//[<Export>]
//let ``match`` =
//    value {
//        let! arg = anyVal
//        let! cases = even (tupleOf patternVal)

//        return! Seq.tryPick (fun (pattern, body) ->
//                    match pattern arg with
//                    | Wrapper ("none", Mono) -> None
//                    | x -> Some(x, body)) cases
//                |> function
//                | Some (Wrapper ("some", value), body)
//                | Some (value, body) -> body value
//                | None -> ErrorValue(NoMatch arg)
//    }

//[<Export>]
//let ``if`` =
//    value {
//        let! opt = optVal
//        let! trueBody = applicableVal
//        let! falseBody = applicableVal

//        return! match opt with
//                | Some x -> trueBody x
//                | None -> falseBody Mono
//    }

let alternativeNames =
    Map.ofList [ "pow", [ "חזקה"; "בחזקת"; "לכח"; "כח"; "מסדר" ]
                 "pi", [ "פאי"; "π"; "עוגה" ]
                 "e", [ "אי" ]
                 "for", [ "לכל"; "loop"; "לופ"; "לולאה" ]
                 "do", [ "עשה" ]
                 "map", [ "מאפ"; "מפה"; "מיפוי"; "למפות" ]
                 "fold", [ "קיפול"; "קפל"; "לקפל"; "העבר" ] ]

let addAlternatives vars alternativeNames =
    let addNames map name otherNames =
        List.fold (fun map name' -> Map.add name' (map.[name]) map) map otherNames

    Map.fold addNames vars alternativeNames

let makeVars (args: obj seq) (modu: Module) =
    let args = Array.ofSeq args
    Seq.map (fun (Exported (name, _, _, getter)) -> name, getter args :?> Value) (exported modu)
    |> Map.ofSeq

let startCon =
    { Vars = addAlternatives (makeVars [] getModule<ReflectionHelper>) alternativeNames }
