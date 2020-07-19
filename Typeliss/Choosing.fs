/// A choosing is a constraint on an object. Like a ('a -> 'b option).
///
/// A choosing is a monad and can be used with monad functions from module Helper
module Choosing

/// A choosing is a constraint on an object. Like a ('a -> 'b option).
///
/// A choosing is a monad and can be used with monad functions from module Helper
type Choosing<'a, 'b> =
    | FuncChoosing of string * ('a -> 'b option)
    | NothingChoosing
    | AnythingChoosing of 'b


let inline choosing name func = FuncChoosing(name, func)


let inline name x =
    match x with
    | NothingChoosing -> "nothing"
    | AnythingChoosing x -> sprintf "anything return %O" x
    | FuncChoosing (name, _) -> name

let inline func x =
    match x with
    | NothingChoosing -> (fun _ -> None)
    | AnythingChoosing x -> (fun _ -> Some x)
    | FuncChoosing (_, func) -> func

let inline (|Choosing|) x = name x, func x


let anything<'a> : Choosing<'a, 'a> = choosing "anything" Some
let nothing<'a, 'b> : Choosing<'a, 'b> = NothingChoosing
let inline anythingReturns x = AnythingChoosing x


let inline mapName f (Choosing (name, func)) = choosing (f name) func

let inline nameAs name (Choosing (_, func)) = choosing name func


let inline many (Choosing (name, func): Choosing<'a, 'b>): Choosing<'a list, 'b list> =
    let rec visit acc =
        function
        | [] -> Some(List.rev acc)
        | x :: rest ->
            match func x with
            | Some x -> visit (x :: acc) rest
            | None -> None

    choosing name (visit [])

let inline union (Choosing (name1, func1): Choosing<'a, 'b>) (Choosing (name2, func2): Choosing<'a, 'b>) =
    choosing
    <| sprintf "%s or %s" name1 name2
    <| fun value -> Option.orElseWith (fun () -> func2 value) (func1 value)

let inline intersect (Choosing (name1, func1): Choosing<'a, 'b>) (Choosing (name2, func2): Choosing<'a, 'c>) =
    choosing
    <| sprintf "%s and %s" name1 name2
    <| fun value -> Option.bind (fun x -> Option.map (fun y -> x, y) (func2 value)) (func1 value)

let inline not (Choosing (name, func)) =
    let optionNot =
        function
        | Some _ -> None
        | None -> Some()

    choosing
    <| sprintf "not %s" name
    <| (func >> optionNot)


type Choosing<'a, 'b> with
    static member inline Zero(): Choosing<'a, 'b> = nothing
    static member inline Unit x: Choosing<'a, 'b> = anythingReturns x
    static member inline One(): Choosing<'a, 'a> = anything
    
    member inline x.Combine (y: Choosing<'b, 'c>): Choosing<'a, 'c> =
        match x, y with
        | NothingChoosing, _
        | _, NothingChoosing -> NothingChoosing
        | AnythingChoosing _, AnythingChoosing a -> AnythingChoosing a
        | Choosing (name1, func1), Choosing (name2, func2) ->
            choosing (sprintf "%s %s" name1 name2) (func1 >> Option.bind func2)

    member inline t.Bind(f: _ -> Choosing<'a, 'c>): Choosing<'a, 'c> =
        let (Choosing (name1, func1): Choosing<'a, 'b>) = t
        choosing name1 (fun inp ->
            func1 inp
            |> Option.bind (fun hid ->
                let (Choosing (_, func2)) = f hid
                func2 inp))
