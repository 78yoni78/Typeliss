/// Defines inline functions and declarations for simplifying generic and monadic scode.
/// 
/// Functions use the notation: ^M is a type of ^a, ^N is a type of ^b, ^L is a type of ^c
module Helper


/// Returns the unit value of a type (or a monad).
///
/// Requires ^M: static member Unit: ^a -> ^M where ^M is the type.
let inline unit< ^a, ^M when ^M: (static member Unit: ^a -> ^M)> x = (^M: (static member Unit: ^a -> ^M) (x))

/// Binds a monad to a function.
///
/// Requires ^M: member Bind: (^a -> ^N) -> ^N) where ^M is the monad, ^a is the inner type and ^N is the result monad.
let inline bind f x = (^M: (member Bind: _ -> _) (x, f))

/// Maps a monad to a mapping
let inline map (f: ^a -> ^b) (x: ^M): ^N = bind (f >> unit) x

/// ^MM is a type of ^M
let inline join (x: ^MM): ^M = x >>= id

let inline (>>=) (x: ^M) (f: ^a -> ^N): ^N = bind f x
let inline (|>>) (x: ^M) (f: ^a -> ^b): ^N = map f x


/// Returns the zero value of a type (or a monad).
///
/// Requires static member Zero: unit -> ^M where ^M is the type.
let inline zero< ^M when ^M: (static member Zero: unit -> ^M)> =
    (^M: (static member Zero: unit -> ^M) ())

/// Returns the zero value of a type (or a monad).
///
/// Requires static member Zero: unit -> ^M where ^M is the type.
let inline one< ^M when ^M: (static member One: unit -> ^M)> = (^M: (static member One: unit -> ^M) ())


let inline choose (f: ^a -> ^b option) (x: ^M): ^N =
    let binder a =
        match f a with
        | Some b -> unit b
        | None -> zero

    x >>= binder

let inline where (predicate: ^a -> bool) (x: ^M): ^M =
    let f (x) = if predicate x then Some x else None
    choose f x


let inline cast< ^a, ^b when (^a or ^b): (static member Cast: ^a -> ^b)> x =
    ((^a or ^b): (static member Cast: ^a -> ^b) (x))


let inline combine (x: ^M) (y: ^N): ^L = (^M: (member Combine: ^N -> ^L) (x, y))
let inline (>~>) (x: ^M) (y: ^N): ^L = combine x y


type Builder() =
    member inline t.Zero(): ^M option = None
    member inline t.ReturnFrom x: ^M option = Some x
    member inline t.Return(x: ^a): ^M option = unit x |> t.ReturnFrom
    member inline t.Delay(f: unit -> ^M option) = f

    member inline t.Run(f: unit -> ^M option) =
        Option.defaultWith (fun () -> zero) (f ())

    member inline t.Combine(first: ^M option, secondDelayed: unit -> ^M option) =
        match first with
        | Some _ -> first
        | None -> secondDelayed ()

    member inline t.Bind(x: ^M, f: ^a -> _): ^N = x >>= f

    member inline t.While(cond: unit -> bool, body): ^M option =
        while cond () do
            body () |> ignore
        t.Zero()

let monad = Builder()
