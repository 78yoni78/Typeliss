[<AutoOpen>]
module Typeliss.Main

open Typeliss
open Choosing
open Helper


/// Gets a set of names that would return Undefined error when evaluated
let rec freeVariables =
    function
    | Const _ -> Set.empty
    | Var name -> set [ name ]
    | Apply (e1, e2) -> Set.union (freeVariables e1) (freeVariables e2)
    | LambdaExpr (name, body) ->
        freeVariables body
        |> Set.remove name
        |> Set.remove recName
    | TupleExpr exprs -> Seq.map freeVariables exprs |> Set.unionMany

/// A value that can be applied
and (|Applicable|_|): Value -> (Choosing<Value, unit -> Value>) option =
    function
    | Function f -> Some f
    | Lambda (name, bound, expr) as v ->
        let f value () =
            eval { Vars = bound |> Map.add recName v |> Map.add name value } expr

        anything |>> f |> Some
    | WrapperCons name ->
        anything
        |>> fun value () -> Wrapper(name, value)
        |> Some
    | NumberValue n1 ->
        choosing "number" (function
            | NumberValue n2 -> Some(fun () -> NumberValue(n1 * n2))
            | _ -> None)
        |> Some
    | _ -> None

and apply (x: Value) (y: Value): Value =
    match x, y with
    | NumberValue n1, NumberValue n2 -> n1 * n2 |> NumberValue  //  This solves an ambiguity
    | Applicable f1, Applicable f2 ->
        match func f1 y, func f2 x with
        | Some _, Some _ -> ErrorValue(AmbiguousApplication(x, y))
        | Some f, None
        | None, Some f -> f ()
        | None, None -> ErrorValue(TypeError([ x; y ], "Cannot be applied to eachother"))
    | Applicable f, v
    | v, Applicable f ->
        match func f v with
        | Some f -> f ()
        | None -> ErrorValue(TypeError([ x; y ], sprintf "function expected a %s" (name f)))
    | x, y -> ErrorValue(TypeError([ x; y ], "Both cannot be applied to anything"))

and eval (context: EvalContext): Expr -> Value =
    function
    | Const value -> value
    | LambdaExpr (name, body) as expr ->
        let boundVariables =
            freeVariables expr
            |> Seq.choose (fun name -> Option.map (fun value -> name, value) (context.Vars.TryFind name))
            |> Map.ofSeq

        Lambda(name, boundVariables, body)
    | Apply (e1, e2) -> apply (eval context e1) (eval context e2)
    | Var name ->
        context.Vars.TryFind name
        |> Option.defaultValue (ErrorValue(UndefinedError name))
    | TupleExpr exprs ->
        Seq.map (eval context) exprs
        |> List.ofSeq
        |> Tuple
