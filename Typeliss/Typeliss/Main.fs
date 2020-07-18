[< AutoOpen >]
module Typeliss.Main

open Typeliss


/// Gets a set of names that would return Undefined error when evaluated
let rec freeVariables = function
    | Const _ -> Set.empty
    | Var name -> set [name]
    | Apply (e1, e2) -> Set.union (freeVariables e1) (freeVariables e2)
    | LambdaExpr (name, body) -> 
        freeVariables body
        |> Set.remove name
        |> Set.remove recName
    | TupleExpr exprs -> Seq.map freeVariables exprs |> Set.unionMany

/// A value that can be applied 
and (|Applicable|_|) = function
    | Function f -> Some f
    | Lambda (name, bound, expr) as v ->
        Some (fun value ->
            let vars' = bound |> Map.add recName v |> Map.add name value
            let context' = { Vars = vars' }
            eval context' expr)
    | WrapperCons name -> Some (fun value -> Wrapper (name, value))
    | _ -> None

and apply x y =
    match x, y with
    | Applicable f, v | v, Applicable f -> f v
    | NumberValue n1, NumberValue n2 -> NumberValue (n1 * n2) 
    | x, y -> ErrorValue (TypeError ([x; y], "Cannot be applied"))

and eval (context: EvalContext) = function
    | Const value -> value
    | LambdaExpr (name, body) as expr ->
        let boundVariables =
            freeVariables expr
            |> Seq.choose (fun name -> 
                Option.map (fun value -> name, value) (context.Vars.TryFind name))
            |> Map.ofSeq
        Lambda (name, boundVariables, body)
    | Apply (e1, e2) -> apply (eval context e1) (eval context e2)
    | Var name -> context.Vars.TryFind name |> Option.defaultValue (ErrorValue (UndefinedError name))
    | TupleExpr exprs -> Seq.map (eval context) exprs |> List.ofSeq |> Tuple
