[<AutoOpen>]
module Typeliss.Typeliss

open Number
open Choosing


/// An error is a reguler value in Typeliss
type Error = 
    | TypeError of Value list * string 
    | AmbiguousApplication of Value * Value
    | UndefinedError of string
    | NoMatch of Value
    override t.ToString() =
        match t with
        | TypeError (v, msg) -> 
            sprintf "TypeError: [%s] %s" (Seq.map string v |> String.concat ", ") msg
        | UndefinedError n -> sprintf "Undefined %s" n
        | NoMatch v -> sprintf "Value %O wasn't matched" v
        | AmbiguousApplication (x, y) -> sprintf "The expression (%O %O) is ambiguous! Use the |> operator" x y


and Vars = Map<string, Value> 
and EvalContext = { Vars: Vars } 


and [<CustomEquality; NoComparison>] Value = 
    | Mono
    | NumberValue of Number
    | StringValue of string
    | ErrorValue of Error
    | Function of Choosing<Value, unit -> Value>    //  A function takes a value and returns a continuation.
    | Lambda of arg: string * boundVariables: Vars * body: Expr
    | Wrapper of string * Value
    | WrapperCons of name: string
    | TupleCons of lhs: Value * rhs: Value
    | Tuple of Value list
    override t.ToString() =
        match t with
        | Mono -> "()"
        | NumberValue n -> string n
        | StringValue v -> v
        | ErrorValue e -> string e
        | Function _ -> sprintf "a function"
        | Lambda (x, map, e) -> if Map.isEmpty map then sprintf "(%s -> %O)" x e 
                                else sprintf "(%s%A -> %O)" x (Map.toSeq map) e
        | Wrapper (name, value) -> sprintf "(%s: %O)" name value
        | WrapperCons name -> sprintf "%s:" name
        | TupleCons (v1, v2) -> sprintf "(%O, %O)" v1 v2
        | Tuple vs -> sprintf "[%s]" (List.map (sprintf "(%O)") vs |> String.concat " ")
    override t.Equals t2 =
        match t2 with
        | :? Value as t2 ->
            match t, t2 with
            | Mono, Mono -> true
            | NumberValue x, NumberValue y -> x = y
            | StringValue x, StringValue y -> x = y
            | ErrorValue x, ErrorValue y -> x = y
            | Wrapper (x1, x2), Wrapper(y1, y2) -> x1 = y1 && x2 = y2
            | Tuple x, Tuple y -> x = y
            | _ -> false
        | _ -> false 
    override t.GetHashCode () = LanguagePrimitives.PhysicalHash t

and Expr =
    | Const of Value
    | LambdaExpr of string * Expr
    | Var of string
    | Apply of Expr * Expr
    | TupleExpr of Expr list
    override t.ToString() =
        match t with
        | Const value -> string value
        | Var x -> x
        | Apply (e1, e2) -> sprintf "(%O %O)" e1 e2
        | LambdaExpr (name, expr) -> sprintf "(%s -> %O)" name expr
        | TupleExpr exprs -> sprintf "[%s]" <| String.concat ", " (Seq.map string exprs)


let recName = "this"

let addVars vars (context: EvalContext) = 
    { context with Vars = Map.ofSeq (Seq.append (Map.toSeq context.Vars) (Map.toSeq vars)) } 
