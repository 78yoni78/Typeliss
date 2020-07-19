module Typeliss.MainTests

open Xunit
open FsUnit.CustomMatchers
open FsUnit.Xunit
open Typeliss
open Number
open Choosing


let [<Fact>] ``TupleExpr freeVariables``() =
    let e = 
        TupleExpr [
            Var "x"
            TupleExpr [
                Const (NumberValue one)
                Apply (Var "y", TupleExpr [Var "x"; Var "z"])
            ]
        ]
    let free = freeVariables e
    free |> should equal (set ["x"; "y"; "z"])

let [<Fact>] ``Lambda freeVariables``() =
    let e = 
        LambdaExpr (
            "x", 
            Apply (
                Var "x", 
                TupleExpr [
                    LambdaExpr ("y", Const (NumberValue zero))
                    Var "y"
                ]
            )
        )
    let free = freeVariables e
    free |> should equal (set ["y"])

let [<Fact>] ``Apply function to number``() =
    let f = Function <| anythingReturns (fun _ -> Mono)
    let i = NumberValue (number 4)
    apply f i |> should be (ofCase <@ Mono @>)

let [<Fact>] ``Apply number to function``() =
    let f = Function <| anythingReturns (fun _ -> Mono)
    let i = NumberValue (number 4)
    apply i f |> should be (ofCase <@ Mono @>)

let [<Fact>] ``Apply number to number``() =
    let i1 = NumberValue (number 6)
    let i2 = NumberValue (number 4)
    apply i1 i2 |> should equal (NumberValue (number 24))

let [<Fact>] ``Apply function to function``() =
    let f1 = Function <| anythingReturns (fun _ -> Wrapper ("f1", Mono))
    let f2 = Function <| anythingReturns (fun _ -> Wrapper ("f2", Mono))
    apply f1 f2 |> should be (ofCase <@ ErrorValue @>)


//let eval (context: EvalContext) = function
//    | Const value -> value
//    | LambdaExpr (name, body) as expr ->
//        let boundVariables =
//            freeVariables expr
//            |> Seq.choose (fun name -> 
//                Option.map (fun value -> name, value) (context.Vars.TryFind name))
//            |> Map.ofSeq
//        Lambda (name, boundVariables, body)
//    | Apply (e1, e2) -> apply (eval context e1) (eval context e2)
//    | Var name -> context.Vars.TryFind name |> Option.defaultValue (ErrorValue (UndefinedError name))
//    | TupleExpr exprs -> Seq.map (eval context) exprs |> List.ofSeq |> Tuple
