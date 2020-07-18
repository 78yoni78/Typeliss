module Typeliss.Parsing

open FParsec
open Typeliss
open Number

let pname = 
    let pend = choice [
        anyOf ['('; ')'; ':'; '['; ']'; '{'; '}'(*; '.'; ','; '"'; '\''; ';'*)] |>> ignore
        spaces1
        eof
    ]
    many1Chars (notFollowedBy pend >>. anyChar) .>> spaces

let pvalue = 
    choiceL [
        stringReturn "()" Mono
        pchar '"' >>. manyCharsTill anyChar (pchar '"') |>> StringValue
        pchar '\'' >>. manyCharsTill anyChar (pchar '\'') |>> StringValue
        pfloat |>> (NumberValue << Number.FromFloat)
        puint64 |>> fun i -> NumberValue (Number(bigint i, 1I))
    ] "value" .>> spaces

let (pexpr: Parser<Expr,unit>), piexpr = createParserForwardedToRef ()
let (patom: Parser<Expr,unit>), piatom = createParserForwardedToRef ()

do piatom := 
    choice [
        pvalue |>> Const
        pchar '(' >>. spaces >>. pexpr .>> pchar ')'
        pchar '[' >>. spaces >>. many patom .>> pchar ']' |>> TupleExpr
        pchar '{' >>. spaces >>. pexpr .>> pchar '}' |>> fun x -> LambdaExpr ("_", x)
        pname .>>. choice [stringReturn ":" (WrapperCons >> Const); preturn Var] 
        |>> fun (name, func) -> func name 
    ] .>> spaces

do piexpr :=
    choiceL [
        pname .>>? skipString "->" .>> spaces .>>. pexpr |>> LambdaExpr
        many1 patom
        |>> function
            | [atom] -> atom 
            | atoms -> List.reduce (fun e1 e2 -> Apply(e1, e2)) atoms
    ] "expression" .>> spaces

let tree (str: string) = 
    run (spaces >>. pexpr .>> eof) str
    |> function
        | Success (expr, _, _) -> Result.Ok expr
        | Failure (err, _, _) -> Result.Error err
