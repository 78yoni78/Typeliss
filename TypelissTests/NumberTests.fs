module NumberTests

open Number
open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Cast`` () =
    number 12 |> should equal (Number (12I, 1I))
    number 12.0 |> should equal (Number (12I, 1I))
    number 12I |> should equal (Number (12I, 1I))

[<Theory>]
[<InlineData(3.3, 2.7, -0.7, -0.6, 0.876, 0.0)>]
[<InlineData(-3.8, 2.8, 3.3, -0.1, 362, -0.13)>]
let ``y = a!/(x^b + c) / d`` (a: float, b: float, c: float, d: float, x: float, y: float) =
    let precision = 0.001
    let a, b, c, d, x = number a, number b, number c, number d, number x
    abs (((x ** b + c) ** (one / a) / d).Float - y) |> should lessThan precision

