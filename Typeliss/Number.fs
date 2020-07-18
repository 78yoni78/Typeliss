/// Module contains the 'Number' struct which is a type 
/// repressenting an unlimited fraction.
/// use 'number' to make numbers and 'float' to convertq
/// them back to standard float
module Number

open System
open Helper


type [<CustomComparison; CustomEquality; Struct>] Number = 
    | Number of bigint * bigint
    /// Top part of the fraction
    member inline t.Top = match t with Number (top, _) -> top
    /// Bottom part of the fraction
    member inline t.Bot = match t with Number (_, bot) -> bot
    /// Convert to bigint. Same as 'bigint x'
    member inline t.BigInt = match t with Number (t, b) -> t / b
    /// Convert to float. Same as 'float x'
    member inline t.Float = match t with Number (t, b) -> float ((t * 1000000I) / b) / 1000000.0
    member inline t.Sign = sign t.Top
    static member inline Abs (Number (t, b)) = Number (abs t, b)
    static member inline Zero = Number (0I, 1I)
    static member inline One = Number (1I, 1I)
    static member inline Inf = Number (1I, 0I)
    static member inline NInf = Number (-1I, 0I)
    static member inline NaN = Number (0I, 0I)

    /// Use this to ensure the top and bottom part aren't divisable and 
    /// special values such as NaN and Zero have correct repressetation.
    /// To be called after any operation.
    static member inline private Reduce (Number (t, b)) =
        if t = 0I then Number.Zero
        elif b = 0I then Number (bigint (sign t), b)
        else
            let t, b = t * bigint (sign b), abs b
            let common = bigint.GreatestCommonDivisor (t, b)
            Number (t / common, b / common)

    static member inline (+) (Number (t1, b1), Number (t2, b2)) = 
        Number (t1*b2 + t2*b1, b1*b2) |> Number.Reduce
    static member inline (~-) (Number (t, b)) = 
        Number (-t, b) |> Number.Reduce
    static member inline (-) (x: Number, y: Number) = 
        x + (-y)
    static member inline ( * ) (Number (t1, b1), Number (t2, b2): Number) = 
        Number (t1*t2, b1*b2) |> Number.Reduce
    static member inline (/) (Number (t1, b1), Number (t2, b2)) = 
        Number (t1*b2, b1*t2) |> Number.Reduce
    interface IComparable with
        member t1.CompareTo t2 =
            let diff = t1 - (t2 :?> Number) 
            sign diff
    interface Number IEquatable with
        member t1.Equals t2 = (t1 :> IComparable).CompareTo t2 = 0
    override t1.Equals t2 = 
        match t2 with
        | :? Number as t2 -> (t1 :> Number IEquatable).Equals t2
        | _ -> false
    override t.GetHashCode() = LanguagePrimitives.GenericHash t

    static member inline FromFloat(x: float) = 
        if x = nan then Number.NaN
        elif x = infinity then Number.Inf
        elif x = -infinity then Number.NInf else
            let x, s = abs x, sign x
            if s = 0 then Number.Zero
            else
                let beforeDot, afterDot, fractBot = bigint x, bigint((x % 1.0) * (10.0 ** 15.0)), 10I ** 15
                Number(afterDot + fractBot*beforeDot, fractBot * bigint s) |> Number.Reduce
    
    static member inline op_Explicit(x: Number): float = x.Float
    static member inline op_Explicit(x: Number): bigint = x.BigInt
    static member inline op_Explicit(x: Number): int = x.BigInt |> int
    static member inline op_Explicit(x: float): Number = Number.FromFloat x
    static member inline op_Explicit(x: bigint): Number = Number (x, 1I)
    static member inline op_Explicit(x: int): Number = Number (bigint x, 1I)

    // static member Pown (x: Number, n) = Number(bigint.Pow(x.Top, n), bigint.Pow(x.Bot, n))
    // static member Root (A: Number, n: int) = 
    //     let percision = Number(1I,100I)
    //     let maxIter = 20
    //     //  Find x for (x^n = A)
    //     //  https://en.wikipedia.org/wiki/Nth_root_algorithm
    //     let n' = Number (bigint n, 1I) |> Number.Reduce
    //     let f(x: Number) = Number.Pown (x, n) - A
    //     let iter x = ((n' - Number.One)*x + A/(Number.Pown (x, n - 1))) / n' 

    //     let timer = Diagnostics.Stopwatch.StartNew()
    //     let mutable i = 0
    //     let mutable x = A
    //     while i < maxIter && timer.Elapsed.TotalSeconds < 10.0 && abs(f(x)) > percision do
    //         x <- (x + iter x) * Number(1I, 2I) 
    //         i <- i + 1
    //         printfn "%O" i
    //     timer.Stop()
    //     x
    // static member Pow (x, Number (integerPower, n)) =
    //     Number.Root(Number.Pown (x, int integerPower), int n)
    static member Pow (Number(t1,b1) as n1, (Number(t2,b2) as n2)) = 
        if b2 = 1I then
            Number(pown t1 (int t2), pown b1 (int t2))
        else 
            //  We approximate by using float power
            let n1 = n1.Float 
            let n2 = n2.Float
            let y = n1 ** n2
            Number.FromFloat y


    override n.ToString() = 
        match n with 
        | Number (t, b) when b = 1I -> sprintf "%O" t
        | Number (t, b) -> Seq.minBy String.length [sprintf "%O/%O" t b; string n.Float]

    static member inline Cast t = Number.FromFloat t
    static member inline Cast t = Number(bigint (t: int), 1I)
    static member inline Cast t = Number(t, 1I)
    
let zero = Number.Zero
let one = Number.One
let inf = Number.Inf
let ninf = Number.NInf
let NaN = Number.NaN

//type System.Double with
//    member inline t.ToNumber() = Number.FromFloat t
//type System.Int32 with
//    member inline t.ToNumber() = Number(bigint t, 1I)
//type System.Numerics.BigInteger with
//    member inline t.ToNumber() = Number(t, 1I)

let inline number x = cast<_, Number> x

let _ = number 2