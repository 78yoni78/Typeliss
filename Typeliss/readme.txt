```
                          T Y P E L I S S

    TM Yonatan's Partial-application Engine Lisp Inspired Syntax System 

                        F U N C T I O N S
    x + y       -   Add 2 numbers or strings
    x - y       -   You can
    x * y       -           figure it 
    x / y       -                       out yourself
    pi          -   3.14...
    e           -   2.718...
    (x > y) (_ -> something...)    -   Does something if x > y

    print value     -   Print any value
    do f1 f2 ... fn -   Call every function 
    for n i f   -   Call f(i) n times passing the result back
                    For example:    (for 3 "input" f) is f(f(f("input")))
    x ; y   -   returns y. For example: (print "hello!" ; 2) prints 
                                        and returns 2
    x |> f  -  same as (f x)

    (x1 , x2 , ... , xn .)  -   Create a tuple of all the xn's
    map (x -> something...) tuple - Does something with each x in the tuple
    fold start-value (value -> x -> some-new-value...) tuple - Foreach 
                    element in the tuple pass the value and the element x 
                    to the function and pass the new value next time
                    Example: (fold 0 (val -> x -> val + x) (1 , 2 , 3 .))
                             takes the sum of elements in the tuple

                            S Y N T A X
    No operator order.  functions can be called in 2 ways:
    function arg1 arg2 ... argn
    arg1 function arg2 ... argn
    For example:    you can write (1 + 2 + 3) or ((+ 1 2) + 3)
                    or (+ (+ 1 2) 3)
                    you can write (bigger 4 3) or (3 bigger 4)
    ALWAYS add more spaces and () between terms.
    The parser is very space sensitive
            :thumbsdown:: print"hi!"; 1+2*7
            :thumbsup:: (print "hi!") ; (1 + (2 * 7))
```