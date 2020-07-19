T Y P E L I S S

TM Yonatan's Partial-application Engine Lisp Inspired Syntax System 

S Y N T A X

The language is made to be readable and consise.
Most expressions can be read and understood even if you dont know the specifics of the syntax.
Writing can be a little more difficult, but once you understand the syntax it should be clear. 

Before that, an example program: 

match (input 'Enter a number: ') [
         parse-number (num -> 
                 print 'Your number plus 2: ';
	print (num + 2))
         always-match (str -> print "You didn't enter a number! ")
]

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