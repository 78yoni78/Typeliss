# typeliss

*Typeliss* is a language originally made for running small scripts on a discord bot!
The name stands for *"TM Yonatan's Partial-application Engine Lisp Inspired Syntax System"* and is a pun on the word typeless as this is my first serious attempt at a non-statically typed language (and I couldn't fit an "e").

I designed it to be very readable and short, so it would be easy to type in as a command to the bot, while still based on function and mathematical concepts most find hard to read by making them intuitive. 

Additionally, I really wanted this language to be easy to maintain and expand, and I did it but merging syntax functions. 

# syntax

The language is made to be readable and concise.
Most expressions can be read and understood even if you don't know the specifics of the syntax.
Writing can be a little more difficult, but once you understand and it should come naturally.

Let's talk about the syntax. 
##### But before that, an example program: 
```
match (input 'Enter a number: ') [
	(to-number then (num -> 
		print 'Your number: ';
		print num;
		if (num > 0) {
			print 'A positive number!'
		} {
			print 'A negetive number'
		}
    ))
    { 
    	print "You didn't enter a number! "
    }
]
```
Admittedly, this is a cherry-picked example (and it could be written shorter). And you may be wondering what the brackets mean. But what if I told you everything here is a function? 

```
if (input 'Enter a number: ' to-number) (num -> 
	if (num > 0) ('A positive number!') ('A negetive number') 
    |> (sign-message -> ['Your number: ' num sign-message])
) ["You didn't enter a number!"]
map print
```

##### All language constructs like `match`, `if`, `as`, `print` and even `+` and `;` are simple functions. 

* `match value [ pattern1 func1 pattern2 func2 ... ]` runs the first function with a matching pattern and returns the result 
* `if value {body} {else-body}` execute body if value is the result of a matching pattern
* `x + y` or `+ x y` adds 2 numbers or strings
* `x; y` executes x and y and returns y

##### Functions can be called in 2 ways 

`function arg1 arg2 ...` or `arg1 function arg2 ...`

For example, you can write: `print 'hello!'` or `'hello!' print`

This comes from the idea that one might say `"The man walks to the store"` or `"Walks, The man, to the store"`

This, sadly, means there is no operator order so `1 + 2 * 3` = `(1 + 2) * 3` because `+` is a function

##### Functional loops: `map`, `for`, `until`

* `map mappable (x -> new-x)` maps every x in the mappable to a new x
* `for iterable x (i -> x -> new-x)` for each element in the iterable, apply it and the x returned by the previous iteration to the function. Start with x. Same as the fold function in other languages. For example, `for 5 0 (i -> acc -> acc + i)` will return the sum `1 + 2 + 3 + 4 + 5`
* `until x (x -> body)` Apply x to the function. If it returns a value matching the pattern `("break")`

##### Typeliss types and patterns

A pattern is usually a function or a string.

* If it is a function, apply the function to the input. If it returned `some: value`, then the output is `value`. If it returned `none: ()`, then the pattern failed. 
* If it is a string, then if the input is a wrapper with a name equal to the string, matches and output is the value of the wrapper`

note the use of expressions like `some: value`, will be expanded upon further 

Some basic patterns: 
* `number?` is `Anything -> int option`
* `string?` is `Anything -> int option`
* `applicable?` is `Anything -> option (Anything -> option (() -> Anything))`
* `length?` is `Anything -> int option`
* `to-number?` is `string -> int option`
* `= x`  is `'T -> 'T -> 'T option`
* `!= x` is `'T -> 'T -> () option`
* `> x`  is `'T -> 'T -> () option`
* `< x`  is `'T -> 'T -> () option`
* `>= x` is `'T -> 'T -> () option`
* `<= x` is `'T -> 'T -> () option`
* `always-match` is `'T -> some: 'T`
* `never-match` is `Anything -> none: ()`

# Design Patterns

##### Implementing intefaces

Let's say you want to implement a vector object, which can be mapped and iterated and added.

We will need a `vector?` pattern that checks if a value is a vector and returns it, and a function `vector-to-tuple` that converts it to a tuple:

```let {vector?} (x -> 
	x match [
		"vector" some: 
		always-match {none: ()}
	])```

```let {vector-to-tuple} (x -> x match "vector" id)```
	
Now we need to *redefine* `for`, `map` and `+`.

```let {for} (iterable ->
	match iterable [
		vector? (vec -> for (vector-to-tuple vec))
		always-match {for iterable}
	])```
	
```let {map} (mappable ->
	match iterable [
		vector? (vec -> mapping -> map (vector-to-tuple vec) mapping |> vector:)
		always-match {map iterable}
	])```
	
```let {+} (addable ->
	match addable [
		vector? (vec1 -> vec2 -> zip vec1 vec2 |> map (+))
		always-match {map iterable}
	])```

```
let {ducks} (how-many-ducks -> repeat how-many-ducks '' (+ ':duck:')) {
    print (5 ducks)
}
```
