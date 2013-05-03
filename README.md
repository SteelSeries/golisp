golisp
======

Simple Lisp implimented in Go for use with SSEnext modular device support

Data Types
----------

**Cons Cells** used both as dotted pairs `(a .  b)` and general lists `(a b)`

**Symbols** are simple identifiers, e.g. `functionname`. Symbols follow the rules for Go identifiers: alphanumeric and underscode, beginning with a letter or underscore. The runtime defines some special symbols to name builtin functions, e.g. arithmetic functions.

**Strings** are any sequence of characters enclosed in double quotes, e.g. `"string"`.

**Numbers** are integers internally represented as `int` so their size reflects `int` on the build of Go being used. E.g. `5`, `9845376`. `-5`.

**Booleans** represent true & false. `nil`\`()` is false, and false is false, everything else is true. Boolean literals are `#t` and `#f` for true and false, respectively.

**Functions** are user defined procedures. The basic function definition is the anonymous lambda form:

    (lambda (x)
            (if (> x 10)
                (* x x)
                (+ x x)))

This creates a anonymous function taking one parameter. This function can be used as any value (returned, passed as an argument, etc).

You can create a named function using `define` as in Scheme:

    (define (foo x)
            (if (> x 10)
                (* x x)
                (+ x x)))

This can then be referred to by name: `(foo 5)`.

**Objects** allow you to encapsulate a Go object (struct) in a Lisp atom. There is no way to do this from Lisp itself, but is useful when writing primitive functions (see below).

**Primitives** are just as they are in Lisp or Smalltalk: functions written in Go and exposed as functions in Lisp. The combination of primitives and object atoms allow you to integrate with the underlying Go program.

Builtin functions
-----------------

`(+ <number>...)`

Adds a series of numbers, e.g.
    `(+ 4)` ==> 4
    `(+ 4 2)` ==> 6
    `(+ 4 2 7)` ==> 13

`(- <number>...)`

Sequentially subtracts a sequence of numbers. With a single argument is a special case that negates the argument. E.g.
    `(- 4)` ==> -4
    `(- 10 3)` ==> 7
    `(- 10 3 4)` ==> 3
    `(- 10 3 4 5)` ==> -2

`(* <number>...)`

Multiplies a series of numbers, e.g.
    `(+ 4)` ==> 4
    `(+ 4 2)` ==> 6
    `(+ 4 2 7)` ==> 13
 
`(/ <number>...)`

Sequentially divides a sequence of numbers. E.g.
    `(/ 30 2)` ==> 15
    `(- 30 2 3 )` ==> 5

`(% <number> <number>)`

Returns the remainder of dividion. E.g.
    `(% 4 2)` ==> 0
    `(% 4 3)` ==> 1 

`(< <number> <number>)`

Returns whether the first argument is less than the second argument.
    `(< 1 2)` ==> #t
    `(< 2 1)` ==> #f
    `(< 2 2)` ==> #f

`(> <number> <number>)`

Returns whether the first argument is greater than the second argument.
    `(> 1 2)` ==> #f
    `(> 2 1)` ==> #t
    `(> 2 2)` ==> #f

`(== <number> <number>)`

Returns whether the first argument is equal to the second argument.
    `(== 1 2)` ==> #f
    `(== 1 1)` ==> #t

`(!= <number> <number>)`

Returns whether the first argument is not equal to the second argument.
    `(!= 1 2)` ==> #t
    `(!= 1 1)` ==> #f

`(<= <number> <number>)`

Returns whether the first argument is less than or equal to the second argument.
    `(<= 1 2)` ==> #t
    `(<= 2 1)` ==> #f
    `(<= 2 2)` ==> #t

`(>= <number> <number>)`

Returns whether the first argument is greater than or equal to the second argument.
    `(>= 1 2)` ==> #f
    `(>= 2 1)` ==> #t
    `(>= 2 2)` ==> #t

`(! <arg>)`

Returns whether the boolean negation of the argument.
    `(! #t)` ==> #f
    `(! #f)` ==> #t

Special forms
-------------

`(if <condition> <true clause>)`

If the condition evaluates to logically true, the true clause is evaluated and the result is the result of the if form, otherwise nil is the result of the if form.
    `(if (< 1 2) "less")` ==> "less"
    `(if (< 2 2) "less")` ==> nil

`(if <condition> <true clause> <else clause>)`

If the condition evaluates to logically true, the true clause is evaluated and the result is the result of the if form, otherwise the else clause is evaluated and is the result of the if form.
    `(if (< 1 2) "less" "not less")` ==> "less"
    `(if (< 2 2) "less" "not less")` ==> "not less"

'(lambda (<param>...) <expr>...)`

Creates an anonymous function. This can then be used in a function call.
    `((lamdba (x) (+ x x)) 5)` ==> 10

`(define <symbol> <value>)`

Evaluates the value expression and binds it to the symbol, returning the value.
    (define x (+ 2 3)) ==> 5
    x ==> 5

`(define (<symbol> <param>...) <body>)`

Define a function:
- `<symbol>` specifies the name (how you reference the function)
- `<param>...` parameters of the function, these are bound to the respective arguments when the function is called.
- `<body>` the sequence of expressions that are evaluated in order when the function is called. The final evaluation result becomes the value of evaluation of the function.

    (define (double x)
            (+ x x))

    (double 5) ==> 10

`(quote <expr>)`

Surpresses evaluation of the expression.

     (quote (+ 1 2)) ==> (+ 1 2)

There is a shortcut for quote that uses the single quote.  NOTE: Due to the use of the go scanner, we can not use a single quote as Lisp does. The entire expression has to be enclosed in single quotes:

    '(+ 1 2)' ==> (+ 1 2)

`(map <function> <list>)`

Applies 'function' (which has tobe opf a single argument) to each element in `list` in order, returning the list of the results.

    (map - '(1 2 3 4)')) ==> (-1 -2 -3 -4)

    (map (lambda (x)
                 (* x x))
         '(1 2 3 4)')
    ==> (1 4 9 16)

Defining Primitives
-------------------

The Go function `MakePrimitiveFunction` allows you to create primitive functions. This takes three arguments:

1. The function name. This is a standard symbol which will be used to reference the function.
2. The number of expected arguments. Using a -1 for this denotes any number of arguments. In the function definition you can enforce further constraints on argument counts and types.
3. The Go function which impliments the primitive. This function **must** have the signature `func <N
e>(args *Data) (result *Data, err error)`

An example:

    MakePrimitiveFunction("!", 1, BooleanNot)

    func BooleanNot(args *Data) (result *Data, err error) {
        if Length(args) != 1 {
            err = errors.New(fmt.Sprintf("! requires 1 argument. Received %d.", Length(args)))
            return
        }

        arg, err := Eval(Car(args))
        if err != nil {
            return
        }

        val := BooleanValue(arg)
        return BooleanWithValue(!val), nil
     }

The core lisp data element is the Data type which logically contains a type tage and a value. The type tags are defined by he constants:

    const (
        ConsCellType = iota
        NumberType
        BooleanType
        StringType
        SymbolType
        FunctionType
        PrimitiveType
        ObjectType
    )

If you need to check the type of a piece of data you can fetch it's type using the `TypeOf(*Data) int` fuction and then compare it to a type tag constant. Additionally there are predicate functions for the most common types:

    SymbolP(*Data) bool
    NumberP(*Data) bool
    PairP(*Data) bool
    ListP(*Data) bool
    FunctionP(*Data) bool

**Creating data**

There are various convience functions that you can use to create data:

    Cons(car *Data, cdr *Data) *Data
    NumberWithValue(n int) *Data
    BooleanWithValue(b bool) *Data
    StringWithValue(s string) *Data
    SymbolWithName(s string) *Data
    ObjectWithValue(o unsafe.Pointer) *Data

Similarly there are convienence functions for extracting primitive values from data elements:

    IntValue(d *Data) int
    StringValue(d *Data) string
    BooleanValue(d *Data) bool
    ObjectValue(d *Data) unsafe.Pointer

The `String(*Data) string` function returns a nice string representation of the data.

The `Eval(*Data) (*Data, error)` function evaluates a piece of data and returns the result and any error that occured dutign evaluation. If an error is returned, the value of the data result is indeterminant.

Other useful functions include:

    Length(*Data) int
    IsEqual(*Data, o *Data) bool
    NilP(*Data) bool
    NotNilP(*Data) bool

A useful integration function is Apply:

    Apply(function *Data, args *Data) (result *Data, err error)

This takes a function elemment, either a user defined one or a primitive, and applies it to the provided arguments.

