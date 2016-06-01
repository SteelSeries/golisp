---
layout: golisp
title: "SteelSeries GoLisp: Integrating with Go"
---

<div class="big-header">
    Integrating with Go
</div>

## Go &rarr; Lisp: parsing and evaluating Code

The example is based on an equation evaluator that accepts simple
algebraic equations using basic operators and parentheses, such as:

    2+3 => 5
    2+3*4 => 24
    (2+3)*4 => 20

Starting with that, I integrated GoLisp. My first step was to add support for using lisp functions in equations. The `convert2postfix` function gets a branch to handle function names, along with the`isLispFunction` predicate.

The function handler creates a `Symbol` from the token and looks it up in GoLisp's global environment frame. This makes sure it is a name known to GoLisp. Then I check that it is, indeed, a function. If either check fails, it returns an error. Otherwise I push the token onto the `functions` stack which keeps track of the names and nesting as well as whether it's in an argument list. I also needed to quietly ignore the argument separating commas.


    var func_rx = regexp.MustCompile(`([a-zA-Z\-\_]+)`)

    func isLispFunction(token string) bool {
        return func_rx.MatchString(token)
    }

    ...
    var stack Stack
    var functions Stack
    
    for _, token := range tokens {

        if isLispFunction(token) {
            f := Global.ValueOf(SymbolWithName(token))
            if NilP(f) {
                err = errors.New(fmt.Sprintf("No function named %s", token))
                return
            } else if !FunctionP(f) {
                err = errors.New(fmt.Sprintf("%s is not a function", token))
                return
            } else {
                functions.Push(token)
            }

        } else if token == "," {

        } else if isOperator(token) {

Later in the function, I need to tweak the closing paren handler to
insert the function name into the result and pop it from
the `functions` stack if the paren closes an argument list

    ...

    } else if token == ")" {
    PAREN:
        for {
            top, err := stack.Top()
            if err == nil && top != "(" {
                pop, _ := stack.Pop()
                result = append(result, pop.(string))
            } else {
                stack.Pop() // pop off "("
                if !functions.IsEmpty() {
                    f, _ := functions.Pop()
                    result = append(result, f.(string))
                }
                break PAREN
            }
        }
    }
    ...

That lets us have things like `foo(2,4*6)` get converted to `2, 4, 6,
*, foo` in the postfix array. Next I need to handle GoLisp functions
in the evaluator. To do that I added the following branch to the
`evaluatePostfix` function:

    } else if isLispFunction(token) {
        //"<token>(<float1> <float2>...)"
        f := Global.ValueOf(SymbolWithName(token))
        var numberOfArgs = 0
        if TypeOf(f) == FunctionType {
            numberOfArgs = f.Func.RequiredArgCount
        } else if TypeOf(f) == PrimitiveType {
            numberOfArgs = f.Prim.NumberOfArgs
        }

        var args []*Data

        // collect args
        for i := 0; i < numberOfArgs; i++ {
            op, err := stack.Pop()
            if err != nil {
                return nil, err
            }
            float := BigratToFloat(op.(*big.Rat))
            args = append(args, FloatWithValue(float32(float)))
        }
        val, err := Apply(f, Reverse(ArrayToList(args)), Global)
        if err != nil {
            return nil, err
        }
        result = FloatToBigrat(float64(FloatValue(val)))
        stack.Push(result)
    } else {

This starts by fetching the value of the function name token from the
global environment frame. Note that I've already checked it's validity
in the infix->postfix converter. Then, based on whether it is a user
defined or primitive function, I extract the number of arguments it
expects. There is a limitation here in that you can't use primitives
that can take a variable number of functions. This could be dealt with
by keeping a count of arguments provided and using that. That would
also allow us to do argument count validation.

Once I know how many arguments are expected, I pop them off the stack
and build an array of GoLisp floating point values. The array is then
converted to a GoLisp list (which has to be reversed due to the effect
of having popped the arguments) to which the named function is
applied. This passes control to the GoLisp runtime which does the
evaluation.

That's one side of the integration.

When the app starts up it loads in any `.lsp` files in the `lisp`
directory. The example repo contains the following in `functions.lsp`:

    (define (fact x)
      (cond ((< x 2) 1)
            (else (* x (fact (- x 1))))))

    (define (double-fact x)
      (* 2 (go-fact x)))

    (define (multiply x y)
      (* x y))

    (define (scale x)
      (* x CONSTANT))

I can now use the functions `fact` and `multiply` in some equations
(I'll discuss the rest a bit later):

    > fact(4)
    ==> 24/1
    > fact(2+2)
    ==> 24/1
    > fact(4)*2
    ==> 48/1
    > multiply(3,5)
    ==> 15/1
    > multiply(2,fact(4))
    ==> 48/1

## Lisp &rarr; Go: defining primitives

Now let's look at the other side of integration: calling Go from Lisp.
Have a look at the file `lisp_prims.go`. In it I define a constant for
use in Lisp as well as the `go-fact` function I used from
the `double-fact` function in `functions.lsp`. I'll step through it
now.

The usual package and imports as required. Notice that I import
golisp, and alias it to `.` to make the code less wordy.

    import (
        "errors"
        . "github.com/steelseries/golisp"
    )

Next is an initializer. Since I import `golisp`, the lisp runtime is
setup by the time this initializer is executed, so I can use the
`Global` environment frame.

First, a global identifier `CONSTANT` is define (aka bound to) the
floating point value `42.0`. Any global constant you desire can be
created in this way and will be available in your lisp code, e.g.
the `scale` function:

    > scale(2)
    ==> 84/1

Next a primitive function is defined. In this case it is
named`go-fact`, takes a single argument, and is implemented by the Go
function `GoFactImpl`. The binding is placed in the global environment
frame.

    func init() {
        Global.BindTo(SymbolWithName("CONSTANT"), FloatWithValue(float32(42.0)))
        MakePrimitiveFunction("go-fact", "1", GoFactImpl)
    }

Now for the primative's implementation. Every primitive implementation
function has an identical signature (other than name). They take a
Lisp list of arguments and the environment top evaluate in. They
return the result as a Lisp object, as well as a possible error.

By definition, primitives receive their argument unevaluated, so if
it's called with a symbol as an argument, e.g. `(go-fact x)` it's
first argument will be the symbol, e.g. `x`. The first step is usually
to evaluate the arguments and do any required type checks. I know
there is a single argument, so I simply `Eval(Car(args))`. That
results in a Lisp value and a possible error. The error is handled
however is appropriate, which is often simply to return it.

    func GoFactImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
        val := Car(args)

Now that the argument has been evaluated without error, I can check
that it's the type I expect: in this case a float.

        if !FloatP(val) {
            return nil, errors.New("go-fact requires a float argument")
        }

Then I can confidently extract the Go value. In this case since I need
an integer for computing a factorial, I'll convert it now.

        n := int(FloatValue(val))

Then it's just a matter of doing what the function should do: compute
the factorial.

        f := 1
        for i := 1; i <= n; i++ {
            f *= i
        }

Now that I have the result, I convert it back to a lisp value and return it.

        return FloatWithValue(float32(f)), nil
    }

And that's it. Now I can use `go-fact` in the equations:

    > go-fact(4)
    ==> 24/1

Code in Go, calling into Lisp, which calls back into Go. The cycle is
complete.

Enjoy.
