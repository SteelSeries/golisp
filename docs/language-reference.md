---
layout: golisp
title: "SteelSeries GoLisp v1.1: Language Reference"
---

<div class="big-header">
    Language Reference v1.1
</div>

<div class="toc"></div>

<script src="/js/toc.js"></script>

<script type="text/javascript">
$(document).ready(function() {
    $('.toc').toc();
});
</script>

Steelseries GoLisp is a Lisp-like language inspired by and based on the
opensource MIT/GNU Scheme. As such, much of the functionallity is identical and
much of the text herein is taken more or less verbatim from the associated
reference manual [6]. Only functions documented here are included, and any
differences from the MIT/GNU Scheme equivalents are noted.

# Data types

**Booleans** represent true and false. Boolean literals are `#t` and `#f` for
  true and false, respectively. The only thing that is considered to be
  logically false is `#f`. Everything else is logically true, inluding 0 and the
  empty list, which may surprise some.

**Integers** are sixtyfour bit signed integers. Both decimal ,hexadecimal, and
  binary formats are supported. E.g. `26`, `#x1a`, `#x1A`, `#b00011001`. The
  C-style format for hexadecimal numbers is also supported: `0x1A`.

**Floats** are Go `float32` numbers. Accordingly they are signed. All arithmetic
  functions with the exception of modulus work as expected for both integers and
  floats. Numbers are coerced to floats as required, specifically if any
  arguments are float, all are converted to float and the result will be a
  float.

**Characters** are single characters. This is differen than strings with length 1.

**Strings** are any sequence of characters other than `"` enclosed by a pair of
  `"`, e.g. `"string"`. If you need to have `"` in a string, use `\"`.

**Symbols** are simple identifiers, e.g. `function-name`. Symbols follow the
  follow 3 simple rules:

- can only contain graphic characters (i.e. no control characters)
- can not contain any of the characters: ();,\"`|[]{}
- can not begin with a number or single quote
- can not contain whitespace

Typically, `-` is used to separate words in a symbol, `_` is used in special
symbols (such as system use) to separate words and as a prefix and suffix. The
characters `?`, `!`, and `*` are typically used as the final character of a
function name to denote:

**?** a predicate, e.g. `nil?`

**!** a mutating function (changes the argument rather than returning
      a modified copy), e.g. `set-car!`

**\*** a variant of the primary function, e.g. `flatten` (which does a one level
flattening of a list) and `flatten*` (which is a recursive `flatten`)

If a symbol ends with `:` it is what is called a _naked symbol_. It has no value
other than itself. If it is evaluated, the result is the symbol itself. This
feature is utilized by frames.

**Cons Cells**, aka **Lists** are the central data type in classic Lisp used
  both as dotted pairs `(a . b)` and general lists `(a b)`. For an overview of
  cons cells and how to use them see \[1\], \[3\], or
  `http://cs.gmu.edu/~sean/lisp/cons/`. Note that dotted pairs *require* spaces
  around the period; `(a.b)` is a list containing the symbol `a.b`, not a cons
  cell with car of `a` and cdr of `b`.

**Bytearrays** are simply objects that encapsulate `[]byte` objects. The
  difference is that there is syntactic support for them. Use square braces
  surrounding a list of numbers between 0 and 255, inclusive. For example:
  `[1 2 3 4 5]`. That format will parse to an `object` containing a the Go
  bytearray (i.e. `[]byte`). Bytearrays evaluate to themselves. There are also
  functions for doing bytearray manipulation.

**Channels** are simple objects that encapsulate Go 'channel' objects.

**Vectors** are a more efficient alternative to lists when the number of
  elements are known ahead of time and static. A vector can be grown, but it's
  done explicitly with a function. Many enumeration and access functions that
  accept a list (or lists) can be passed a vector (or vectors) instead.

**Ports** provide access to files for reading and writing.

**Frames** are sets of named slots that hold arbitrary values, including
  functions. Frames can _inherit_ from other frames in a prototypical manner.
  They are inspired by the languages Self [4] and Newtonscript [5].

**Functions** are user defined procedures. They are covered in detail later.

**Macros** are user defined syntactic extensions. Note that GoLisp macros are
  not yet hygenic, so use caution.

You can create an manipulate all of the above types in GoLisp. There are two
more types that are usable in GoLisp, but can only be created in Go.

**Primitives** are just as they are in Lisp or Smalltalk: functions written in
  Go and exposed as functions in Lisp. The combinaion of primitives and objects
  allow you to integrate with the underlying Go program. **Special Forms** are
  almost identical to primitives, except that they use normal evaluation order
  instead of applicative order which functions and primitives use.

**Objects** allow you to encapsulate a Go object (struct) in a Lisp data object.
  There is no way to do this from Lisp itself, but is useful when writing
  primitive functions (see below).


# Special Forms

A special form is an expression that follows special evaluation rules. This
section describes the basic GoLisp special forms.

## Lambda Expressions ##

### (lambda _formals_ _sexpr_...) ###

A **lambda** expression evaluates to a procedure. The environment in effect when
the **lambda** expression is evaluated is remembered as part of the procedure;
it is called the _closing environment_. When the procedure is later called with
some arguments, the closing environment is extended by binding the variables in
the formal parameter list to fresh locations, and the locations are filled with
the arguments according to rules about to be given. The new environment created
by this process is referred to as the _invocation environment_.

Once the invocation environment has been constructed, the _sexprs_ in the body
of the **lambda** expression are evaluated sequentially in that environment.
This means that the region of the variables bound by the **lambda** expression
is all of the _sexprs_ in the body. The result of evaluating the last _sexpr_ in
the body is returned as the result of the procedure call.

_formals_, the formal parameter list, is often referred to as a _lambda list_.

The process of matching up formal parameters with arguments is somewhat
involved, but simpler than Scheme. There are two types of parameters, and the
matching treats each in sequence:

**Required**

All of the _required_ parameters are matched against the arguments first. If
there are fewer arguments than required parameters, an error of type `wrong
number of arguments` is signalled; this error is also signalled if there are
more arguments than required parameters and there are no further parameters.

**Rest**

Finally, if there is a _rest_ parameter (there can only be one), any remaining
arguments are made into a list, and the list is bound to the rest parameter. (If
there are no remaining arguments, the rest parameter is bound to the empty
list.)

In Scheme, unlike some other Lisp implementations, the list to which a rest
parameter is bound is always freshly allocated. It has infinite extent and may
be modified without affecting the procedure's caller.

A period, i.e. ".", is used to separate the _rest_ parameter (if there is one)
from the _required_ parameters. The "." **must** be surrounded by spaces.

`(a b c)`: `a`, `b`, and `c` are all required. The procedure must be passed
exactly three arguments.

`(a b . c)`: `a` and `b` are required and `c` is rest. The procedure may be
passed two or more arguments.

Some examples of **lambda** expressions:

    (lambda (x) (+ x x))            ⇒  <function: unnamed>

    ((lambda (x) (+ x x)) 4)        ⇒  8

    (define reverse-subtract
      (lambda (x y)
        (- y x)))
    (reverse-subtract 7 10)         ⇒  3

    (define foo
      (let ((x 4))
        (lambda (y) (+ x y))))
    (foo 6)                         ⇒  10


## Lexical Binding ##

The three binding constructs `let`, `let*`, and `letrec`, give Scheme
block structure. The syntax of the three constructs is identical, but they
differ in the regions they establish for their variable bindings. In a `let`
expression, the initial values are computed before any of the variables become
bound. In a `let*` expression, the evaluations and bindings are sequentially
interleaved. And in a `letrec` expression, all the bindings are in effect
while the initial values are being computed (thus allowing mutually recursive
definitions).

### (let ((_variable_ _init_)...) _sexpr_...) ###

The _inits_ are evaluated in the current environment (in some unspecified
order), the _variables_ are bound to fresh locations holding the results, the
_sexprs_ are evaluated sequentially in the extended environment, and the value
of the last _sexpr_ is returned. Each binding of a _variable_ has the
sequence of _sexpr_ as its region.

GoLisp allows any of the _inits_ to be omitted, in which case the corresponding
_variables_ are unassigned.

Note that the following are equivalent:

    (let ((variable init)...) expression...)
    ((lambda (variable...) expression...) init...)

Some examples:

    (let ((x 2) (y 3))
      (* x y))                              ⇒  6

    (let ((x 2) (y 3))
      (let ((foo (lambda (z) (+ x y z)))
            (x 7))
        (foo 4)))                           ⇒  9

### (let* ((_variable_ _init_)...) _sexpr_...) ###

`let*` is similar to `let`, but the bindings are performed sequentially
from left to right, and the region of a binding is that part of the `let*'
expression to the right of the binding. Thus the second binding is done in an
environment in which the first binding is visible, and so on.

Note that the following are equivalent:

    (let* ((variable1 init1)
           (variable2 init2)
           ...
           (variablen initn))
       expression...)

    (let ((variable1 init1))
      (let ((variable2 init2))
        ...
          (let ((variablen initn))
            expression...)
        ...))

An example:

    (let ((x 2) (y 3))
      (let* ((x 7)
             (z (+ x y)))
        (* z x)))                           ⇒  70

### (letrec ((_variable_ _init_)...) _sexpr_...) ###

The _variables_ are bound to fresh locations holding unassigned values, the
_inits_ are evaluated in the extended environment (in some unspecified order),
each _variable_ is assigned to the result of the corresponding _init_, the
_expressions_ are evaluated sequentially in the extended environment, and the
value of the last _expression_ is returned. Each binding of a _variable_ has the
entire `letrec` expression as its region, making it possible to define
mutually recursive procedures.

Any of the _inits_ may be omitted, in which case the
corresponding _variable_ is unassigned.

    (letrec ((even? (lambda (n)
                      (if (zero? n)
                          #t
                          (odd? (- n 1)))))
             (odd? (lambda (n)
                     (if (zero? n)
                         #f
                         (even? (- n 1))))))
    (even? 88))                                 ⇒  #t

One restriction on `letrec` is very important: it shall be possible to
evaluated each _init_ without assigning or referring to the value of any
_variable_. If this restriction is violated, then it is an error. The
restriction is necessary because Scheme passes arguments by value rather than by
name. In the most common uses of `letrec`, all the _inits_ are **lambda**
expressions and the restriction is satisfied automatically.

## Definitions ##

### (define _variable_ [_sexpr_]) ###

### (define _formals_ _sexpr_...) ###

Definitions are valid in some but not all contexts where expressions are
allowed. Definitions may only occur at the top level of a program and at the
beginning of a lambda body (that is, the body of a **lambda**, `let`,
`let*`, `letrec`, or _procedure `define`_ expression). A definition that
occurs at the top level of a program is called a _top-level definition_, and a
definition that occurs at the beginning of a body is called an _internal
definition_.

The second form is used as a shorthand to define a procedure. The first
"required parameter" in _formals_ is not a parameter but the _name_ of the
resulting procedure; thus _formals_ must have at least one required parameter.

Hence the following are identical.

    (define inc (lambda (x) (+ x 1)))
    (define (foo x) (+ x 1))

Using this form of define, a function that accepts a completely option set of arguments can be made:

    (define (f . args) args)
    
    (f) ⇒ ()
    (f 1) ⇒ (1)
    (f 1 2 3) ⇒ (1 2 3)

Please note: You can not currently define a lambda with completely optional arguments.

## Type Signatures ##

GoLisp provides basic, and optional, type checking for the arguments and return values of user defined functions. Additionally, primtive functions also have type checking on arguments, as appropriate.

### (typedef (_fname_ _arg-types_...) [-> _return-type_]) ###

This is similar to defining a function: _fname_ is the name of a function that will be defined later (typicaly the next form) and _arg-types_ correspond to its arguments). But with `typedef` these are argument type specification, not argument names.

Argunment type specifications can take two forms: _type_ which can be a string or symbol, or a set of types separated by a pipe (i.e. |) with no spaces. The latter must be a string.

When a function is passed a type that does not match its specified type(s) an error is raised, similar to:

```
> (typedef (less-than number number))
> (define (less-than x y) (< x y))
> (less-than 1 4.3)
==> #t
> (less-than 1 'a)
Error in evaluation: 
Evaling (less-than 1 'a). less-than argument 1 has the wrong type, expected float or integer but was given symbol
```

A type specification can also include a type specification of the result of the function. Note that the `->` is required.

```
> (typedef (less-than number number) -> boolean)
==> nil
> (define (less-than x y) (if (< x y) 'yes 'no))
==> <function: less-than>
> (less-than 1 4.3)
Error in evaluation: 
Evaling (less-than 1 4.3). less-than returns the wrong type, expected boolean but returned float
```

The following types are supported:

* list
* vector
* sequence _(equivalent to list|vector)_
* integer
* float
* number _(equivalent to integer|float)_
* boolean
* string
* character
* symbol
* function
* macro
* primitive
* procedure _(equivalent to function|primitive)_
* boxedobject
* frame
* environment
* port

Note that the _list_ type just requires a ConsCell; if a proper list or other specific type is required, then either [pre-conditions](http://daveastels.typed.com/blog/code-contracts-in-golisp) or explicit tests will be needed.

## Top-Level Definitions ##

A top-level definition,

    (define _variable_ _sexpr_)

has essentially the same effect as this assignment expression, if _variable_ is
bound:

    (set! _variable_ _expression_)

If _variable_ is not bound, however, `define` binds _variable_ to a new
location in the current environment before performing the assignment (it is an
error to perform a `set!` on an unbound variable).

    (define add3
      (lambda (x) (+ x 3)))                ⇒  unspecified
    (add3 3)                                ⇒  6

    (define first car)                      ⇒  unspecified
    (first '(1 2))                          ⇒  1

    (define bar)                            ⇒  unspecified
    bar                                     error--> Unassigned variable

## Internal Definitions ##

An _internal definition_ is a definition that occurs at the beginning of a
_body_ (that is, the body of a `lambda`, `let`, `let*`, `letrec`, or
_procedure `define`_ expression), rather than at the top level of a program.
The variable defined by an internal definition is local to the _body_. That is,
_variable_ is bound rather than assigned, and the region of the binding is the
entire _body_. For example,

    (let ((x 5))
      (define foo (lambda (y) (bar x y)))
      (define bar (lambda (a b) (+ (* a b) a)))
      (foo (+ x 3)))                        ⇒  45

A _body_ containing internal definitions can always be converted into a
completely equivalent `letrec` expression. For example, the `let` expression
in the above example is equivalent to

    (let ((x 5))
      (letrec ((foo (lambda (y) (bar x y)))
               (bar (lambda (a b) (+ (* a b) a))))
        (foo (+ x 3))))

## Assignments ##

### (set! _variable_ _object_) ###

_expression_ is evaluated and the resulting value is stored in the location to
which _variable_ is bound. The value of the `set!` expression is unspecified.

_variable_ must be bound either in some region enclosing the `set!`
expression, or at the top level. However, **variable** is permitted to be
unassigned when the `set!' form is entered.

    (define x 2)                            ⇒  unspecified
    (+ x 1)                                 ⇒  3
    (set! x 4)                              ⇒  unspecified
    (+ x 1)                                 ⇒  5

## Quoting ##

This section describes the expressions that are used to modify or prevent the
evaluation of objects.

### (quote _datum_) ###

**(quote _datum_)** evaluates to _datum_. _datum_ may be any external
representation of a GoLisp object. Use `quote` to include literal constants in
Scheme code.

    (quote a)                               ⇒  a
    (quote #(a b c))                        ⇒  #(a b c)
    (quote (+ 1 2))                         ⇒  (+ 1 2)

`(quote datum)` may be abbreviated as `'DATUM`. The two notations are equivalent
in all respects.

    'a                                      ⇒  a
    '#(a b c)                               ⇒  #(a b c)
    '(+ 1 2)                                ⇒  (+ 1 2)
    '(quote a)                              ⇒  (quote a)
    ''a                                     ⇒  (quote a)

Numeric constants, string constants, character constants, and boolean constants
evaluate to themselves, so they don't need to be quoted.

    '"abc"                                  ⇒  "abc"
    "abc"                                   ⇒  "abc"
    '145932                                 ⇒  145932
    145932                                  ⇒  145932
    '#t                                     ⇒  #t
    #t                                      ⇒  #t

### (quasiquote _template_) ###

"Backquote" or "quasiquote" expressions are useful for constructing a list or
vector structure when most but not all of the desired structure is known in
advance. If no commas appear within the _template_, the result of evaluating
<code>`template</code> is equivalent (in the sense of `equal?`) to the result
of evaluating `'template`. If a comma appears within the _template_, however,
the expression following the comma is evaluated ("unquoted") and its result is
inserted into the structure instead of the comma and the expression. If a comma
appears followed immediately by an at-sign (@), then the following expression
shall evaluate to a list; the opening and closing parentheses of the list are
then "stripped away" and the elements of the list are inserted in place of the
comma at-sign expression sequence.

    `(list ,(+ 1 2) 4)                       ⇒  (list 3 4)

    (let ((name 'a)) `(list ,name ',name))   ⇒  (list a 'a)

    `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)    ⇒  (a 3 4 5 6 b)

    `((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))
                                             ⇒  ((foo 7) . cons)

    `#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)
                                             ⇒  #(10 5 2 4 3 8)

    `,(+ 2 3)                                ⇒  5

Quasiquote forms may be nested. Substitutions are made only for unquoted
components appearing at the same nesting level as the outermost backquote. The
nesting level increases by one inside each successive quasiquotation, and
decreases by one inside each unquotation.

    `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)
         ⇒  (a `(b ,(+ 1 2) ,(foo 4 d) e) f)

    (let ((name1 'x)
          (name2 'y))
      `(a `(b ,,name1 ,',name2 d) e))       ⇒  (a `(b ,x ,'y d) e)

The notations <code>`template</code> and (`quasiquote template`) are identical
in all respects. <code>`,expression</code> is identical to <code>`(unquote
expression)</code> and <code>`,@expression</code> is identical to
<code>`(unquote-splicing expression)</code>.

    (quasiquote (list (unquote (+ 1 2)) 4))
               ⇒  (list 3 4)

    '(quasiquote (list (unquote (+ 1 2)) 4))
               ⇒  `(list ,(+ 1 2) 4)
               _i.e.,_ (quasiquote (list (unquote (+ 1 2)) 4))

Unpredictable behavior can result if any of the symbols `quasiquote`,
`unquote`, or `unquote-splicing` appear in a _template_ in ways otherwise
than as described above.

## Macros ##


### (define-macro (_formals_) _template_) ###
### (defmacro (_formals_) _template_) ###

_The former is from standard Scheme and is the preferred name. The
latter is retained for backward compatibility with earlier versions of
GoLisp._

Create a named macro:

_formals_ is the same as in a procedure definition: a name followed by formal
parameters, if any. **NOTE** that the arguments to a macro invocation are
**not** evaluated, but are passed as is to the macro to do with as it wishes.

_template_ the template expression that is processed when the macro is invoked.
The result of evaluating the processed template expression becomes the value of
the macro's invocation. _template_ is typically (even always) a quasiquoted
expression using the formal parameter names for purposes of unquotiing in order
to fill in the template.

    (define-macro (double x)
       `(+ ,x ,x))

    (double 5) ⇒ 10

### (expand _name_ [_object_...]) ###

Expands the macro named by _name_, passing the evaluated sequence of _object_ as
arguments. **NOTE:** whereas invoking the macro (in the same way you invoke a
function) expands and evaluates, `expand` (as you would expect) only expands the
macro, resulting in the expanded template sexpr. This can then be evaluated as
desired.

    (define-macro (double x)
       `(+ ,x ,x))

    (expand double 5) ⇒ (+ 5 5)
       
## Sequencing ##

The `begin` special form is used to evaluate expressions in a particular order.

### (begin _expression_ ...) ###

The _expressions_ are evaluated sequentially from left to right, and the value of
the last _expression_ is returned. This expression type is used to sequence side
effects such as input and output. Keep in mind, `begin` does **not** create a nested lexical environment.

    (define x 0)
    (begin (set! x 5)
           (+ x 1))                 ⇒  6

    (begin (display "4 plus 1 equals ")
           (display (+ 4 1)))
                                    -|  4 plus 1 equals 5
                                    ⇒  unspecified

Often the use of `begin` is unnecessary, because many special forms already
support sequences of expressions (that is, they have an implicit `begin`). Some
of these special forms are:

* case
* cond
* define          ;"procedure define" only
* do
* lambda
* let
* let*
* letrec
     
### (-> _value_ _sexpr_|_symbol_...) ###

This creates a function chain. _value_ (evaluated first) is used as the first
argument to the first _sexpr_. The result of each _sexpr_ is used as the first
argument of the next, and the result of the final _sexpr_ is the value of the
`->` form. If a _sexpr_ would take a single argument (which would be provided
by the _value_ or the result of the previous _sexpr_, just the function name can
be used.

The form `(-> 0 a b c)` is equivalent to `(c (b (a 0)))`.

    (-> 1 (+ 3) (- 2))     ⇒ 2     ; (- (+ 1 3) 2)
    (-> 1 (+ 3) (- 2) str) ⇒ "2"   ; (str (- (+ 1 3) 2))

The major advantage of this form is avoiding having to create a sequence on
intermediate values/bindings purely to support the data flow. `->` lets you
create a pipeline that you can put one value into and get the final result out
the other end.

### (=> _value_ _sexpr_|_symbol_...) ###

This operates similarly to `->` with two differences:

1. _value_ (evaluated **once** at the beginning) is used as the initial argument
   to **each** function, and they are independent and do not pass results one to
   another.

2.  _value_ is the result of the form.

The expression

    (=> 1 a b c)

is equivalent to

    (begin
      (a 1)
      (b 1)
      (c 1)
      1)

and

    (=> (+ x 1) a b c)

is the same as

    (let ((y (+ x 1)))
      (a y)
      (b y)
      (c y)
      y)


## Conditionals ##

The behavior of the "conditional expressions" is determined by whether objects
are true or false. The conditional expressions count only `#f` as false. They
count everything else, including `#t`, pairs, symbols, numbers, strings,
vectors, and procedures as true.

In the descriptions that follow, we say that an object has "a true value" or "is
true" when the conditional expressions treat it as true, and we say that an
object has "a false value" or "is false" when the conditional expressions treat
it as false.

### (cond _clause_...) ###

Each _clause_ has this form:

    (predicate expression...)

where _predicate_ is any expression. The last _clause_ may be an "`else`
clause", which has the form:

    (else expression...)

A `cond` expression does the following:

1.  Evaluates the _predicate_ expressions of successive _clauses_ in order, until one
of the _predicates_ evaluates to a true value.

2.  When a _predicate_ evaluates to a true value, `cond` evaluates the
_expressions_ in the associated _clause_ in left to right order, and returns the
result of evaluating the last _expression_ in the _clause_ as the result of the
entire `cond` expression.
<br/><br/>If the selected _clause_ contains only the _predicate_ and no _expressions_, `cond`
returns the value of the _predicate_ as the result.

3. If all _predicates_ evaluate to false values, and there is no `else` clause,
the result of the conditional expression is unspecified; if there is an `else`
clause, `cond` evaluates its _expressions_ (left to right) and returns the value
of the last one.

        (cond ((> 3 2) 'greater)
            ((< 3 2) 'less))                  ⇒  greater

        (cond ((> 3 3) 'greater)
              ((< 3 3) 'less)
              (else 'equal))                    ⇒  equal

Normally, programs should not depend on the value of a `cond` expression that
has no `else` clause. However, some Scheme programmers prefer to write `cond`
expressions in which at least one of the _predicates_ is always true. In this
style, the final _clause_ is equivalent to an `else` clause.

GoLisp (and Scheme) supports an alternative clause syntax:

    (predicate => recipient)

where _recipient_ is an expression. If _predicate_ evaluates to a true value, then
_recipient_ is evaluated. Its value must be a procedure of one argument; this
procedure is then invoked on the value of the _predicate_.

    (cond ((assv 'b '((a 1) (b 2))) => cadr)
          (else #f))                        ⇒  2

### (case _key_ _clause_...) ###

_key_ may be any expression.  Each _clause_ has this form:

    ((object...) expression...)

No _object_ is evaluated, and all the _objects_ must be distinct. The last
_clause_ may be an "**else** clause", which has the form:

    (else expression...)

A `case` expression does the following:

1. Evaluates _key_ and compares the result with each _object_.

2. If the result of evaluating _key_ is equivalent (in the sense of `eqv?`) to
an _object_, `case` evaluates the _expressions_ in the corresponding _clause_
from left to right and returns the result of evaluating the last _expression_ in
the _clause_ as the result of the `case` expression.

3. If the result of evaluating _key_ is different from every _object_, and if
there's an `else` clause, `case` evaluates its _expressions_ and returns the
result of the last one as the result of the `case` expression. If there's no
`else` clause, `case` returns an unspecified result. Programs should not depend
on the value of a `case` expression that has no `else` clause.

For example,

    (case (* 2 3)
       ((2 3 5 7) 'prime)
       ((1 4 6 8 9) 'composite)) ⇒  composite

    (case (car '(c d))
       ((a) 'a)
       ((b) 'b))                 ⇒  unspecified

    (case (car '(c d))
       ((a e i o u) 'vowel)
       ((w y) 'semivowel)
       (else 'consonant))        ⇒  consonant

### (and _expression_...) ###

The _expressions_ are evaluated from left to right, and the value of the first
_expression_ that evaluates to a false value is returned. Any remaining
_expressions_ are not evaluated. If all the _expressions_ evaluate to true values,
the value of the last _expression_ is returned. If there are no _expressions_ then
`#t` is returned.

    (and (= 2 2) (> 2 1)) ⇒  #t
    (and (= 2 2) (< 2 1)) ⇒  #f
    (and 1 2 'c '(f g))   ⇒  (f g)
    (and)                 ⇒  #t

### (or _expression_...) ###

The _expressions_ are evaluated from left to right, and the value of the first
_expression_ that evaluates to a true value is returned. Any remaining _expressions_
are not evaluated. If all _expressions_ evaluate to false values, the value of the
last _expression_ is returned. If there are no _expressions_ then `#f` is returned.

    (or (= 2 2) (> 2 1))            ⇒  #t
    (or (= 2 2) (< 2 1))            ⇒  #t
    (or #f #f #f)                   ⇒  #f
    (or (memq 'b '(a b c)) (/ 3 0)) ⇒  (b c)

### (if _predicate_ _consequent_ [_alternative_]) ###
<p class="annotation">conditional.scm</p>

`if` is a macro based on `cond`.

_predicate_, _consequent_, and _alternative_ are expressions. An `if` expression is
evaluated as follows: first, _predicate_ is evaluated. If it yields a true value,
then _consequent_ is evaluated and its value is returned. Otherwise _alternative_ is
evaluated and its value is returned. If _predicate_ yields a false value and no
_alternative_ is specified, then the result of the expression is unspecified.

An `if` expression evaluates either _consequent_ or _alternative_, never both.
Programs should not depend on the value of an `if` expression that has no
_alternative_.

    (if (> 3 2) 'yes 'no)                   ⇒  yes
    (if (> 2 3) 'yes 'no)                   ⇒  no
    (if (> 3 2)
        (- 3 2)
        (+ 3 2))                            ⇒  1

### (when _predicate_ _expression_...) ###
<p class="annotation">conditional.scm</p>

`when` is a macro based on `cond`.

If _predicate_ evaluates to logically `true`, the sequence of _expresions_ is
evaluated and the result of the last one is the result of the `when` form,
otherwise 'nil' is the result.

    (when (> x 5)
      (write-line "greater")
      (+ x 2))

The above is equivalent to the following, but is simpler and clearer.

    (if (> x 5)
        (begin (write-line "greater")
               (+ x 2)))

### (unless _predicate_ _expression_...) ###
<p class="annotation">conditional.scm</p>

`unless` is a macro based on `cond`.

If _predicate_ evaluates to logically `false`, the sequence of _expresions_ is
evaluated and the result of the last one is the result of the `unless` form,
otherwise `nil` is the result.

    (unless (> x 5)
      (write-line "greater")
      (+ x 2))

The above is equivalent to the following, but is much simpler and clearer.

    (if (> x 5)
        ()
        (begin (write-line "greater")
               (+ x 2)))

## Iteration ##

The "iteration expressions" are: "named `let`" and `do`.  They are also
binding expressions, but are more commonly referred to as iteration
expressions.

### (let _name_ ((_variable_ _init_)...) _expression...) ###

GoLisp permits a variant on the syntax of *let* called "named `let`" which
provides a more general looping construct than `do`, and may also be used to
express recursions.

Named `let` has the same syntax and semantics as ordinary `let` except that _name_
is bound within the _expressions_ to a procedure whose formal arguments are the
_variables_ and whose body is the _expressions_. Thus the execution of the
_expressions_ may be repeated by invoking the procedure named by _name_.

GoLisp allows any of the _inits_ to be omitted, in which case the corresponding
_variables_ are unassigned.

Note: the following expressions are equivalent:

    (let name ((variable init)...)
      expression...)

    ((letrec ((name
               (lambda (variable...)
                 expression...)))
       name)
     init...)

Here is an example:

    (let loop
         ((numbers '(3 -2 1 6 -5))
          (nonneg '())
          (neg '()))
      (cond ((null? numbers)
             (list nonneg neg))
            ((>= (car numbers) 0)
             (loop (cdr numbers)
                   (cons (car numbers) nonneg)
                   neg))
            (else
             (loop (cdr numbers)
                   nonneg
                   (cons (car numbers) neg)))))

         ⇒  ((6 1 3) (-5 -2))

### (do ((_variable_ _init_ _step_)...) (_test_ _expression_...) _command_...) ###

`do` is an iteration construct. It specifies a set of variables to be bound,
how they are to be initialized at the start, and how they are to be updated on
each iteration. When a termination condition is met, the loop exits with a
specified result value.

`do` expressions are evaluated as follows: The _init_ expressions are
evaluated (in some unspecified order), the _variables_ are bound to fresh
locations, the results of the _init_ expressions are stored in the bindings of
the _variables_, and then the iteration phase begins.

Each iteration begins by evaluating _test_; if the result is false, then the
_command_ expressions are evaluated in order for effect, the _step_ expressions
are evaluated in some unspecified order, the _variables_ are bound to fresh
locations, the results of the _steps_ are stored in the bindings of the
_variables_, and the next iteration begins.

If _test_ evaluates to a true value, then the _expressions_ are evaluated from
left to right and the value of the last _expression_ is returned as the value of
the `do` expression. If no _expressions_ are present, then the value of the
`do` expression is the empty list (i.e. `nil`).

The region of the binding of a _variable_ consists of the entire `do`
expression except for the _inits_. It is an error for a _variable_ to appear
more than once in the list of `do` variables.

A _step_ may be omitted, in which case the effect is the same as if `(variable
init variable)` had been written instead of `(variable init)`.

    (do ((vec (make-vector 5))
          (i 0 (+ i 1)))
         ((= i 5) vec)
       (vector-set! vec i i))        ⇒  #(0 1 2 3 4)

    (let ((x '(1 3 5 7 9)))
       (do ((x x (cdr x))
            (sum 0 (+ sum (car x))))
           ((null? x) sum)))         ⇒  25


## Eval/Apply ##

### (apply _function_ _object_...) ###

Apply the function that results from evaluating _function_ to the argument list
resulting from evaluating each _object_.

Each initial _object_ can be any type of object, but the final one (and there
must be at least one _object_) must be a list.

    (apply + 1 2 '(3 4)) ⇒ 10
    (apply + '(1 2 3 4)) ⇒ 10

### (eval _expression_) ###

Evaluate _expression_ in the current environment.

    (eval '(+ 1 2 3 4)) ⇒ 10

### (definition-of _function_) ###

Fetch the definition of _function_. This returns an expression that can be
evaluated to define it. One use of this is to copy definitions to a source file.

    (define (square x)
      (* x x))

    (definition-of square) ⇒ (define (square x) (* x x))

    (define square (lambda (x)
                     (* x x)))

    (definition-of square) ⇒ (define square (lambda (x) (* x x)))


# Type checking #

### (atom? _object_) ###

Returns whether _object_ is an atom, i.e. a number, string, boolean, or symbol.

### (list? _object_) ###

Returns whether _object_ is a list.

### (pair? _object_) ###

Returns whether _object_ is a pair, i.e. a list or a dotted pair.

### (alist? _object_) ###

Returns whether _object_ is an association list.

### (vector? _object_) ###

Returns `#t` if `object` is a vector; otherwise returns `#f`.

### (nil? _object_) ###

### (null? _object_) ###

Returns whether _object_ is an empty list, i.e. the nil value.

### (notnil? _object_) ###

### (notnull? _object_) ###

Returns whether _object_ is not an empty list, i.e. the nil value.

### (symbol? _object_) ###

Returns whether _object_ is a symbol.

### (string? _object_) ###

Returns whether _object_ is a string.

### (number? _object_) ###

Returns whether _object_ is a number.

### (integer? _object_) ###

Returns whether _object_ is an integer.

### (float? _object_) ###

Returns whether _object_ is a floating point number.

### (function? _object_) ###

Returns whether _object_ is a function.

### (macro? _object_) ###

Returns whether _object_ is a macro.

### (frame? _object_) ###

Returns whether _object_ is a frame.

### (bytearray? _object_) ###

Returns whether _object_ is a bytearray.

### (port? _object_) ###

Returns whether _object_ is a port.

### (channel? _object_) ###

Returns whether _object_ is a channel.



## Numerical Operations

### (+ _number_...) ###

### (* _number_ ...) ###

These procedures return the sum or product of their arguments.

    (+ 3 4)                 ⇒  7
    (+ 3)                   ⇒  3
    (+)                     ⇒  0
    (* 4)                   ⇒  4
    (*)                     ⇒  1

### (- _number_...) ###

### (/ _number_...) ###

With two or more arguments, these procedures return the difference or quotient
of their arguments, associating to the left. With one argument, however, they
return the additive or multiplicative inverse of their argument.

    (- 3 4)                 ⇒  -1
    (- 3 4 5)               ⇒  -6
    (- 3)                   ⇒  -3
    (/ 3 4 5)               ⇒  0.15
    (/ 4)                   ⇒  0.25

Note that `/` always preforms floating point division. If the quotient is a
whole number it will be returned as an integer.

    (/ 12 5)                ⇒  2.4
    (/ 12 2.4)              ⇒  5

### (succ _integer_) ###

### (1+ _integer_) ###

Equivalent to `(+ integer 1)`.

### (pred _integer_) ###

### (-1+ _integer_) ###

Equivalent to `(- integer 1)`.

### (quotient _n1_ _n2)

### (remiander _n1_ _n2_)

### (modulo _n1_ _n2_)

These procedures implement number-theoretic (integer) division: for positive
integers _n1_ and _n2_, if _n3_ and _n4_ are integers such that

    n1 = (n2 * n3) + n4

    0 <= n4 < n2

then

    (quotient n1 n2)        ⇒  n3
    (remainder n1 n2)       ⇒  n4
    (modulo n1 n2)          ⇒  n4

for integers n1 and n2 with n2 not equal to 0,

    (= n1
       (+ (* n2 (quotient n1 n2))
          (remainder n1 n2)))
                            ⇒  #t

The value returned by `quotient` always has the sign of the product of its
arguments. `remainder` and `modulo` differ on negative arguments -- the
`remainder` always has the sign of the dividend, the `modulo` always has the
sign of the divisor:

    (modulo 13 4)           ⇒  1
    (remainder 13 4)        ⇒  1

    (modulo -13 4)          ⇒  3
    (remainder -13 4)       ⇒  -1

    (modulo 13 -4)          ⇒  -3
    (remainder 13 -4)       ⇒  1

    (modulo -13 -4)         ⇒  -1
    (remainder -13 -4)      ⇒  -1

The `%` function is an alias for `remainder`.

### (floor _number_) ###

Returns the greatest integer value less than or equal to `number`. `number` can
be an integer or float. Return value is a float.

    (floor 3.4)             ⇒ 3.0
    (floor -3.4)            ⇒ -4.0
    (floor 3)               ⇒ 3.0

### (ceiling _number_) ###

Returns the largest integer value greater than or equal to `number`. `number`
can be an integer or float. Return value is a float.

    (ceiling 3.4)           ⇒ 4.0
    (ceiling -3.4)          ⇒ -3.0
    (ceiling 3)             ⇒ 3.0

### (integer _number_)

Returns the integer value of _number_. If it is an integer, it is simply
returned. However, if it is a float the integer part is returned. 

    (integer 5)             ⇒ 5
    (integer 5.2)           ⇒ 5
    (integer -5.8)          ⇒ -5

### (float _number_)

Returns the float value of _number_. If it is a float, it is simply returned.
However, if it is an integer the corresponding float is returned.

    (float 5)               ⇒ 5.0

Note that converting a float to a string for printing using the format `%g` to
use the minimum number of characters so `5.0` will actually print as `5`.

### (number->string _number_ [_base_])

Converts _number_ (first converted to an integer) to a string, in the given
_base_. Allowed bases are 2, 8, 10, and 16. If the base is omitted, 10 is used.
No base prefixes (e.g. `0x` for base 16) are added.

    (number->string 42)    ⇒ "42"
    (number->string 42 2)  ⇒ "101010"
    (number->string 42 8)  ⇒ "52"
    (number->string 42 10) ⇒ "42"
    (number->string 42 16) ⇒ "2a"
    (number->string 42 15) ⇒ ERROR number->string: unsupported base 15

### (string->number _numeric-string_ [_base_])

Converts _numeric-string_ to an integer, in the given base. Allowed bases are 2,
8, 10, and 16. If the base is omitted, 10 is used. No base prefixes (e.g. `0x`
for base 16) are allowed. Specifying an unsupported base will result in `0`.

    (string->number "42")       ⇒ 42
    (string->number "101010" 2) ⇒ 42
    (string->number "52" 8)     ⇒ 42
    (string->number "42" 10)    ⇒ 42
    (string->number "2a" 16)    ⇒ 42
    (string->number "42" 15)    ⇒ ERROR number->string: unsupported base 15

## Comparisons

All comparison operations work with floating point numbers as well.

### (== _number1_ _number2_)

### (= _number1_ _number2_)

### (!= _number1_ _number2_) ###

### (/= _number1_ _number2_) ###

### (&lt; _number1_ _number2_ ...)

### (&gt; _number1_ _number2_ ...)

### (&lt;= _number1_ _number2_ ...)

### (&gt;= _number1_ _number2_ ...)

These procedures return `#t` if their arguments are (respectively):
equal (two alternatives), not equal (two alternatives), monotonically
increasing, monotonically decreasing, monotonically nondecreasing, or
monotonically nonincreasing. They return `#f` otherwise. Note that
`&lt;`, `&gt;`, `&lt;=`, and `&gt;=` can take more than 2 arguements.

### (zero? _number_)

### (positive? _number_)

### (negative? _number_)

### (odd? _number_)

### (even? _number_)

These numerical predicates test a number for a particular property, returning
`#t` or `#f`.

### (min _number_...) ###

### (min (_number_...)) ###

### (max _number_...) ###

### (max (_number_...)) ###

These procedures return the maximum or minimum of their arguments. Note that the
arguments can be a series of numbers or a list of numbers:

    (min 3 7 1 2)     ⇒ 1
    (min '(3 7 1 2))  ⇒ 1

### (log _number_) ###

This computes the natural logarithm of _number_ **(not the base ten logarithm)**. An integer argument will be converted to a float. The result is always a float.

# Equivalence Predicates #

A "predicate" is a procedure that always returns a boolean value (`#t` or `#f`).
An "equivalence predicate" is the computational analogue of a mathematical
equivalence relation (it is symmetric, reflexive, and transitive). Of the
equivalence predicates described in this section, `eq?` is the finest or most
discriminating, and `equal?` is the coarsest. `eqv?` is slightly less
discriminating than `eq?`.

### (eqv? _obj1_ _obj2_) ###

The `eqv?` procedure defines a useful equivalence relation on objects. Briefly,
it returns `#t` if _obj1_ and _obj2_ should normally be regarded as the same
object.

The `eqv?` procedure returns `#t` if:

* _obj1_ and _obj2_ are both `#t` or both `#f`.

* _obj1_ and _obj2_ are both interned symbols and

        (string=? (symbol->string obj1)
                  (symbol->string obj2))
            ⇒ #t

* _obj1_ and _obj2_ are both numbers, are numerically equal
according to the `=` procedure.

* both _obj1_ and _obj2_ are the empty list.

* _obj1_ and _obj2_ are procedures whose location tags are equal.

* _obj1_ and _obj2_ are pairs, vectors, strings, byte arrays, records, cells, or
weak pairs that denote the same locations in the store.

The `eqv?` procedure returns `#f` if:

* _obj1_ and _obj2_ are of different types.

* one of _obj1_ and _obj2_ is `#t` but the other is `#f`.

* _obj1_ and _obj2_ are symbols but

        (string=? (symbol->string obj1)
                  (symbol->string obj2))
           ⇒ #f

* _obj1_ and _obj2_ are numbers for which the `=` procedure returns
`#f`.

* one of _obj1_ and _obj2_ is the empty list but the other is not.

* _obj1_ and _obj2_ are procedures which have distinct underlying representations.

* _obj1_ and _obj2_ are pairs, vectors, strings, byte arrays, or frames that
 denote distinct locations.

Some examples:

    (eqv? 'a 'a)                 ⇒  #t
    (eqv? 'a 'b)                 ⇒  #f
    (eqv? 2 2)                   ⇒  #t
    (eqv? '() '())               ⇒  #t
    (eqv? 100000000 100000000)   ⇒  #t
    (eqv? (cons 1 2) (cons 1 2)) ⇒  #f
    (eqv? (lambda () 1)
          (lambda () 2))         ⇒  #f
    (eqv? #f 'nil)               ⇒  #f
    (let ((p (lambda (x) x)))
      (eqv? p p))                ⇒  #t

The following examples illustrate how GoLisp behaves in cases where MIT Scheme's
rules do not fully specify the behavior of `eqv?'.

    (eqv? "" "")                 ⇒  #f
    (eqv? '#() '#())             ⇒  #f
    (eqv? (lambda (x) x)
          (lambda (x) x))        ⇒  #f
    (eqv? (lambda (x) x)
          (lambda (y) y))        ⇒  #f

Objects of distinct types must never be regarded as the same object.

Since it is an error to modify constant objects (those returned by literal
expressions), the implementation may share structure between constants where
appropriate. Thus the value of `eqv?` on constants is sometimes unspecified,
however, the following cases hold.

    (let ((x '(a)))
      (eqv? x x))                ⇒  #t
    (eqv? '(a) '(a))             ⇒  #f
    (eqv? "a" "a")               ⇒  #f
    (eqv? '(b) (cdr '(a b)))     ⇒  #f


### (eq? _obj1_ _obj2_) ###

`eq?` is similar to `eqv?` except that in some cases it is capable of
discerning distinctions finer than those detectable by `eqv?`.

`eq?` and `eqv?` are guaranteed to have the same behavior on symbols,
booleans, the empty list, pairs, records, and non-empty strings and vectors.
`eq?`'s behavior on numbers and characters is implementation-dependent, but it
will always return either true or false, and will return true only when `eqv?`
would also return true. `eq?` may also behave differently from `eqv?` on
empty vectors and empty strings.

    (eq? 'a 'a)                  ⇒  #t
    (eq? '(a) '(a))              ⇒  #f
    (eq? (list 'a) (list 'a))    ⇒  #f
    (eq? "a" "a")                ⇒  #t
    (eq? "" "")                  ⇒  #t
    (eq? '() '())                ⇒  #t
    (eq? 2 2)                    ⇒  #t
    (eq? car car)                ⇒  #t
    (let ((n (+ 2 3)))
      (eq? n n))                 ⇒  #t
    (let ((x '(a)))
      (eq? x x))                 ⇒  #t
    (let ((x '#()))
      (eq? x x))                 ⇒  #t
    (let ((p (lambda (x) x)))
      (eq? p p))                 ⇒  #t


### (equal? _obj1_ _obj2_) ###

`equal?` recursively compares the contents of pairs, vectors, and strings,
applying `eqv?` on other objects such as numbers, symbols, and booleans. A
rule of thumb is that objects are generally `equal?` if they print the same.
`equal?` may fail to terminate if its arguments are circular data structures.

    (equal? 'a 'a)               ⇒  #t
    (equal? '(a) '(a))           ⇒  #t
    (equal? '#(a) '#(a))         ⇒  #t
    (equal? '(a (b) c)
            '(a (b) c))          ⇒  #t
    (equal? "abc" "abc")         ⇒  #t
    (equal? 2 2)                 ⇒  #t
    (equal? (make-vector 5 'a)
            (make-vector 5 'a))  ⇒  #t
    (equal? (lambda (x) x)
            (lambda (y) y))      ⇒  #f


### (neqv? _object_ _object_) ###

### (neq? _object_ _object_) ###

### (nequal? _object_ _object_) ###

Each is the logical negation of the corresponding equivalence predicate.

# Logical #

### (boolean=? _object1_ _object2_) ###
<p class="annotation">misc.scm</p>

Returns whether _object1_ and _object2_ are both truthy or are both falsy.

### (boolean/and _object_...) ###
<p class="annotation">misc.scm</p>

Each _object_ is evaluated; if all of the evaluated _objects_ are truthy, returns the value of the final one

### (boolean/or _object_...) ###
<p class="annotation">misc.scm</p>

Each _object_ is evaluated; if any of the evaluated _objects_ are truthy, returns the first one that is

### (not _object_)

### (false? _object_) ###
<p class="annotation">misc.scm</p>

Returns the boolean negation of the argument.

    (not #t)    ⇒ #f
    (false? #f) ⇒ #t


# Binary #

### (binary-and _int-1_ _int-2_)

Performs a bitwise AND of _int-1_ and _int-2_, returning the result.

    (number->string (binary-and 0xaa 0x0f) 16) ⇒ "a"
    (number->string (binary-and 0xaa 0xf0) 16) ⇒ "a0"

### (binary-or _int-1_ _int-2_)

Performs a bitwise OR of _int-1_ and _int-2_, returning the result.

    (number->string (binary-or 0xaa 0x0f) 16) ⇒ "af"
    (number->string (binary-or 0xaa 0xf0) 16) ⇒ "fa"

### (binary-not _int_)

Performs a bitwise NOT of _int_, returning the result.

    (number->string (binary-not 0x000000aa) 16) ⇒ "ffffff55"

### (left-shift _int_ _count_)

Shifts _int_ left by _count_ bits, returning the result.

    (number->string (left-shift (string->number "10101010" 2) 1) 2) ⇒ "101010100"
    (number->string (left-shift (string->number "10101010" 2) 3) 2) ⇒ "10101010000"

### (right-shift _int_ _count_)

Shifts _int_ right by _count_ bits, returning the result.

    (number->string (right-shift (string->number "10000" 2) 1) 2) ⇒ "1000"
    (number->string (right-shift (string->number "10000" 2) 4) 2) ⇒ "1"

# Characters #

GoLisp has minimal support for characters: just that required for basic string
manipulation. this may be expanded in the future.

## External Representation of Characters ##

Characters are written using the notation `#\CHARACTER` or
`#\CHARACTER-NAME`.  For example:

     #\a                     ; lowercase letter
     #\A                     ; uppercase letter
     #\(                     ; left parenthesis
     #\space                 ; the space character
     #\newline               ; the newline character

Case is significant in `#\CHARACTER`, but not in `#\CHARACTER-NAME`. If
CHARACTER in `#\CHARACTER` is a letter, _CHARACTER_ must be followed by a
delimiter character such as a space or closing parenthesis, bracket, or brace.
Characters written in the `#\' notation are self-evaluating; you don't need to
quote them.

The following CHARACTER-NAMEs are supported, shown here with their
ASCII equivalents:

     Character Name          ASCII Name
     --------------          ----------

     altmode                 ESC
     backspace               BS
     esc                     ESC
     linefeed                LF
     page                    FF
     return                  CR
     rubout                  DEL
     space
     tab                     HT


# Strings #

A "string" is a immutable sequence of characters.

A string is written as a sequence of characters enclosed within
double quotes `" "`.  To include a double quote inside a string, precede
the double quote with a backslash `\' (escape it), as in

     "The word \"recursion\" has many meanings."

The printed representation of this string is

     The word "recursion" has many meanings.

To include a backslash inside a string, precede it with another
backslash; for example,

     "Use #\\Control-q to quit."

The printed representation of this string is

     Use #\Control-q to quit.

The effect of a backslash that doesn't precede a double quote or backslash is
unspecified in standard Scheme, but GoLisp specifies the effect for
three other characters: `\t`, `\n`, and `\f`. These escape sequences are
respectively translated into tab, newline, and page characters. whose ISO-8859-1
code is those digits.

If a string literal is continued from one line to another, the string will
contain the newline character at the line break. Standard Scheme does not
specify what appears in a string literal at a line break.

The "length" of a string is the number of characters that it contains. This
number is a non-negative integer that is established when the string is created.
Each character in a string has an "index", which is a number that indicates the
character's position in the string. The index of the first (leftmost) character
in a string is 0, and the index of the last character is one less than the
length of the string. The "valid indexes" of a string are the non-negative
integers less than the length of the string.

    0 <= start <= end <= (string-length string)

## Construction of Strings ##

### (make-string _k_ [_char_]) ###
<p class="annotation">strings.scm</p>

Returns a newly allocated string of length _k_. If you specify _char_, all
elements of the string are initialized to _char_, otherwise the contents of the
string are unspecified.

    (make-string 10 #\x)              ⇒  "xxxxxxxxxx"

### (string _char_...) ###
<p class="annotation">strings.scm</p>

Returns a newly allocated string consisting of the specified
characters.  The arguments should be single character strings.

    (string "a")                         ⇒  "a"
    (string "a" "b" #\c)                 ⇒  "abc"
    (string #\a #\space #\b #\space #\c) ⇒  "a b c"
    (string)                             ⇒  undefined

### (list->string _char-list_) ###

_char-list_ must be a list of strings. `list->string` returns a newly
allocated string formed by concatenating the elements of _char-list_. This is
equivalent to `(apply string _char-list_)`. The inverse of this operation is
`string->list`.

    (list->string '(#\a #\b))       ⇒  "ab"
    (string->list "Hello")          ⇒  (#\H #\e #\l #\l #\o)

### (string-copy _string_) ###
<p class="annotation">strings.scm</p>

Returns a newly allocated copy of _string_.

## Selecting String Components ##

### (string? _object_) ###

Returns `#t` if _object_ is a string; otherwise returns `#f`.

    (string? "Hi")                  ⇒  #t
    (string? 'Hi)                   ⇒  #f

### (string-length _string_) ###

Returns the length of _string_ as an non-negative integer.

    (string-length "")              ⇒  0
    (string-length "The length")    ⇒  10

### (string-null? _string_) ###
<p class="annotation">strings.scm</p>

Returns `#t` if _string_ has zero length; otherwise returns `#f`.

    (string-null? "")               ⇒  #t
    (string-null? "Hi")             ⇒  #f

### (string-ref _string_ _k_) ###

Returns character _k_ of _string_.  _k_ must be a valid index of _string_.

    (string-ref "Hello" 1)          ⇒  #\e
    (string-ref "Hello" 5)          ERROR 5 not in correct range

### (string-set! _string_ _k_ _char_) ###

Stores _char_ (a single character string) in element _k_ of _string_ and returns
an unspecified value. _k_ must be a valid index of _string_.

    (define s "Dog")              ⇒  "Dog"
    (string-set! s 0 #\L)         ⇒  "Log"
    s                             ⇒  "Log"
    (string-set! s 3 #\t)         ERROR 3 not in correct range


## Comparison of Strings ##

### (string=? _string1_ _string2_)###
<p class="annotation">strings.scm</p>

### (substring=? _string1_ _start1_ _end1_ _string2_ _start2_ _end2_) ###
<p class="annotation">strings.scm</p>

### (string-ci=? _string1_ _string2_) ###
<p class="annotation">strings.scm</p>

### (substring-ci=?_ string1_ _start1_ _end1_ _string2_ _start2_ _end2_) ###
<p class="annotation">strings.scm</p>

Returns `#t` if the two strings (substrings) are the same length and contain the
same characters in the same (relative) positions; otherwise returns `#f`.
`string-ci=?` and `substring-ci=?` don't distinguish uppercase and lowercase
letters, but `string=?` and `substring=?` do.

    (string=? "PIE" "PIE")                  ⇒  #t
    (string=? "PIE" "pie")                  ⇒  #f
    (string-ci=? "PIE" "pie")               ⇒  #t
    (substring=? "Alamo" 1 3 "cola" 2 4)    ⇒  #t ; compares "la"

### (string<? _string1_ _string2_) ###
<p class="annotation">strings.scm</p>

### (substring<?_ string1_ _start1_ _end1_ _string2_ _start2_ _end2_) ###
<p class="annotation">strings.scm</p>

### (string-ci<? _string1_ _string2_) ###
<p class="annotation">strings.scm</p>

### (substring-ci<?_ string1_ _start1_ _end1_ _string2_ _start2_ _end2_) ###
<p class="annotation">strings.scm</p>
 
### (string>? _string1_ _string2_) ###
<p class="annotation">strings.scm</p>

### (substring>?_ string1_ _start1_ _end1_ _string2_ _start2_ _end2_) ###
<p class="annotation">strings.scm</p>

### (string-ci>? _string1_ _string2_) ###
<p class="annotation">strings.scm</p>

### (substring-ci>?_ string1_ _start1_ _end1_ _string2_ _start2_ _end2_) ###
<p class="annotation">strings.scm</p>
 
### (string<=? _string1_ _string2_) ###
<p class="annotation">strings.scm</p>

### (substring<=?_ string1_ _start1_ _end1_ _string2_ _start2_ _end2_) ###
<p class="annotation">strings.scm</p>

### (string-ci<=? _string1_ _string2_) ###
<p class="annotation">strings.scm</p>

### (substring-ci<=?_ string1_ _start1_ _end1_ _string2_ _start2_ _end2_) ###
<p class="annotation">strings.scm</p>

### (string>=? _string1_ _string2_) ###
<p class="annotation">strings.scm</p>

### (substring>=?_ string1_ _start1_ _end1_ _string2_ _start2_ _end2_) ###
<p class="annotation">strings.scm</p>

### (string-ci>=? _string1_ _string2_) ###
<p class="annotation">strings.scm</p>

### (substring-ci>=?_ string1_ _start1_ _end1_ _string2_ _start2_ _end2_) ###
<p class="annotation">strings.scm</p>

These procedures compare strings (substrings) according to the order of the
characters they contain. The arguments are compared using a lexicographic (or
dictionary) order. If two strings differ in length but are the same up to the
length of the shorter string, the shorter string is considered to be less than
the longer string.

    (string<? "cat" "dog")          ⇒  #t
    (string<? "cat" "DOG")          ⇒  #f
    (string-ci<? "cat" "DOG")       ⇒  #t
    (string>? "catkin" "cat")       ⇒  #t ; shorter is lesser

### (string-compare _string1_ _string2_ _if-eq_ _if-lt_ _if-gt_) ###

### (string-compare-ci _string1_ _string2_ _if-eq_ _if-lt_ _if-gt_) ###
 
_if-eq_, _if-lt_, and _if-gt_ are procedures of no arguments (thunks). The two strings
are compared; if they are equal, _if-eq_ is applied, if _string1_ is less than
_string2_, _if-lt_ is applied, else if _string1_ is greater than _string2_, _if-gt_ is
applied. The value of the procedure is the value of the thunk that is applied.

`string-compare` distinguishes uppercase and lowercase letters;
`string-compare-ci` does not.

    (define (cheer) (display "Hooray!"))
    (define (boo)   (display "Boo-hiss!"))
    (string-compare "a" "b"  cheer  (lambda() 'ignore)  boo)
            -|  Hooray!
            ⇒  unspecified

In GoLisp, only `string-compare` and `string-compare-ci` are available by
default. If you want the other comparison functions you need to `(load
"lisp/strings.scm")`.

## Alphabetic Case in Strings ##

### (string-capitalized? _string_) ###

### (substring-capitalized? _string_ _start_ _end_) ###
<p class="annotation">strings.scm</p>

These procedures return `#t` if the first word in the string (substring) is
capitalized, and any subsequent words are either lower case or capitalized.
Otherwise, they return `#f`. A word is defined as a non-null contiguous sequence
of alphabetic characters, delimited by non-alphabetic characters or the limits
of the string (substring). A word is capitalized if its first letter is upper
case and all its remaining letters are lower case.

    (map string-capitalized? '(""    "A"    "art"  "Art"  "ART"))
                           ⇒ (#f    #t     #f     #t     #f)

### (string-upper-case? _string_) ###

### (substring-upper-case? _string_ _start_ _end_) ###
<p class="annotation">strings.scm</p>

### (string-lower-case? _string_) ###

### (substring-lower-case? _string_ _start_ _end_) ###
<p class="annotation">strings.scm</p>

These procedures return `#t` if all the letters in the string (substring) are of
the correct case, otherwise they return `#f`. The string (substring) must
contain at least one letter or the procedures return `#f`.

    (map string-upper-case?  '(""    "A"    "art"  "Art"  "ART"))
                           ⇒ (#f    #t     #f     #f     #t)

### (string-capitalize _string_) ###

### (string-capitalize! _string_) ###

### (substring-capitalize! _string_ _start_ _end_) ###

`string-capitalize` returns a newly allocated copy of _string_ in which the
first alphabetic character is uppercase and the remaining alphabetic characters
are lowercase. For example, `"abcDEF"` becomes `"Abcdef"`.
`string-capitalize!` is the destructive version of `string-capitalize`: it
alters _string_ and returns an unspecified value. `substring-capitalize!`
destructively capitalizes the specified part of _string_.

### (string-downcase _string_) ###

### (string-downcase! _string_) ###

### (substring-downcase! _string_ _start_ _end_) ###

`string-downcase` returns a newly allocated copy of _string_ in which all
uppercase letters are changed to lowercase. `string-downcase!` is the
destructive version of `string-downcase`: it alters _string_ and returns an
unspecified value. `substring-downcase!` destructively changes the case of the
specified part of _string_.

    (define str "ABCDEFG")          ⇒  unspecified
    (substring-downcase! str 3 5)   ⇒  "ABCdeFG"
    str                             ⇒  "ABCdeFG"

### (string-upcase _string_) ###

### (string-upcase! _string_) ###

### (substring-upcase! _string_ _start_ _end_) ###

`string-upcase` returns a newly allocated copy of _string_ in which all
lowercase letters are changed to uppercase. `string-upcase!` is the
destructive version of `string-upcase`: it alters _string_ and returns an
unspecified value. `substring-upcase!` destructively changes the case of the
specified part of _string_.

## Cutting and Pasting Strings ##

### (string-split _string_ _separator_) ###

Splits _string_ into a list of substrings that are separated by _separator_.

    (string-split "1-2-3" "-")) ⇒ ("1" "2" "3")

### (string-join _strings_ _separator_) ###

Joins the list of _strings_ into a single string by interposing _separator_.

    (string-join '("1" "2" "3") "-") ⇒ "1-2-3"

### (string-append _string_...) ###
<p class="annotation">strings.scm</p>

Returns a newly allocated string made from the concatenation of the given
strings.

    (string-append)                 ⇒  undefined
    (string-append "*" "ace" "*")   ⇒  "*ace*"
    (string-append "" "" "")        ⇒  ""
    (eqv? str (string-append str))  ⇒  #f ; newly allocated

### (substring _string_ _start_ _end_) ###

Returns a newly allocated string formed from the characters of _string_ beginning
with index _start_ (inclusive) and ending with _end_ (exclusive).

    (substring "" 0 0)              ⇒ ""
    (substring "arduous" 2 5)       ⇒ "duo"
    (substring "arduous" 2 8)       ERROR 8 not in correct range

    (define (string-copy s)
      (substring s 0 (string-length s)))

### (string-head _string_ _end_) ###
<p class="annotation">strings.scm</p>

Returns a newly allocated copy of the initial substring of _string_,
up to but excluding _end_.  It could have been defined by:

    (define (string-head string end)
      (substring string 0 end))

    (string-head "uncommon" 2)      ⇒ "un"

### (string-tail _string_ _start_) ###
<p class="annotation">strings.scm</p>

Returns a newly allocated copy of the final substring of _string_, starting at
index _start_ and going to the end of _string_. It could have been defined by:

    (define (string-tail string start)
      (substring string start (string-length string)))

    (string-tail "uncommon" 2)      ⇒  "common"

### (string-pad-left _string_ _k_ [_char_]) ###
<p class="annotation">strings.scm</p>

### (string-pad-right _string_ _k_ [_char_]) ###
<p class="annotation">strings.scm</p>

These procedures return a newly allocated string created by padding _string_ out
to length _k_, using _char_. If _char_ is not given, it defaults to `#\space`.
If _k_ is less than the length of _string_, the resulting string is a truncated
form of _string_. `string-pad-left` adds padding characters or truncates from
the beginning of the string (lowest indices), while `string-pad-right` does so
at the end of the string (highest indices).

    (string-pad-left "hello" 4)             ⇒  "ello"
    (string-pad-left "hello" 8)             ⇒  "   hello"
    (string-pad-left "hello" 8 #\*)         ⇒  "***hello"
    (string-pad-right "hello" 4)            ⇒  "hell"
    (string-pad-right "hello" 8)            ⇒  "hello   "
    (string-pad-right "hello" 8 #\*)        ⇒  "hello***"

### (string-trim _string_ [_char-set_]) ###

### (string-trim-left _string_ [_char-set_]) ###

### (string-trim-right _string_ [_char-set_]) ###

Returns a newly allocated string created by removing all characters that are not
in _char-set_ from: `string-trim` both ends of _string_; `string-trim-left` the
beginning of _string_; or `string-trim-right` the end of _string_. _char-set_
defaults to `char-set:not-whitespace`.

    (string-trim "  in the end  ")          ⇒  "in the end"
    (string-trim "              ")          ⇒  ""
    (string-trim "100th" char-set:numeric)  ⇒  "100"
    (string-trim-left "-.-+-=-" (char-set #\+))
                                            ⇒  "+-=-"
    (string-trim "but (+ x y) is" (char-set #\( #\)))
                                            ⇒  "(+ x y)"


## Regexp Support ##

There is some preliminary support for regular expressions.

### (re-string-match-go _regexp_ _string_) ###

This matches regexp against the respective string,
returning #f for no match, or a list of strings (see below) if
the match succeeds.

When a successful match occurs, the above procedure returns
a list of strings. Each string
corresponds to an instance of the regular-expression grouping
operator ‘\(’. Additionally, the first string corresponds to the entire
substring matching the regular expression.

Note that this is different from the Scheme matching procedure.

# Lists #

A "pair" (sometimes called a "dotted pair") is a data structure with two fields
called the "car" and "cdr" fields (for historical reasons). Pairs are created by
the procedure `cons`. The car and cdr fields are accessed by the procedures
`car` and `cdr`. The car and cdr fields are assigned by the procedures
`set-car!` and `set-cdr!`.

Pairs are used primarily to represent "lists".  A list can be
defined recursively as either the empty list or a pair whose cdr is a
list.  More precisely, the set of lists is defined as the smallest set
X such that

* The empty list is in X.

* If LIST is in X, then any pair whose cdr field contains LIST is also in X.

The objects in the car fields of successive pairs of a list are the "elements"
of the list. For example, a two-element list is a pair whose car is the first
element and whose cdr is a pair whose car is the second element and whose cdr is
the empty list. The "length" of a list is the number of elements, which is the
same as the number of pairs. The "empty list" is a special object of its own
type (it is not a pair); it has no elements and its length is zero.

The most general notation (external representation) for GoLisp pairs is the
"dotted" notation `(C1 . C2)` where C1 is the value of the car field and C2 is
the value of the cdr field. For example, `(4 . 5)` is a pair whose car is `4`
and whose cdr is `5`. Note that `(4 . 5)` is the external representation of a
pair, not an expression that evaluates to a pair.

A more streamlined notation can be used for lists: the elements of the list are
simply enclosed in parentheses and separated by spaces. The empty list is
written `()`. For example, the following are equivalent notations for a list of
symbols:

     (a b c d e)
     (a . (b . (c . (d . (e . ())))))

Whether a given pair is a list depends upon what is stored in the cdr field.
When the `set-cdr!` procedure is used, an object can be a list one moment and
not the next:

     (define x (list 'a 'b 'c))
     (define y x)
     y                                       ⇒ (a b c)
     (list? y)                               ⇒ #t
     (set-cdr! x 4)                          ⇒ (a . 4)
     x                                       ⇒ (a . 4)
     (eqv? x y)                              ⇒ #t
     y                                       ⇒ (a . 4)
     (list? y)                               ⇒ #f
     (set-cdr! x x)                          ⇒ <Unprintable looping pair structure>
     (list? y)                               ⇒ #f

A chain of pairs that doesn't end in the empty list is called an "improper
list". Note that an improper list is not a list. The list and dotted notations
can be combined to represent improper lists, as the following equivalent
notations show:

     (a b c . d)
     (a . (b . (c . d)))

Within literal expressions and representations of objects read by the `read`
procedure, the forms `'DATUM`, <code>`DATUM</code>, `,DATUM`, and `,@DATUM`
denote two-element lists whose first elements are the symbols `quote`,
`quasiquote`, `unquote`, and `unquote-splicing`, respectively. The second
element in each case is DATUM. This convention is supported so that arbitrary
GoLisp programs may be represented as lists. Among other things, this permits
the use of the `read` procedure to parse Scheme programs.

## Pairs ##

This section describes the simple operations that are available for constructing
and manipulating arbitrary graphs constructed from pairs.

### (pair? _object_) ###

Returns `#t` if _object_ is a pair; otherwise returns `#f`.

    (pair? '(a . b))                        ⇒ #t
    (pair? '(a b c))                        ⇒ #t
    (pair? '())                             ⇒ #f
    (pair? '#(a b))                         ⇒ #f

### (cons _obj1_ _obj2_) ###

Returns a newly allocated pair whose car is _obj1_ and whose cdr is _obj2_. The
pair is guaranteed to be different (in the sense of `eqv?`) from every
previously existing object.

    (cons 'a '())                           ⇒ (a)
    (cons '(a) '(b c d))                    ⇒ ((a) b c d)
    (cons "a" '(b c))                       ⇒ ("a" b c)
    (cons 'a 3)                             ⇒ (a . 3)
    (cons '(a b) 'c)                        ⇒ ((a b) . c)

### (car _pair_) ###

Returns the contents of the car field of _pair_. Note that taking the `car` of the empty list results in the empty list.

    (car '(a b c))                          ⇒ a
    (car '((a) b c d))                      ⇒ (a)
    (car '(1 . 2))                          ⇒ 1
    (car '())                               ⇒ ()

### (cdr _pair_) ###

Returns the contents of the cdr field of _pair_. Note that taking the `cdr` of the empty list results in the empty list.

    (cdr '((a) b c d))                      ⇒ (b c d)
    (cdr '(1 . 2))                          ⇒ 2
    (cdr '())                               ⇒ ()

### (set-car! _pair_ _object_) ###

Stores _object_ in the car field of _pair_. The value returned by `set-car!`
is unspecified.

    (define (f) (list 'not-a-constant-list))
    (define (g) '(constant-list))
    (set-car! (f) 3)                        ⇒ unspecified
    (set-car! (g) 3)                        ERROR Illegal datum

### (set-cdr! _pair_ _object_) ###

Stores _object_ in the cdr field of _pair_. The value returned by `set-cdr!`
is unspecified.

### (set-nth! _n_ _list_ _new-value_) ###

Set the `car` pointer of the nth cons cell of _list_. Numbering starts at 1.

    (define a '(1 2 3 4))
    (set-nth! 3 a 0)
    a ⇒ (1 2 0 4)

### (caar _pair_) ###

### (cadr _pair_) ###

### (cdar _pair_) ###

### (cddr _pair_) ###

### (caaar _pair_) ###

### (caadr _pair_) ###

### (cadar _pair_) ###

### (caddr _pair_) ###

### (cdaar _pair_) ###

### (cdadr _pair_) ###

### (cddar _pair_) ###

### (cdddr _pair_) ###

### (caaaar _pair_) ###

### (caaadr _pair_) ###

### (caadar _pair_) ###

### (caaddr _pair_) ###

### (cadaar _pair_) ###

### (cadadr _pair_) ###

### (caddar _pair_) ###

### (cadddr _pair_) ###

### (cdaaar _pair_) ###

### (cdaadr _pair_) ###

### (cdadar _pair_) ###

### (cdaddr _pair_) ###

### (cddaar _pair_) ###

### (cddadr _pair_) ###

### (cdddar _pair_) ###

### (cddddr _pair_) ###

These procedures are compositions of `car` and `cdr'; for example, `caddr`
could be defined by

    (define caddr (lambda (x) (car (cdr (cdr x)))))

### (general-car-cdr _object_ _path_) ###

This procedure is a generalization of `car` and `cdr`. _path_ encodes a
particular sequence of `car` and `cdr` operations, which `general-car-cdr`
executes on _object_. _path_ is a non-negative integer that encodes the
operations in a bitwise fashion: a zero bit represents a `cdr` operation, and
a one bit represents a `car`. The bits are executed LSB to MSB, and the most
significant one bit, rather than being interpreted as an operation, signals the
end of the sequence.

For example, the following are equivalent:

    (general-car-cdr OBJECT #b1011)
    (cdr (car (car OBJECT)))

Here is a partial table of path/operation equivalents:

    #b10    cdr
    #b11    car
    #b100   cddr
    #b101   cdar
    #b110   cadr
    #b111   caar
    #b1000  cdddr

### (copy _object_) ###

This copies an arbitrary _object_, recursively if it is made from pairs.

## Construction of Lists ##

### (list _object_...) ###

Returns a list of its arguments.

    (list 'a (+ 3 4) 'c)                    ⇒ (a 7 c)
    (list)                                  ⇒ ()

These expressions are equivalent:

    (list OBJ1 OBJ2 ... OBJN)
    (cons OBJ1 (cons OBJ2 ... (cons OBJN '()) ...))

### (make-list k [element]) ###

This procedure returns a newly allocated list of length _k_, whose elements are
all _element_. If _element_ is not supplied, it defaults to the empty list.

    (make-list 4 'c)                        ⇒ (c c c c)

### (cons* object object ...) ###

`cons*` is similar to `list`, except that `cons*` conses together the
last two arguments rather than consing the last argument with the empty list. If
the last argument is not a list the result is an improper list. If the last
argument is a list, the result is a list consisting of the initial arguments and
all of the items in the final argument. If there is only one argument, the
result is the argument.

    (cons* 'a 'b 'c)                        ⇒ (a b . c)
    (cons* 'a 'b '(c d))                    ⇒ (a b c d)
    (cons* 'a)                              ⇒ a

These expressions are equivalent:

    (cons* OBJ1 OBJ2 ... OBJN-1 OBJN)
    (cons OBJ1 (cons OBJ2 ... (cons OBJN-1 OBJN) ...))

### (make-initialized-list k init-proc) ###

Returns a _K_-element list. Element I of the list, where 0 <= I < _k_, is
produced by `(_init-proc_ I)`. No guarantee is made about the dynamic order in
which _init-proc_ is applied to these indices.

    (make-initialized-list 4 (lambda (x) (* x x))) ⇒ (0 1 4 9)

### (list-copy list) ###

Returns a newly allocated copy of _list_. This copies each of the pairs
comprising _list_. This could have been defined by

    (define (list-copy list)
      (if (null? list)
          '()
          (cons (car list)
                (list-copy (cdr list)))))

### (iota count [start [step]]) ###
<p class="annotation">lists.scm</p>

Returns a list containing the elements

    (START START+STEP ... START+(COUNT-1)*STEP)

_count_ must be a non-negative integer, while _start_ and _step_ can be
any numbers. The _start_ and _step_ parameters default to 0 and 1, respectively.

    (iota 5)          ⇒  (0 1 2 3 4)
    (iota 5 0 -0.1)   ⇒  (0 -0.1 -0.2 -0.3 -0.4)

### (interval _hi_) ###

### (interval _lo_ _hi_) ###

### (interval _lo_ _hi_ _step_) ###

The first form creates a list of numbers from 1 to `hi`, inclusive. `hi`
**must** be a positive integer.

    (interval 5) ⇒ (1 2 3 4 5)
    (interval 2) ⇒ (1 2)

The second form creates a list of numbers from `lo` to `hi`, inclusive, stepping
by 1. If `lo` > `hi`, a step of -1 is used.

    (interval 1 5) ⇒ (1 2 3 4 5)
    (interval -2 2) ⇒ (-2 -1 0 1 2)
    (interval 5 1) ⇒ (5 4 3 2 1)
    (interval 2 -2) ⇒ (2 1 0 -1 -2)

The third form creates a list of numbers from `lo` to `hi`, inclusive (if
possible), `step` apart. `step` **must** be non-zero and it's sign must match
the ordering of `lo` and `hi`. I.e. if `lo` > `hi`, `step` must be negative,
otherwise positive.

    (interval 1 5 2) ⇒ (1 3 5)
    (interval 1 8 2) ⇒ (1 3 5 7)
    (interval -2 2 2) ⇒ (-2 0 2)
    (interval 2 -2 -2) ⇒ (2 0 -2)
    (interval 5 1 -2) ⇒ (5 3 1)
    (interval -1 -8 -2) ⇒ (-1 -3 -5 -7)

### (vector->list _vector_) ###

### (subvector->list _vector_ _start_ _end_) ###
<p class="annotation">lists.scm</p>

`vector->list` returns a newly allocated list of the elements of _vector_.
`subvector->list` returns a newly allocated list of the elements of the given
subvector. The inverse of `vector->list` is `list->vector`.

    (vector->list '#(dah dah didah))        ⇒ (dah dah didah)

### (string->list string) ###

### (substring->list string start end) ###
<p class="annotation">lists.scm</p>

`string->list` returns a newly allocated list of the character elements of
_string_. `substring->list` returns a newly allocated list of the character
elements of the given substring. The inverse of `string->list` is
`list->string`.

    (string->list "abcd")                   ⇒ (#\a #\b #\c #\d)
    (substring->list "abcdef" 1 3)          ⇒ (#\b #\c)

## Selecting List Components ##

### (list? _object_) ###

Returns `#t` if _object_ is a list, otherwise returns `#f`. By definition, all
lists have finite length and are terminated by the empty list. This procedure
returns an answer even for circular structures.

Any _object_ satisfying this predicate will also satisfy exactly one of `pair?`
or `null?`.

    (list? '(a b c))                        ⇒ #t
    (list? '())                             ⇒ #t
    (list? '(a . b))                        ⇒ #f
    (let ((x (list 'a)))
      (set-cdr! x x)
      (list? x))                            ⇒ #f

### (circular-list? _object_) ###

Returns `#t` if _object_ is a circular list, otherwise returns `#f`.

    (circular-list? (list 'a 'b 'c))        ⇒ #f
    (circular-list? (cons* 'a 'b 'c))       ⇒ #f
    (circular-list? (circular-list 'a 'b 'c)) ⇒ #t

### (dotted-list? _object_) ###

Returns `#t` if _object_ is an improper list, otherwise returns `#f`.

    (dotted-list? (list 'a 'b 'c))          ⇒ #f
    (dotted-list? (cons* 'a 'b 'c))         ⇒ #t
    (dotted-list? (circular-list 'a 'b 'c)) ⇒ #f

### (length _list_) ###

Returns the length of _list_. Signals an error if _list_ isn't a proper list.

    (length '(a b c))                       ⇒ 3
    (length '(a (b) (c d e)))               ⇒ 3
    (length '())                            ⇒ 0
    (length (circular-list 'a 'b 'c))       ERROR

### (length+ _clist_) ###
<p class="annotation">lists.scm</p>

Returns the length of _clist_, if it is a proper list. Returns `#f` if _clist_ is a
circular list. Otherwise signals an error.

    (length+ (list 'a 'b 'c))               ⇒ 3
    (length+ (cons* 'a 'b 'c))              ERROR
    (length+ (circular-list 'a 'b 'c))      ⇒ #f

### (null? _object_) ###

### (nil? _object_) ###

Returns `#t` if _object_ is the empty list; otherwise returns `#f`.

    (null? '(a . b))                        ⇒ #f
    (null? '(a b c))                        ⇒ #f
    (null? '())                             ⇒ #t

### (notnull? _object_) ###

### (notnil? _object_) ###

Returns `#f` if _object_ is the empty list; otherwise returns `#t`.

    (notnull? '(a . b))                        ⇒ #f
    (notnull? '(a b c))                        ⇒ #f
    (notnull? '())                             ⇒ #t

### (list-ref _list_ _k_) ###
<p class="annotation">lists.scm</p>

### (nth _k_ _list_) ###

Returns the _k_th element of _list_, using zero-origin indexing. The "valid
indexes" of a list are the non-negative integers less than the length of the
list. The first element of a list has index `0`, the second has index `1`, and
so on. `nth` is provided for Common Lisp familiarity.

    (list-ref '(a b c d) 2)                 ⇒ c

### (first _list_) ###

### (second _list_) ###

### (third _list_) ###

### (fourth _list_) ###

### (fifth _list_) ###

### (sixth _list_) ###

### (seventh _list_) ###
 
### (eighth _list_) ###

### (ninth _list_) ###

### (tenth _list_) ###

Returns the specified element of _list_. It is an error if _list_ is not long
enough to contain the specified element (for example, if the argument to
`seventh` is a list that contains only six elements).

### (last _list_) ###

Returns the last element in the list. An error is raised if _list_ is a circular list.

## Cutting and Pasting Lists ##

### (sublist _list_ _start_ _end_) ###

_start_ and _end_ must be integers satisfying

    0 <= START <= END <= (length LIST)

`sublist' returns a newly allocated list formed from the elements of _list_
beginning at index _start_ (inclusive) and ending at _end_ (exclusive).

### (list-head _list_ _k_) ###
<p class="annotation">lists.scm</p>

### (take _k_ _list_)

Returns a newly allocated list consisting of the first K elements of _list_. _k_
must not be greater than the length of _list_.

We could have defined `list-head` this way:

          (define (list-head list k)
            (sublist list 0 k))

### (list-tail _list_ _k_) ###
<p class="annotation">lists.scm</p>

### (drop _k_ _list_)

Returns the sublist of _list_ obtained by omitting the first _k_ elements. The
result, if it is not the empty list, shares structure with _list_. _k_ must not
be greater than the length of _list_.

### (append _list_...) ###

Returns a list consisting of the elements of the first _list_ followed by the
elements of the other _list_ arguments.

    (append '(x) '(y))                      ⇒ (x y)
    (append '(a) '(b c d))                  ⇒ (a b c d)
    (append '(a (b)) '((c)))                ⇒ (a (b) (c))
    (append)                                ⇒ ()

The resulting list is always newly allocated, except that it shares structure
with the last _list_ argument. The last argument may actually be any object; an
improper list results if the last argument is not a proper list.

    (append '(a b) '(c . d))                ⇒ (a b c . d)
    (append '() 'a)                         ⇒ a

### (append! _list_...) ###

Returns a list that is the all the _list_ arguments concatenated together. The arguments
are changed rather than copied. (Compare this with `append`, which copies
arguments rather than destroying them.) For example:

    (define x '(a b c))
    (define y '(d e f))
    (define z '(g h))
    (append! x y z)                         ⇒ (a b c d e f g h)
    x                                       ⇒ (a b c d e f g h)
    y                                       ⇒ (d e f g h)
    z                                       ⇒ (g h)

### (last-pair _list_) ###

Returns the last pair in _list_, which may be an improper list.
`last-pair` could have been defined this way:

    (define last-pair
      (lambda (x)
        (if (pair? (cdr x))
            (last-pair (cdr x))
            x)))

### (except-last-pair _list_) ###
<p class="annotation">lists.scm</p>

### (except-last-pair! _list_) ###
<p class="annotation">lists.scm</p>

These procedures remove the last pair from _list_. _list_ may be an improper
list, except that it must consist of at least one pair. `except-last-pair`
returns a newly allocated copy of _list_ that omits the last pair.
`except-last-pair!` destructively removes the last pair from _list_ and
returns _list_. If the cdr of _list_ is not a pair, the empty list is returned
by either procedure.

## Filtering Lists ##

### (filter _predicate_ _list_) ###

Returns a newly allocated copy of _list_ containing only the elements satisfying
_predicate_. _predicate_ must be a procedure of one argument.

    (filter odd? '(1 2 3 4 5)) ⇒ (1 3 5)

### (remove _predicate_ _list_) ###

Like `filter`, except that the returned list contains only those elements
**not** satisfying _predicate_.

    (remove odd? '(1 2 3 4 5)) ⇒ (2 4)


### (partition _predicate_ _list_) ###

### (partition _size_ _step_ _list_) ###

The first form partitions the elements of _list_ with _predicate_, and returns a
list of two elements: the list of in-elements and the list of out-elements. The
_list_ is not disordered--elements occur in the result lists in the same order
as they occur in the argument _list_. The dynamic order in which the various
applications of _predicate_ are made is not specified. One of the returned
lists may share a common tail with the argument _list_.

    (partition symbol? '(one 2 3 four five 6)) ⇒
        ((one four five) (2 3 6))

The second form partitions the elements of _list_ into lists of length
_size_, returning a list of those lists. Only lists of _size_ are
returned; any at the end that don't fit are discarded. As with the
first form, elements occur in the result lists in the same order as
they occur in the argument _list_. If the optional _step_ argument is ommitted it defaults to size. Each sublist starts at _step_ elements from the start of the previous. If _step_ > _size_ elements will be skipped between sublists. If _step_ < _size_ the sublists will overlap.

    (partition 2 '(one 2 3 four five 6))    ⇒ ((one 2) (3 four) (five 6))
    
    (partition 2 '(one 2 3 four five 6 7))  ⇒ ((one 2) (3 four) (five 6) (7))

    (partition 2 1 '(1 2 3 4 5 6 7 8 9 0)) ⇒ ((1 2) (2 3) (3 4) (4 5) (5 6) (6 7) (7 8) (8 9))
    
    (partition 2 3 '(1 2 3 4 5 6 7 8 9 0)) ⇒ ((1 2) (4 5) (7 8))


### (delq element list) ###
<p class="annotation">lists.scm</p>

### (delv element list) ###
<p class="annotation">lists.scm</p>

### (delete element list) ###
<p class="annotation">lists.scm</p>

Returns a newly allocated copy of _list_ with all entries equal to _element_
removed. `delq` uses `eq?` to compare _element_ with the entries in _list_,
`delv` uses `eqv?`, and `delete` uses `equal?`.

## Searching Lists ##

### (find _predicate_ _list_) ###

Returns the first element in _list_ for which _predicate_ is true; returns `#f` if
it doesn't find such an element. _predicate_ must be a procedure of one argument.

    (find even? '(3 1 4 1 5 9)) ⇒ 4

Note that `find` has an ambiguity in its lookup semantics--if `find` returns
`#f`, you cannot tell (in general) if it found a `#f` element that satisfied
_predicate_, or if it did not find any element at all. In many situations, this
ambiguity cannot arise--either the list being searched is known not to contain
any `#f` elements, or the list is guaranteed to have an element satisfying
_predicate_. However, in cases where this ambiguity can arise, you should use
`find-tail` instead of `find` -- `find-tail` has no such ambiguity:

    (cond ((find-tail pred lis)
            => (lambda (pair) ...)) ; Handle (CAR PAIR)
          (else ...)) ; Search failed.

### (find-tail _predicate_ _list_) ###

Returns the first pair of _list_ whose car satisfies _predicate_; returns `#f`
if there's no such pair. `find-tail` can be viewed as a general-predicate
variant of `memv`.

### (memq _object_ _list_) ###

### (memv _object_ _list_) ###

### (member _object_ _list_) ###

These procedures return the first pair of _list_ whose car is _object_; the returned
pair is always one from which _list_ is composed. If _object_ does not occur in
_list_, `#f` (n.b.: not the empty list) is returned. `memq` uses `eq?` to compare
_object_ with the elements of _list_, while `memv` uses `eqv?` and `member` uses
`equal?`.

    (memq 'a '(a b c))                      ⇒ (a b c)
    (memq 'b '(a b c))                      ⇒ (b c)
    (memq 'a '(b c d))                      ⇒ #f
    (memq (list 'a) '(b (a) c))             ⇒ #f
    (member (list 'a) '(b (a) c))           ⇒ ((a) c)
    (memq 101 '(100 101 102))               ⇒ (101 102)
    (memv 101 '(100 101 102))               ⇒ (101 102)

Although they are often used as predicates, `memq`, `memv`, and
`member` do not have question marks in their names because they return
useful values rather than just `#t` or `#f`.

### (memp _predicate_ _list_)

Returns the first pair of _list_ for which _predicate_ returns `#t` when passed
the car; the returned pair is always one from which _list_ is composed. If
_predicate_ never returns `#t`, `#f` (n.b.: not the empty list) is returned.

## Mapping of Lists ##

### (map _procedure_ _list_...) ###

_procedure_ must be a procedure taking as many arguments as there are _lists_.
If more than one _list_ is given, then they must all be the same length. `map`
applies _procedure_ element-wise to the elements of the _lists_ and returns a
list of the results, in order from left to right. The dynamic order in which
_procedure_ is applied to the elements of the _lists_ is unspecified; use
`for-each` to sequence side effects.

    (map cadr '((a b) (d e) (g h)))           ⇒ (b e h)
    (map (lambda (n) (expt n n)) '(1 2 3 4))  ⇒ (1 4 27 256)
    (map + '(1 2 3) '(4 5 6))                 ⇒ (5 7 9)
    (let ((count 0))
      (map (lambda (ignored)
             (set! count (+ count 1))
             count)
           '(a b c)))                         ⇒ unspecified

### (for-each _procedure_ _list_ ...) ###

The arguments to `for-each` are like the arguments to `map`, but `for-each`
calls _procedure_ for its side effects rather than for its values. Unlike `map`,
`for-each` is guaranteed to call _procedure_ on the elements of the _lists_ in order
from the first element to the last, and the value returned by `for-each` is
unspecified.

    (let ((v (make-vector 5)))
      (for-each (lambda (i)
                  (vector-set! v i (* i i)))
                '(0 1 2 3 4))
      v)                            ⇒ #(0 1 4 9 16)

## Reduction of Lists ##

### (reduce _procedure_ _initial_ _list_) ###

### (reduce-left _procedure_ _initial_ _list_) ###

Combines all the elements of _list_ using the binary operation _procedure_. For
example, using `+` one can add up all the elements:

    (reduce-left + 0 list-of-numbers)

The argument _initial_ is used only if _list_ is empty; in this case _initial_ is the
result of the call to `reduce-left`. If _list_ has a single argument, it is
returned. Otherwise, the arguments are reduced in a left-associative fashion.
For example:

    (reduce-left + 0 '(1 2 3 4))            ⇒ 10
    (reduce-left + 0 '(1 2))                ⇒ 3
    (reduce-left + 0 '(1))                  ⇒ 1
    (reduce-left + 0 '())                   ⇒ 0
    (reduce-left + 0 '(foo))                ⇒ foo
    (reduce-left list '() '(1 2 3 4))       ⇒ (((1 2) 3) 4)

### (reduce-right _procedure_ _initial_ _list_) ###

Like `reduce-left` except that it is right-associative.

    (reduce-right list '() '(1 2 3 4))      ⇒ (1 (2 (3 4)))

### (fold-right _procedure_ _initial_ _list_) ###

Combines all of the elements of _list_ using the binary operation _procedure_.
Unlike `reduce-left` and `reduce-right`, _initial_ is always used:

    (fold-right + 0 '(1 2 3 4))             ⇒ 10
    (fold-right + 0 '(foo))                 ERROR Illegal datum
    (fold-right list '() '(1 2 3 4))        ⇒ (1 (2 (3 (4 ()))))

`fold-right` has interesting properties because it establishes a homomorphism
between (`cons`, `()`) and (_procedure_, _initial_). It can be thought of as
replacing the pairs in the spine of the list with _procedure_ and replacing the
`()` at the end with _initial_. Many of the classical list-processing procedures
can be expressed in terms of `fold-right`, at least for the simple versions that
take a fixed number of arguments:

    (define (copy-list list)
      (fold-right cons '() list))

    (define (append list1 list2)
      (fold-right cons list2 list1))

    (define (map p list)
      (fold-right (lambda (x r) (cons (p x) r)) '() list))

    (define (reverse items)
      (fold-right (lambda (x r) (append r (list x))) '() items))

### (fold-left _procedure_ _initial_ _list_) ###

Combines all the elements of _list_ using the binary operation _procedure_.
Elements are combined starting with _initial_ and then the elements of _list_
from left to right. Whereas `fold-right` is recursive in nature, capturing the
essence of cdr-ing down a list and then computing a result (although all the
reduce/fold functions are implemented iteratively in the runtime), `fold-left`
is iterative in nature, combining the elements as the list is traversed.

    (fold-left list '() '(1 2 3 4))         ⇒ ((((() 1) 2) 3) 4)

    (define (length list)
      (fold-left (lambda (sum element) (+ sum 1)) 0 list))

    (define (reverse items)
      (fold-left (lambda (x y) (cons y x)) () items))

### (any _predicate_ _list_...) ###

Applies _predicate_ across the _lists_, returning true if _predicate_ returns true on
any application.

If there are n list arguments _list1_ ... _listn_, then _predicate_ must be a
procedure taking n arguments and returning a boolean result.

`any` applies _predicate_ to the first elements of the _list_ parameters. If
this application returns a true value, `any` immediately returns that value.
Otherwise, it iterates, applying _predicate_ to the second elements of the
_list_ parameters, then the third, and so forth. The iteration stops when a true
value is produced or one of the lists runs out of values; in the latter case,
`any` returns `#f`. The application of _predicate_ to the last element of the
_lists_ is a tail call.

Note the difference between `find` and `any` -- `find` returns the element that
satisfied the predicate; `any` returns the true value that the _predicate_
produced.

Like `every`, `any`'s name does not end with a question mark -- this is to
indicate that it does not return a simple boolean (`#t` or `#f`), but a general
value.

    (any integer? '(a 3 b 2.7))   ⇒ #t
    (any integer? '(a 3.1 b 2.7)) ⇒ #f
    (any < '(3 1 4 1 5)
           '(2 7 1 8 2)) ⇒ #t

### (every _predicate_ _list_...) ###

Applies _predicate_ across the _lists_, returning true if _predicate_ returns
true on every application.

If there are n list arguments _list1_ ... _listn_, then _predicate_ must be a
procedure taking n arguments and returning a boolean result.

`every` applies _predicate_ to the first elements of the _list_ parameters. If
this application returns false, `every` immediately returns false. Otherwise,
it iterates, applying _predicate_ to the second elements of the _list_ parameters,
then the third, and so forth. The iteration stops when a false value is produced
or one of the _lists_ runs out of values. In the latter case, `every` returns the
true value produced by its final application of _predicate_. The application of
_predicate_ to the last element of the _lists_ is a tail call.

If one of the _lists_ has no elements, `every` simply returns `#t`.

Like `any`, `every`'s name does not end with a question mark -- this is to
indicate that it does not return a simple boolean (`#t` or `#f`), but a general
value.

## Miscellaneous List Operations ##

### (circular-list _object_...) ###

This procedure is like `list`, except that the returned list is circular.

### (reverse _list_) ###

Returns a newly allocated list consisting of the top-level elements of _list_ in
reverse order.

    (reverse '(a b c))                  ⇒ (c b a)
    (reverse '(a (b c) d (e (f))))      ⇒ ((e (f)) d (b c) a)

### (sort _sequence_ _procedure_) ###

_sequence_ must be either a list or a vector. _procedure_ must be a procedure of two
arguments that defines a "total ordering" on the elements of _sequence_. In other
words, if X and Y are two distinct elements of _sequence_, then it must be the
case that

    (and (PROCEDURE X Y)
         (PROCEDURE Y X))
         ⇒ #f

If _sequence_ is a list (vector), `sort` returns a newly allocated list (vector)
whose elements are those of _sequence_, except that they are rearranged to be
sorted in the order defined by _procedure_. So, for example, if the elements of
_sequence_ are numbers, and _procedure_ is `<`, then the resulting elements are
sorted in monotonically nondecreasing order. Likewise, if _procedure_ is `>`, the
resulting elements are sorted in monotonically nonincreasing order. To be
precise, if X and Y are any two adjacent elements in the result, where X
precedes Y, it is the case that

    (PROCEDURE Y X)
         ⇒ #f

There is also the function `vector-sort` that applies only to vectors, and
will raise an erro if applied to a list.

### (flatten _list_) ###

Returns a list with the contents of all top level nested lists placed directly
in the result. This is best illustrated with some examples:

    (flatten '(a b c d)) ⇒ (a b c d)
    (flatten '(a (b c) d)) ⇒ (a b c d)
    (flatten '(a (b (c d)))) ⇒ (a b (c d))

### (flatten* _list_) ###

Returns a list with the contents of all nested lists placed directly in the
result. This is also best illustrated with some examples:

    (flatten* '(a b c d)) ⇒ (a b c d)
    (flatten* '(a (b c) d)) ⇒ (a b c d)
    (flatten* '(a (b (c d)))) ⇒ (a b c d)

### (union _list_...) ###

Returns a list that contains all items in the argument _list_s. Each item
appears only once in the result regardless of whether it was repeated in any
_list_.

    (union '(1 2 3) '(4 5))      ⇒ (1 2 3 4 5)
    (union '(1 2 3) '(3 4 5))    ⇒ (1 2 3 4 5)
    (union '(1 2 3 2) '(4 4 5))  ⇒ (1 2 3 4 5)

### (intersection _list_...) ###

Returns a list that contains only items that are in all _list_ arguments.

    (intersection '(1 2 3) '(3 4 5)) ⇒ (3)
    (intersection '() '(3 4 5))      ⇒ ()

### (complement _list_...) ###

Returns a list that contains only items that were in the first _list_ argument,
but not in any of the subsequent argument _list_s.

    (complement '(1 2 3 4 5) '(1 3 5))      ⇒ (2 4)
    (complement '() '(1 2))                 ⇒ ()
    (complement '(1 2 3 4 5) '(1 2) '(3 4)) ⇒ (5)


# Vectors #

Vectors are heterogenous structures whose elements are indexed by non-negative
integers. A vector typically occupies less space than a list of the same length,
and the average time required to access a randomly chosen element is typically
less for the vector than for the list.

The length of a vector is the number of elements that it contains. This number
is a non-negative integer that is fixed when the vector is created. The valid
indexes of a vector are the non-negative integers less than the length of the
vector. The first element in a vector is indexed by zero, and the last element
is indexed by one less than the length of the vector.

Vectors are written using the notation `#(object ...)`. For example, a vector of
length 3 containing the number zero in element 0, the list `(2 2 2 2)` in
element 1, and the string `"Anna"` in element 2 can be written as

     #(0 (2 2 2 2) "Anna")

Note that this is the external representation of a vector, not an expression
evaluating to a vector. Like list constants, vector constants must be quoted:

    '#(0(2222)"Anna") ⇒ #(0 (2222) "Anna")

A number of the vector procedures operate on subvectors. A subvector is a
segment of a vector that is specified by two non-negative integers, start and
end. Start is the index of the first element that is included in the subvector,
and end is one greater than the index of the last element that is included in
the subvector. Thus if start and end are the same, they refer to a null
subvector, and if start is zero and end is the length of the vector, they refer
to the entire vector. The valid indexes of a subvector are the integers between
start inclusive and end exclusive.

## Construction of Vectors ##

### (make-vector _k_ [_object_])  ###

Returns a newly allocated vector of k elements. If _object_ is specified,
`make-vector` initializes each element of the vector to the _object_.
Otherwise the initial elements of the result are unspecified.

### (vector _object_...) ###

Returns a newly allocated vector whose elements are the given _objects_.
`vector` is analogous to `list`.

    (vector 'a 'b 'c) ⇒ #(a b c)

### (vector-copy _vector_) ###

Returns a newly allocated vector that is a copy of _vector_.

### (list->vector _list_) ###

Returns a newly allocated vector initialized to the elements of _list_.

    (list->vector '(dididit dah)) ⇒ #(dididit dah)

### (vector->list _vector_) ###

Returns a newly allocated list initialized to the elements of _vector_.

    (vector->list '#(dididitdah)) ⇒ (dididit dah)

### (make-initialized-vector _k_ _initialization_) ###

Similar to `make-vector`, except that the elements of the result are
determined by calling the procedure _initialization_ on the indices. For
example:

    (make-initialized-vector 5 (lambda (x) (* x x))) ⇒ #(0 1 4 9 16)

### (vector-grow _vector_ _k_) ###

_k_ must be greater than or equal to the length of _vector_. Returns a newly
allocated vector of length _k_. The first `(vector-length vector)` elements of
the result are initialized from the corresponding elements of _vector_. The
remaining elements of the result are unspecified.

## Enumerating over Vectors ##

### (vector-map _procedure_ _vector_...) ###

_procedure_ must be a procedure with arity the same as the number or _vector_s.
`vector-map` applies _procedure_ element-wise to the corresponding elements of
each _vector_ and returns a newly allocated vector of the results, in order from
left to right. The dynamic order in which procedure is applied to the elements
of vector is unspecified.

    (vector-map cadr '#((ab)(de)(gh)))            ⇒ #(b e h)
    (vector-map (lambda (n) (* n n)) '#(1 2 3 4)) ⇒ #(1 4 9 16)
    (vector-map + '#(1 2 3) '#(4 5 6))            ⇒ #(5 7 9)

## Selecting Vector Components ##

### (vector-length _vector_) ###

Returns the number of elements in _vector_.

### (vector-ref _vector_ _k_) ###

Returns the contents of element _k_ of _vector_. _k_ must be a valid index of
_vector_.

          
    (vector-ref '#(1 1 2 3 5 8 13 21) 5)    ⇒  8

### (vector-set! _vector_ _k_ _object_) ###

Stores _object_ in element _k_ of _vector_ and returns an unspecified value. _K_
must be a valid index of _vector_.

    (let ((vec (vector 0 '(2 2 2 2) "Anna")))
      (vector-set! vec 1 '("Sue" "Sue"))
      vec)
                ⇒  #(0 ("Sue" "Sue") "Anna")

### (vector-first _vector_) ###

### (vector-second _vector_) ###

### (vector-third _vector_) ###

### (vector-fourth _vector_) ###

### (vector-fifth _vector_) ###

### (vector-sixth _vector_) ###

### (vector-seventh _vector_) ###

### (vector-eighth _vector_) ###

### (vector-ninth _vector_) ###

### (vector-tenth _vector_) ###

These procedures access the first several elements of _vector_ in the obvious
way. It is an error if the implicit index of one of these procedures is not a
valid index of _vector_.  Using `first` - `tenth` will work on both lists and vectors.

### (vector-last _vector_) ###

Returns the last element of _vector_. `last` will also work on both lists and vectors.

### (vector-binary-search _vector_ _key<?_ _unwrap-key_ _key_) ###

Searches _vector_ for an element with a key matching _key_, returning the
element if one is found or _#f_ if none. The search operation takes time
proportional to the logarithm of the length of _vector_. _unwrap-key_ must be a
procedure that maps each element of _vector_ to a key. _key<?_ must be a
procedure that implements a total ordering on the keys of the elements.

    (define (translate number)
      (vector-binary-search '#((1 . i)
                               (2 . ii)
                               (3 . iii)
                               (6 . vi))
                            < car number))
    (translate 2)  ⇒  (2 . ii)
    (translate 4)  ⇒  #f

## Cutting Vectors ##

### (subvector _vector_ _start_ _end_) ###

Returns a newly allocated vector that contains the elements of _vector_ between
index _start_ (inclusive) and _end_ (exclusive).

### (vector-head _vector_ _end_) ###

Equivalent to

    (subvector vector 0 end)

### (vector-tail _vector_ _start_) ###

Equivalent to

    (subvector vector start (vector-length vector))

## Modifying Vectors ##

### (vector-fill! _vector_ _object_) ###

### (subvector-fill! _vector_ _start_ _end_ _object_) ###

Stores _object_ in every element of the vector (subvector) and returns an
unspecified value.

### (subvector-move-left! _vector1_ _start1_ _end1_ _vector2_ _start2_) ###

### (subvector-move-right! _vector1_ _start1_ _end1_ _vector2_ _start2_) ###

Destructively copies the elements of _vector1_, starting with index _start1_
(inclusive) and ending with _end1_ (exclusive), into _vector2_ starting at index
_start2_ (inclusive). _vector1_, _start1_, and _end1_ must specify a valid
subvector, and _start2_ must be a valid index for _vector2_. The length of the
source subvector must not exceed the length of _vector2_ minus the index
_start2_.

The elements are copied as follows (note that this is only important when
_vector1_ and _vector2_ are `eqv?`):

`subvector-move-left!`: The copy starts at the left end and moves toward the
right (from smaller indices to larger). Thus if _vector1_ and _vector2_ are the
same, this procedure moves the elements toward the left inside the vector.

`subvector-move-right!`: The copy starts at the right end and moves toward the
left (from larger indices to smaller). Thus if _vector1_ and _vector2_ are the
same, this procedure moves the elements toward the right inside the vector.

### (vector-sort! _vector_ _procedure_) ###

_procedure_ must be a procedure of two arguments that defines a _total ordering_
on the elements of _vector_. The elements of _vector_ are rearranged so that
they are sorted in the order defined by _procedure_. The elements are rearranged
in place, that is, VECTOR is destructively modified so that its elements are in
the new order.

`sort!` returns _vector_ as its value.

See also the definition of `sort`.

# Associations #

## Association Lists ##

"Association lists" are one of Lisp's oldest association mechanisms. Because
they are made from ordinary pairs, they are easy to build and manipulate, and
very flexible in use. However, the average lookup time for an association list
is linear in the number of associations. Frames are a more efficient

An "association list", or "alist", is a data structure used very frequently in
Scheme. An alist is a list of pairs, each of which is called an "association".
The car of an association is called the "key", and the cdr is called the
"value". Having lists as pair values can cause confusion becase the pair in the
alist look like proper lists and not dotted pairs. Functions that look
specifically for dotted pairs will not consider it an association list (e.g.
`alist?`) while those that don't will work fine (e.g. the `assoc` &
`dissoc` functions). The latter simply look at the car and cdr of the pairs.
not whether they are canonical dotted pairs (i.e. their cdr is not a pair).

    '((a . (1 2)) (b . (3 4)))                   ⇒ ((a 1 2) (b 3 4))
    (alist? '((a . (1 2)) (b . (3 4))))          ⇒ #f
    (assoc 'b '((a . (1 2)) (b . (3 4))))        ⇒ (b 3 4)
    (cdr (assoc 'b '((a . (1 2)) (b . (3 4)))))  ⇒ (3 4)

An advantage of the alist representation is that an alist can be incrementally
augmented simply by adding new entries to the front. Moreover, because the
searching procedures `assv' et al. search the alist in order, new entries can
"shadow" old entries. If an alist is viewed as a mapping from keys to data, then
the mapping can be not only augmented but also altered in a non-destructive
manner by adding new entries to the front of the alist.

### (alist? _object_) ###
<p class="annotation">lists.scm</p>

Returns `#t` if _object_ is an association list (including the empty list);
otherwise returns `#f`. Any _object_ satisfying this predicate also satisfies
`list?`.

### (acons _key_ _value_ [_alist_]) ###

Returns the result of consing a pair `(key . value)` to _alist_. If _alist_ is
omitted, it defaults to the empty list.

    (acons 'a 1)            ⇒ ((a . 1))
    (acons 'a 1 '((b . 2))) ⇒ ((a . 1) (b . 2))
    (acons 'b 1 '((b . 2))) ⇒ ((b . 1) (b . 2))

### (pairlis _keys_ _values_ [_alist_]) ###

Creates an association list from lists of _keys_ and _values_ by acons-ing
onto _alist_. If _alist_ is omitted, it defaults to the empty list. Note that
the key and value lists are paired up in left to right order, but the order they
are consed onto _alist_ is unspecified.

    (pairlis '(a b) '(1 2))                    ⇒ ((b . 2) (a . 1)))
    (pairlis '(a b) '(1 2) '((c . 3) (d . 4))) ⇒ ((b . 2) (a . 1) (c . 3) (d . 4))))

### (assq _object_ _alist_) ###

### (assv _object_ _alist_) ###

### (assoc _object_ _alist_) ###

These procedures find the first pair in _alist_ whose car field is _object_, and
return that pair; the returned pair is always an **element** of _alist_,
**not** one of the pairs from which _alist_ is composed. If no pair in _alist_
has _object_ as its car, `#f` (n.b.: not the empty list) is returned. `assq`
uses `eq?` to compare _object_ with the car fields of the pairs in _alist_,
while `assv` uses `eqv?` and `assoc` uses `equal?`.

    (define e '((a . 1) (b . 2) (c . 3)))
    (assq 'a e)                             ⇒  (a . 1)
    (assq 'b e)                             ⇒  (b . 2)
    (assq 'd e)                             ⇒  #f
    (assq (list 'a) '(((a)) ((b)) ((c))))   ⇒  #f
    (assoc (list 'a) '(((a)) ((b)) ((c))))  ⇒  ((a))
    (assv 5 '((2 . 3) (5 . 7) (11 . 13)))   ⇒  (5 . 7)

### (rassoc _value_ _alist_) ###

Return the pair from _alist_ whose cdr is equal to _value_. `#f`
is returned is _value_ isn’t found.

    (rassoc 1 '((a . 1) (b . 2) (c . 3))) ⇒ (a . 1)
    (rassoc 3 '((a . 1) (b . 2)))         ⇒ #f


### (del-assq _object_ _alist_) ###
<p class="annotation">lists.scm</p>

### (dissq _object_ _alist_) ###

### (del-assv _object_ _alist_) ###
<p class="annotation">lists.scm</p>

### (dissv _object_ _alist_) ###

### (del-assoc _object_ _alist_) ###
<p class="annotation">lists.scm</p>

### (dissoc _object_ _alist_) ###

These procedures return a newly allocated copy of _alist_ in which all
associations with keys equal to _object_ have been removed. Note that while the
returned copy is a newly allocated list, the association pairs that are the
elements of the list are shared with _alist_, not copied. `del-assq`/`dissq`
use `eq?` to compare _object_ with the keys, while `del-assv`/`dissv` use
`eqv?` and `del-assoc`/`dissoc` use `equal?`.

    (define a
      '((butcher . "231 e22nd St.")
        (baker . "515 w23rd St.")
        (hardware . "988 Lexington Ave.")))

    (del-assq 'baker a)
         ⇒
         ((butcher . "231 e22nd St.")
          (hardware . "988 Lexington Ave."))

# Frames

GoLisp contains a frame system inspired by Self [4] and NewtonScript [5].

A frame is a set of named slots that hold arbitrary values. Slot names must be
symbols that end with a colon. For example: `color:`, or `height:`. When
evaluated normally, these special symbols don’t get looked up in the
environment, they simply evaluate to themselves.

    (define a a:)

    'a ⇒ a
    a  ⇒ a:

    'a: ⇒ a:
    a:  ⇒ a:

### (make-slotname _symbol_) ###

This function takes a symbol or string and returns an interned slotname based on it, doing what is required.

    (make-slotname 'name) ⇒ name:
    (make-slotname "name") ⇒ name:
    (make-slotname name:) ⇒ name:

## Basic functions

### (make-frame _slot-name_ _slot-value_ ... )

Frames can be created using the `make-frame` function, passing it an alternating
sequence of slot names and values:

    (make-frame a: 1 b: 2)

This results in a frame with two slots, named `a:` and `b:` with values `1` and
`2`, respectively.

### { _slot-name_ _slot-value_ ... }

This is an alternative syntax for defining frame literals:

    {a: 1 b: 2}

Both are equivalent. Slot names and values in both cases are evaluated (this is
one reason for the non-evaluating symbols: it avoiding having to quote literal
slot names).

### (clone _frame_)

Frames represent things. For example, you could use a frame that looks like `{x:
1 y: 10}` to represent a point. A system that would use point frames will
typically need many independant points. The approach to this is to create a
prototypical point data frame, and use the `clone` function to create
individual, independant frames:

    (define point {x: 1 y: 1})
    (define p1 (clone point))
    (set-slot! p1 x: 5)
    (get-slot p1 x:)    ⇒ 5
    (get-slot point x:) ⇒ 1

### (has-slot? _frame_ _slot-name_)

### (_slot-name_? _frame_)

The `has-slot?` function is used to query whether a frame contains (directly or
in an ancestor) the particular slot:

    (define f {a: 1 b: 2})
    (has-slot? f a:)      ⇒ #t
    (a:? f)               ⇒ #t
    (has-slot? f c:)      ⇒ #f
    (c:? f)               ⇒ #f

### (get-slot _frame_ _slot-name_)

### (_slot-name_ _frame_)

The `get-slot` function is used to retrieve values from frame slots:

    (define f {a: 1 b: 2})
    (get-slot f a:)       ⇒ 1
    (a: f)                ⇒ 1
    (get-slot f b:)       ⇒ 2
    (b: f)                ⇒ 2

If the frame passed to `get-slot` contains a slot with the specified name, it’s
value is returned. If not, then parent frames are searched in a nondeterministic
order until a slot with the specified name is found. If a matching slot is
found, it’s value is returned. If none is found an error is raised.

    (define f {a: 1 b: 2})
    (define g {parent*: f c: 3})

    (get-slot g c:) ⇒ 3
    (get-slot g a:) ⇒ 1  ; from the frame f

### (get-slot-or-nil _frame_ _slot-name_)

The same as above, except that if a matching slot is not found, `nil` is
returned instead of raising an error.

### (set-slot! _frame_ _slot-name_ _new-value_)

### (_slot-name_! _frame_ _new-value_)

The `set-slot!` function is used to change values in frame slots:

    (define f {a: 1 b: 2})
    (get-slot f a:)    ⇒ 1
    (set-slot! f a: 5) ⇒ 5
    (a:! f 5) ⇒ 5
    (get-slot f a:)    ⇒ 5

Trying to set a slot that doesn’t exist in the frame will result in a
corresponding slot being created.

    (define f {a: 1 b: 2})
    (set-slot! f c: 5) ⇒ 5
    f                  ⇒ {a: 1 b: 2 c: 5}

### (remove-slot! _frame_ _slot-name_)

The `remove-slot!` function is used to function is used to remove a slot from a
frame. It only removes slots from the frame itself. not any of it's parents.
`remove-slot!` return `#t` if the slot was removed, `#f` otherwise.

    (define f {a: 1 b: 2})
    (remove-slot! f a:) ⇒ #t
    f                   ⇒ {b: 2}
    (remove-slot! f a:) ⇒ #f

### (frame-keys _frame_)

Returns a list of the slot names in _frame_. Note that the order of the result
is nondeterministic.

    (frame-keys {a: 1 b: 2}) ⇒ (a: b:)

### (frame-values _frame_)

Returns a list of the slot values in _frame_. Note that the order of the result
is nondeterministic.

    (frame-values {a: 1 b: 2}) ⇒ (1 2)

## Parent slots

Frames can have slots that refer to other slots to provide prototype
inheritance. These slots have names that have a `*` immediately preceeding the
trailing `:`, for example `proto*:`. The names of the parent slots don't matter;
it is the trailing `*:` in the name that marks them as parent slots. A frame can
have any number of parent slots.

When a slot is being searched for, if it isn't found in the specified slot,
these _parent_ slots are recursively searched until the requested slot is found
or the entire graph has been examined.

    > (define y {a: 1})
    =⇒ {a: 1}
    > (define x {b: 2 p*: y})
    =⇒ {b: 2 p*: {...}}

    > (a: x)
    =⇒ 1

**Note:** Parent slots are searched in arbitrary order.

## Function slots

Now things get interesting. Slot values can be functions (typically `lambda`
expressions) as well as data. Function slots can be executed by using the `send`
function

### (send _frame_ _slot-name_ _arg_...)

### (_slot-name_> _frame_  _arg_...)

    (define f {add: (lambda () (+ 1 2))})
    (send f add:) ⇒ 3
    (add:> f) ⇒ 3

As expected, parameters are supported:

    (define f {add: (lambda (x) (+ 1 x))})
    (send f add: 2) ⇒ 3

In the body of a function, slots can be refrenced like normal variables. To do
so, simply omit the trailing colon:

    (define f {a: 3
               add: (lambda (x) (+ a x))})
    (send f add: 2) ⇒ 5

Likewise, functions in the frame (or parent frames) can be referred to directly
by name.

    (define f {a: 5
               b: 2
               foo: (lambda (x) (+ x a))
               bar: (lambda () (foo b))})
    (send f bar:) ⇒ 7

Bindings defined in the local environment (e.g. by a `let` form) hide frame
slots of the same name. In the following, `let` overrides the `a:` slot by
introducing a local binding for `a`.

    (let ((f {a: 42})
          (g {parent*: f  foo: (lambda ()
                                 (let ((a 10))
                                   (+ 1 a)))}))
    (send g foo:) ⇒ 11

Of course the end game of all this is to be able to inherit functions from
parent frames:

    (define f {a: 5
               foo: (lambda (x) (+ x a))})
    (define g {parent*: f
               b: 2
               bar: (lambda () (foo b))})
    (send g bar:) ⇒ 7

Notice that we’ve been saying parent **frames**, i.e. plural. Also note that
parent slot names are arbitrary and for documentation purposes only. A frame can
have any number of parents. When a slot is looked for, the explictily specified
frame is searched first, recursively followed by parent frames in a
nondeterministic order until a matching slot is found. If none are found, the
result is nil.

    (define e {a: 5})
    (define f {b: 2})
    (define g {parent-e*: e
               parent-f*: f
               foo: (lambda (x) (+ x a))
               bar: (lambda () (foo b))}))
    (send g bar:)       ⇒ 7
    (set-slot! g a: 10)
    (get-slot g a:)     ⇒ 10
    (get-slot e a:)     ⇒ 5

When you set a slot with parent frames involved, if the slot is found in the
explicit frame it’s value is set and the new value is returned. If it doesn’t
exist in the explicit frame, it gets created there. This new slot now hides any
slots in a parent with the same name.

### (send-super _slot-name_ _arg_...)

### (_slot-name_^ _arg_...)

Like `send`, but sends to the first parent that has the named slot.
**`send-super` can only be used from within a function slot.**

### (apply-slot _frame_ _slot-name_ _sexpr_...)

Apply the function that results from evaluating the function in slot _slot-name_ of _frame_ to the argument list resulting from evaluating each _sexpr_.

Each initial _sexpr_ can evaluate to any type of object, but the final one (and there must be at least one _sexpr_) must evaluate to a list.

    (define f {foo: (lambda (x y z) (+ 1 x y z))})
    (apply-slot f foo: 2 '(3 4)) ⇒ 10
    (apply-slot f foo: '(2 3 4)) ⇒ 10

### (apply-slot-super _slot-name_ _sexpr_...)

Like `apply-slot`, but sends to the first parent that has the named slot. **`apply-slot-super` can only be used from within a function slot.**

## Dynamic inheritence

Parent slots are slots like any other and can have their values changed at any
time. This ability is somewhat unusual for those with a heavy OO background but
can be very useful for changing behavior on the fly. A prime example of this is
the implimentation of a state machine. The functions for each state can be
placed in different frames and transitions can modify the slot contaiing that
state behavior.

Here’s an example of this.

    (define state {name: ""
                   enter: (lambda ())
                   halt: (lambda ())
                   set-speed: (lambda (s))
                   halt: (lambda ())
                   transition-to: (lambda (s)
                                    (set! state* s)
                                    (enter))})

    (define stop-state {name: "stop"
                        parent*: state
                        enter: (lambda ()
                                 (set! speed 0)
                                 (transition-to idle-state))})

    (define idle-state {name: "idle"
                        parent*: state
                        set-speed: (lambda (s)
                                     (set! speed s)
                                     (transition-to start-state))})

    (define start-state {name: "start"
                         parent*: state
                         halt: (lambda ()
                                 (transition-tostop-state))
                         set-speed: (lambda (s)
                                      (set! speed s)
                                      (transition-to change-speed-state))})

    (define change-speed-state {name: "change-speed"
                                parent*: state
                                halt: (lambda ()
                                        (transition-to stop-state))
                                set-speed: (lambda (s)
                                             (set! speed s))})

    (define motor {speed: 0
                   state*: state
                   start: (lambda () (transition-to stop-state)) })

Now you can do things like the following:

    (send motor start:)
    motor ⇒ {speed: 0 state*: {name: "idle" ...}}
    (send motor set-speed: 10)
    motor ⇒ {speed: 10 state*: {name: "start" ...}}
    (send motor set-speed: 20)
    motor ⇒ {speed: 20 state*: {name: "change-speed" ...}}
    (send motor set-speed: 15)
    motor ⇒ {speed: 15 state*: {name: "change-speed" ...}}
    (send motor halt:)
    motor ⇒ {speed: 0 state*: {name: "idle" ...}}

## Json support ##

GoLisp has built-in support for converting between stringified Json and frames,
according to the following rules:

* numbers and strings map directly in both directions
* frames recursively map to objects, and the reverse
* lists recursively map to arrays, and the reverse
* frame slots names map to string field names, and the reverse
* function slots **do not** get mapped to json
* parent slots **DO not** get mapped to json

### (json->lisp *string*) ###

    (json->lisp "{'key': [1, 2, 3]}") ⇒ {key: (1 2 3)}

### (lisp->json *frame*) ###

    (lisp->json {key: (1 2 3)}) ⇒ "{'key': [1, 2, 3]}"

# Miscellaneous Datatypes #

## Booleans ##

The "boolean objects" are "true" and "false".  The boolean constant
true is written as `#t`, and the boolean constant false is written as
`#f`.

The primary use for boolean objects is in the conditional expressions `if`,
`cond`, `and`, and `or`; the behavior of these expressions is determined
by whether objects are true or false. These expressions count only `#f` as
false. They count everything else, including `#t`, pairs, symbols, numbers,
strings, vectors, and procedures as true.

Boolean constants evaluate to themselves, so you don't need to quote them.

     #t                                      ⇒  #t
     #f                                      ⇒  #f
     '#f                                     ⇒  #f
     t                                       ERROR Unbound variable

### false ###
<p class="annotation">misc.scm</p>

### true ###
<p class="annotation">misc.scm</p>

These variables are bound to the objects `#f` and `#t` respectively. 

Note that the symbol `true` is not equivalent to `#t`, and the symbol `false` is
not equivalent to `#f`.

### (boolean? _object_) ###

Returns `#t` if _object_ is either `#t` or `#f`; otherwise returns `#f`.

    (boolean? #f)                           ⇒  #t
    (boolean? 0)                            ⇒  #f

### (not _object_) ###

### (false? _object_) ###
<p class="annotation">misc.scm</p>

These procedures return `#t` if _object_ is false; otherwise they return `#f`. In
other words they _invert_ boolean values. These two procedures have identical
semantics; their names are different to give different connotations to the test.

    (not #t)                                ⇒  #f
    (not 3)                                 ⇒  #f
    (not (list 3))                          ⇒  #f
    (not #f)                                ⇒  #t

### (boolean=? _obj1_ _obj2_) ###
<p class="annotation">misc.scm</p>

This predicate is true iff _obj1_ and _obj2_ are either both true or both false.

### (boolean/and _object_...) ###
<p class="annotation">misc.scm</p>

This procedure returns `#t` if none of its arguments are `#f`.
Otherwise it returns `#f`.

### (boolean/or _object_...) ###
<p class="annotation">misc.scm</p>

This procedure returns `#f` if all of its arguments are `#f`.
Otherwise it returns `#t`.

## Symbols ##

Unlike MIT/GNU Scheme, GoLisp only provides one type of symbol: "interned".
Interned symbols are far more common than uninterned symbols, and there are more
ways to create them. We decided that uninterned symbols were not necessary for
our uses. Throughtout this document "symbol" means "interned symbol"

Symbols have an extremely useful property: any two symbols whose names are the
same, in the sense of `string=?`, are the same object (i.e. they are `eq?`
to one another). The term "interned" refers to the process of "interning" by
which this is accomplished.

The rules for writing an symbol are the same as the rules for writing an
identifier. Any symbol that has been returned as part of a literal expression,
or read using the `read` procedure and subsequently written out using the
`write` procedure, will read back in as the identical symbol (in the sense of
`eq?`).

Usually it is also true that reading in an symbol that was previously written
out produces the same symbol. An exception are symbols created by the procedures
`string->symbol` and `intern`; they can create symbols for which this
write/read invariance may not hold because the symbols' names contain special
characters.

### (symbol? _object_) ###

Returns `#t` if _object_ is a symbol, otherwise returns `#f`.

    (symbol? 'foo)                                  ⇒  #t
    (symbol? (car '(a b)))                          ⇒  #t
    (symbol? "bar")                                 ⇒  #f

### (symbol->string _symbol_) ###

Returns the name of _symbol_ as a string. If _symbol_ was returned by
`string->symbol`, the value of this procedure will be identical (in the sense
of `string=?`) to the string that was passed to `string->symbol`. Unlike
MIT/GNU Scheme, the result of `symbol->string` is not converted to lower case.

    (symbol->string 'flying-fish)           ⇒  "flying-fish"
    (symbol->string 'Martin)                ⇒  "Martin"
    (symbol->string (string->symbol "Malvina"))
                                            ⇒  "Malvina"

### (intern _string_) ###

Returns the symbol whose name is _string_. This is the preferred way to create
symbols, as it guarantees the following independent of which case the
implementation uses for symbols' names:

    (eq? 'bitBlt (intern "bitBlt")) ⇒     #t

The user should take care that _string_ obeys the rules for identifiers,
otherwise the resulting symbol cannot be read as itself.

### (string->symbol _string_) ###
<p class="annotation">misc.scm</p>

Returns the interned symbol whose name is _string_. Although you can use this
procedure to create symbols with names containing special characters, it's
usually a bad idea to create such symbols because they cannot be read as
themselves. See `symbol->string`.

    (eq? 'mISSISSIppi 'mississippi)         ⇒  #t
    (string->symbol "mISSISSIppi")
         ⇒  the symbol with the name "mISSISSIppi"
    (eq? 'bitBlt (string->symbol "bitBlt")) ⇒  #t
    (eq? 'JollyWog
          (string->symbol
            (symbol->string 'JollyWog)))    ⇒  #t
    (string=? "K. Harper, M.D."
               (symbol->string
                 (string->symbol
                   "K. Harper, M.D.")))     ⇒  #t

### (gensym [_prefix_]) ###

Create a new, unique symbol made from the _prefix_ (or `GENSYM` if a
prefix is omitted) and an increasing integer. This is useful when you are
generating code and need a unique name (in a macro, for example).

    (gensym)      ⇒ GENSYM1
    (gensym)      ⇒ GENSYM2
    (gensym)      ⇒ GENSYM3

    (gensym "hi") ⇒ hi1
    (gensym "ho") ⇒ ho1
    (gensym "hi") ⇒ hi2
    (gensym "ho") ⇒ ho2
    (gensym "ho") ⇒ ho3
    (gensym "hi") ⇒ hi3

    (gensym)      ⇒ GENSYM4

### (symbol<? _symbol1_ _symbol2_) ###
<p class="annotation">misc.scm</p>

This procedure computes a total order on symbols. It is equivalent to

    (string<? (symbol->string symbol1)
              (symbol->string symbol2))


## Bytearrays ##

Bytearrays are an extension that GoLisp makes to Scheme 

### (list-to-bytearray _list of bytes and/or bytearrays_) ###

### (list->bytearray _list of bytes and/or bytearrays_) ###

The list must be comprised of elements that are either numbers between 0 and
255, inclusive, or existing bytearray objects. The result is an _object_
containing a `[]byte`.

    (list-to-bytearray '(1 2 3 4))     ⇒ [1 2 3 4]
    (list-to-bytearray '(1 [2 3] 4))   ⇒ [1 2 3 4]
    (list-to-bytearray '([1 2] [3 4])) ⇒ [1 2 3 4]

### (bytearray-to-list _bytearray_) ###

### (bytearray->list _bytearray_) ###

This is the opposite of the previous function. The result is a list containing
the numbers in the bytearray.

    (bytearray-to-list [1 2 3 4]) ⇒ (1 2 3 4)

### (replace-byte _bytearray_ _index_ _value_)

Makes a copy of _bytearray_ and replaces the byte at _index_ with _value_. The
new bytearray with the replaced byte is returned. _index_ must be a valid index
into the byte array (zero based), and _value_ must be a valid byte value, i.e.
between 0 and 255, inclusive.

    (define a [1 2 3 4])    ⇒ [1 2 3 4]
    (replace-byte a 2 100)  ⇒ [1 2 100 4]
    a                       ⇒ [1 2 3 4]

### (replace-byte! _bytearray_ _index_ _value_)

Replaces the byte at _index_ with _value_. _index_ must be a valid index into
the byte array (zero based), and _value_ must be a valid byte value, i.e.
between 0 and 255, inclusive. The original byte array is modified and the
returned bytearray object is the one that is passed to the function.

    (define a [1 2 3 4])    ⇒ [1 2 3 4]
    (replace-byte! a 2 100) ⇒ [1 2 100 4]
    a                       ⇒ [1 2 100 4]

### (extract-byte _bytearray_ _index_)

Fetch and return the byte at _index_. _index_ must be a valid index into the
byte array (zero based).

    (extract-byte [1 2 3 4] 2) ⇒ 3

### (append-bytes _bytearray_ _byte_...)

### (append-bytes _bytearray_ _list of bytes_)

### (append-bytes _bytearray_ _bytearray_...)

Appends the rest of the arguments to a copy of the bytearray that is the first
arg. The copy is returned. Things that can be appended are: a single byte, a
sequence of bytes (as a sequence of separate arguments), a list of bytes, a
bytearray object, a sequence of bytearray objects (as a sequence of separate
arguments), and code that evaluates to a byte, list of bytes, or bytearray.

    (append-bytes [1 2 3] 4)            ⇒ [1 2 3 4]
    (append-bytes [1 2 3] 4 5 6)        ⇒ [1 2 3 4 5 6]
    (append-bytes [1 2 3] '(4 5 6))     ⇒ [1 2 3 4 5 6]
    (append-bytes [1 2 3] [4 5 6])        ⇒ [1 2 3 4 5 6]
    (append-bytes [1 2 3] [4 5] [6])      ⇒ [1 2 3 4 5 6]
    (append-bytes [1 2 3] (list 4 5 6))  ⇒ [1 2 3 4 5 6]

### (append-bytes! _bytearray_ _byte_...)

### (append-bytes! _bytearray_ _list of bytes_)

### (append-bytes! _bytearray_ _bytearray_...)

As with `append-bytes`, but modifies and returns _bytearray_ rather than
making a copy.

    (define a [1 2 3])  ⇒ [1 2 3]
    (append-bytes a 4)  ⇒ [1 2 3 4]
    a                   ⇒ [1 2 3]
    (append-bytes! a 4) ⇒ [1 2 3 4]
    a                   ⇒ [1 2 3 4]

### (take _k_ _bytearray_)

As with the list implementation of `take`, fetches and returns a new bytearray
consisting of the bytes from _bytearray_ starting at index _k_.

    (take 1 [1 2 3] ⇒ [1])
    (take 3 [1 2 3] ⇒ [1 2 3])

### (drop _k_ _bytearray_)

    (drop 1 [1 2 3] ⇒ [2 3])
    (drop 2 [1 2 3] ⇒ [3])

As with the list implementation of `drop`, fetches and returns a new bytearray
consisting of the bytes from _bytearray_ prior to index _k_.

### (extract-bytes _bytearray_ _index_ _length_)

Returns a new bytearray consisting of _length_ bytes from _bytearray_, starting
at index _index_. This is functionally equivalent to `(take length (drop index
bytearray))` with bounds checking added.

    (extract-bytes [1 2 3 4 5] 0 1) ⇒ [1]
    (extract-bytes [1 2 3 4 5] 0 3) ⇒ [1 2 3]
    (extract-bytes [1 2 3 4 5] 2 1) ⇒ [3]
    (extract-bytes [1 2 3 4 5] 2 3) ⇒ [3 4 5]

# Environments #

Scheme (and thus GoLisp) is lexically scoped. This is implemented by the
creation of a lexical environment (aka symbol table) for each lexical scope:

* function/lambda/macro invocations, which holds parameters and any local
  definitions
* `let` structures, which hold the `let` bindings
* `do` structures, which hold the `do` bindings

Functions and lambdas capture a reference to the environment in which they were
defined, so they always have access to it's bindings (that's a closure, btw).

Each environment has a connection to it's containing environment, and can
override/hide bindings in outer scopes. When a symbol is evaluated, the most
local environment is searched first. If a binding for the system isn't found
there, the containing environment is searched. This continues until a binding
for the sybol is found or we go all the way to the global environment and still
can't find a binding.

Section 3.2 of \[1\] does a great job of explaining environments in Scheme,
which is the basis for environments in GoLisp.

In Scheme, some environments are more important than others, mainly as they tend
to be larger, long lived, and serve as the root of many other environments as a
program runs. These are known as *top level environments*. Specifially, these
are the global environment (the only environment that is contained by nothing),
and any environments directly below it in the environment tree. The REPL runs in
one such environment, which effectively sandboxes it, protecting the bindings in
the global environment from corruption.


### (environment? _object_) ###

Returns `#t` if _object_ is an environment; otherwise returns `#f`.

### (environment-has-parent? _environment_) ###

Returns `#t` if _environment_ has a parent environment; otherwise returns `#f`.

### (environment-parent _environment_) ###

Returns the parent environment of _environment_. It is an error if _environment_
has no parent.

### (environment-bound-names _environment_) ###

Returns a newly allocated list of the names (symbols) that are bound by
_environment_. This does not include the names that are bound by the parent
environment of _environment_. It does include names that are unassigned or
keywords in _environment_.

### (environment-macro-names _environment_) ###

Returns a newly allocated list of the names (symbols) that are bound to
syntactic keywords in _environment_.

### (environment-bindings _environment_) ###

Returns a newly allocated list of the bindings of _environment_; does not
include the bindings of the parent environment. Each element of this list takes
one of two forms: `(symbol)` indicates that _symbol_ is bound but unassigned,
while `(symbol object)` indicates that _symbol_ is bound, and its value is
`object`.

### (environment-reference-type _environment_ _symbol_) ###

Returns a symbol describing the reference type of _symbol_ in _environment_ or
one of its ancestor environments. The result is one of the following:

* `normal` means _symbol_ is a variable binding with a normal value.
* `unassigned` means _symbol_ is a variable binding with no value.
* `macro` means _symbol_ is a keyword binding.
* `unbound` means _symbol_ has no associated binding.

### (environment-bound? _environment_ _symbol_) ###

Returns `#t` if _symbol_ is bound in _environment_ or one of its ancestor
environments; otherwise returns `#f`. This is equivalent to

    (not (eq? ’unbound
              (environment-reference-type environment symbol)))

### (environment-assigned? _environment_ _symbol_) ###

Returns `#t` if _symbol_ is bound in _environment_ or one of its ancestor
environments, and has a normal value. Returns `#f` if it is bound but
unassigned. Signals an error if it is unbound or is bound to a keyword.

### (environment-lookup _environment_ _symbol_) ###

_symbol_ must be bound to a normal value in _environment_ or one of its ancestor
environments. Returns the value to which it is bound. Signals an error if
unbound, unassigned, or a keyword.

### (environment-lookup-macro _environment_ _symbol_) ###

If _symbol_ is a keyword binding in _environment_ or one of its ancestor
environments, returns the value of the binding. Otherwise, returns `#f`. Does
not signal any errors other than argument-type errors.

### (environment-assignable? _environment_ _symbol_) ###

_symbol_ must be bound in _environment_ or one of its ancestor environments.
Returns `#t` if the binding may be modified by side effect.

### (environment-assign! _environment_ _symbol_ _value_) ###

_symbol_ must be bound in _environment_ or one of its ancestor environments, and
must be assignable. Modifies the binding to have _value_ as its value, and
returns an unspecified result.

### (environment-definable? _environment_ _symbol_) ###

Returns `#t` if _symbol_ is definable in _environment_, and `#f` otherwise.

### (environment-define _environment_ _symbol_ _value_) ###

Defines _symbol_ to be bound to object in _environment_, and returns an
unspecified value. Signals an error if _symbol_ isn’t definable in
_environment_.

### (eval _sexpr_ _environment_) ###

Evaluates _sexpr_ in _environment_. You rarely need eval in ordinary programs;
it is useful mostly for evaluating expressions that have been created “on the
fly” by a program.

### (system-global-environment) ###

The function `system-global-environment` is returns the distinguished
environment that’s the highest level ancestor of all other environments. It is
the parent environment of all other top-level environments. Primitives, system
procedures, and most syntactic keywords are bound in this environment.

### (the-environment) ###

Returns the current environment. This form may only be evaluated in a top-level
environment. An error is signalled if it appears elsewhere.

### (procedure-environment _procedure_) ###

Returns the closing environment of _procedure_. Signals an error if _procedure_
is a primitive procedure.

### (make-top-level-environment [_names_ [_values_]]) ###

Returns a newly allocated top-level environment. extend-top-level-environment
creates an environment that has parent environment, make-top-level-environment
creates an environment that has parent system-global-environment, and make-
root-top-level-environment creates an environment that has no parent.

The optional arguments _names_ and _values_ are used to specify initial bindings
in the new environment. If specified, _names_ must be a list of symbols, and
_values_ must be a list of objects. If only _names_ is specified, each name in
_names_ will be bound in the environment, but unassigned. If _names_ and
_values_ are both specified, they must be the same length, and each name in
_names_ will be bound to the corresponding value in _values_. If neither _names_
nor _values_ is specified, the environment will have no initial bindings.

Environments in GoLisp differ slightly from standard Scheme in that they have a
name attached. For the various forms of `let` and `do` this is simply `"let"`
and `"do"`, respectively. Not of much use, but then these are just a byproduct
of having lexical scopes. What's more useful is the higher level environments.
This brings us to the real reason for adding environment support: game
integration sandboxes. When we were writing the game integration functionallity
for Engine3, we wanted each game's event handling to live in a separate sandbox.
This is implemented buy creating a new top level environment under the global
environment. The problem here is that it's off in it's own world, separate from
the repl. By naming environments (in this case by the name of the game), we can
add a function to return an environment given it's name. That allows us to peek
inside the sandbox from the repl, examining and manipulating the bindings there.
And so we added a function to let us do that:

### (find-top-level-environment _name_) ###

Returns the top level environment with the given name.

# Utility #

GoLisp provides a handful of utility functions.

### (random-byte) ###

Returns a psuedo-random unsigned integer between 0 and 255, inclusive.

    (random-byte) ⇒ 13
    (random-byte) ⇒ 207

### (random) ###

The return value is a pseudorandom integer in the range [0, 2,147,483,647]

### (random _modulus_) ###

_Modulus_ must be a positive real number. If _modulus_ is an integer, `random` returns a pseudo-random number between zero (inclusive) and _modulus_ (exclusive). If _modulus_ is the float 1.0, the returned number is a float in the range [0.0, 1.0). Other float values of _modulus_ are rejected.

### (sleep _millis_)

Sleep for _millis_ milliseconds.

    (sleep 1000)  ;; resumes execution 1 second later

### (time _expression_...)

Evaluates each _expression_ and reports the number of milliseconds taken to do so.

### (write-line _object_...)

Writes the concatenation of the string forms of _objects_ followed by a newline.

    > (write-line "Hello, " 42 " world")
    Hello, 42 world
    =⇒ ()

### (str _object_...)

If you provide multiple arguments to `str` it creates a string from
concatenating the string forms of all the _objects_.

    (str 1 "." 2) ⇒ "1.2"

### (copy _object_)

Make a copy of the result of evaluating _object_, IFF it’s mutable. This is
limited to lists and association lists. All other values are immutable. Copying
an immutable item will return the item, whereas copying a list or association
list will make a deep copy of the structure, and return it.

### (exec _command_ _arg_...) ###

Makes an operating system call. `command` is the command to execute and the
`arg`s are the arguments passed on the command line to `command`. `command` must
be a string, and the `arg`s can be anything.

# Concurrency #

GoLisp has limited concurrency support that is built on top of goroutines and
channels.

## Process management ##

These functions make use of a *process* object. This is an opaque piece of data
that wraps a custom structure used by the concurrency code; it is returned from
`fork` and `schedule` and is used by `proc-sleep`, `wake`, and `abandon` to
interact with the underlying goroutine.

### (fork _function_) ###

Executes _function_ in a separate goroutine. When _function_ ends, the goroutine
terminates. _function_ takes a single argument which is the process object that
is returned.

    (define (run-once proc)
      (write-line "start")
      (sleep 1000)
      (write-line "stop"))

    (fork run-once)

    > start
    [a second goes by]
    stop
    [run-once completes and the goroutine terminates]

### (proc-sleep _process_ _millis_) ###

Use `proc-sleep` in a forked function to sleep for _millis_ milliseconds. Using
`proc-sleep` rather than `sleep` (which can be used) allows code in another
process (that has a reference to the process object of the forked code) to
preemptively terminate the sleep using the `wake` function.

`proc-sleep` returns a boolean that indicates whether the sleep was terminated
using `wake`.

### (wake _process_) ###

Preemptively terminate a `proc-sleep` in the code associated with _process_.

    > (define (run proc)
        (do ((woken #f woken))
            (woken (write-line "woken"))
          (write-line "tick")
          (set! woken (proc-sleep proc 10000))))

    > (define p (fork run))

    tick
    tick
    [times goes by, tick is printed every 10 seconds]
    > (wake p)
    woken
    [run completes and the goroutine terminates]

### (join _process_) ###

Blocks the calling function until the process completes or aborts with an error.
The return value of the process is returned, or nil if the process ran into an
error and aborted. Attempting to call `join` on a process twice raises an error.

    (define (run proc) '(1 2 3))
    (define p (fork run))
    (join p) ⇒ (1 2 3)

### (schedule _millis_ _function_) ###

Schedule _function_ to be evaluated in a separate goroutine _millis_
milliseconds from now. _function_ takes a single argument which is the process
object that is returned. The process object associated with that goroutine is
returned immediately.

    > (define (run-delayed proc)
        (write-line "running"))

    > (schedule 10000 run-delayed)
    [10 seconds pass]
    running
    [run-delayed completes and the goroutine terminates]

### (abandon _process_) ###

Cancels the scheduled evaluation associated with _process_.

    > (define (run-delayed proc)
        (write-line "running"))

    > (define p (schedule 10000 run-delayed))
    [5 seconds pass]
    > (abandon p)
    [the delay is cancelled and the goroutine terminates]

### (reset-timeout _process_) ###

Resets the timer on a scheduled process. Causing it to start over. You can use
this function to postpone the evaluation of scheduled code.

    > (define (run-delayed proc)
        (write-line "running"))

    > (define p (schedule 10000 run-delayed))
    [less than 10 seconds pass]
    > (reset-timeout p)
    [10 seconds pass]
    running
    [run-delayed completes and the goroutine terminates]

## Atomic Operations ##

GoLisp has support for several kinds of atomic operations. These can be useful
for protecting memory when working with GoLisp code with concurrent processes,
or just Go code with multiple goroutines.

These functions make use of a _atomic_ object. This is an opaque piece of data
that wraps an integer; it is returned from `atomic` and is used by all the
`atomic-*` primitives to interact with the underlying integer using only atomic
operations.

### (atomic [_value_]) ###

Creates a new *atomic* object and returns it. It can optionally be passed a
starting value to initialize to. Otherwise, the starting value is 0.

    > (atomic)   ⇒ <atomic object with value 0>
    > (atomic 5) ⇒ <atomic object with value 5>

### (atomic-load _atomic_) ###

Loads the current integer value of the _atomic_ object and returns it as an
integer.

    > (define a (atomic 5))
    > (atomic-load a) ⇒ 5

### (atomic-store! _atomic_ _new_) ###

Stores a new integer value in a _atomic_ object.

    > (define a (atomic 5))
    > (atomic-store! a 8)
    > (atomic-load a) ⇒ 8

### (atomic-add! _atomic_ _delta_) ###

Adds the _delta_ value to the one stored in the _atomic_ object. The new sum is
also returned.

    > (define a (atomic 5))
    > (atomic-add! a 4) ⇒ 9
    > (atomic-load a)   ⇒ 9

### (atomic-swap! _atomic_ _new_) ###

Swaps the value currently in the _atomic_ object with a new value. The old value
is returned.

    > (define a (atomic 5))
    > (atomic-swap! a 4) ⇒ 5
    > (atomic-load a)    ⇒ 4

### (atomic-compare-and-swap! _atomic_ _old_ _new_) ###

The value in the _atomic_ object is compared to _old_. If the value matches, the
value in the _atomic_ object is swapped with the value in _new_ and true is
returned. Otherwise, the values are not swapped and false is returned.

    > (define a (atomic 5))
    > (atomic-compare-and-swap! a 5 4) ⇒ #t
    > (atomic-load a)                  ⇒ 4

    > (define b (atomic 5))
    > (atomic-compare-and-swap! b 9 4) ⇒ #f
    > (atomic-load b)                  ⇒ 5

## Channels ##

Channels are the main way you communicate between goroutines in Go. GoLisp has
full support of channels.

### (make-channel [_buffer-size_]) ###

Creates a new channel object with an optional buffer size. If _buffer-size_ is
omitted or 0, the channel is unbuffered.

### (channel-write _channel_ _value_) ###

### (_channel_<- _value_) ###

Writes a value to a channel. If the channel is unbuffered or has a full buffer,
this call locks until there either another process tries to read from the
channel or room is made in the buffer.

### (channel-read _channel_) ###

### (<-_channel_) ###

Reads a value from a channel. If the channel is unbuffered or has no buffered
data, this call locks until there is data in the channel. `<-channel` returns
two values. The first value is the data read from the channel. The second value
is a boolean flag stating whether there is more data in the channel. If the
channel is closed and there are no more items left in the buffer, a false flag
is returned. Otherwise, a true flag is returned. If a flag of false is returned,
the first value will also be nil.

    > (define c (make-channel 1))
    > (channel-write c 1)
    > (c<- 1) ; alternate syntax for the previous line
    > (channel-read c) ⇒ (1 #t)
    > (<-c)            ⇒ (1 #t) ; alternate syntax for the previous line
    > (channel-read c) ; blocks until another process writes to c

### (channel-try-write _channel_ _value_) ###

Tries to write a value to a channel. If the channel is unbuffered with nobody
waiting for a write or has a full buffer, it returns immediately a false value.
Otherwise, it writes the value to the channel and returns a true value.

   > (define c (make-channel 1)
   > (channel-try-write c 1) ⇒ #t

   > (define c (make-channel))
   > (channel-try-write c 1) ⇒ #f

### (channel-try-read _channel_) ###

Tries to reads a value from a channel. This call returns three values. The first
is whether data could be read or not. The second is the data that is read, or
nil if none was. The last value is whether the channel has more data in it.

    > (define c (make-channel 1))
    > (c<- 1)
    > (channel-try-read c) ⇒ (#t 1 #t)
    > (channel-try-read c) ⇒ (#f () #t)

### (close-channel _channel_) ###

Closes the specified channel. The channel's buffered is cleared by any other
goroutines trying to read from it then all other reads immediately return with
the more flag set to false. Trying to write to a closed channel or trying to
close a channel twice results in an error.

    > (define c (make-channel 1))
    > (c<- 1)
    > (close-channel c)
    > (<-c) ⇒ (1 #t)
    > (<-c) ⇒ (() #f)
    > (<-c) ⇒ (() #f) ; repeats on subsequent calls
    > (channel-try-read c) ⇒ (#t () #f)

# Timers #

### (timer _millis_ _func_) ###

Schedules _func_ (a function of zero arguments) to be evaluated after _millis_ milliseconds. Returns a timer object.

### (stop-timer _timer_)###

Stop and cancel the timer _timer_.

### (ticker _millis_ _func_) ###

Schedules _func_ (a function of zero arguments) to be evaluated after _millis_ milliseconds and every _millis_ milliseconds thereafter until stopped. Returns a ticker object.

### (stop-ticker _ticker_)###

Stop and cancel the ticker _ticker_.

# Input/Output #

### (open-input-file _filename_) ###

Takes a _filename_ referring to an existing file and returns an input port
capable of delivering characters from the file.

### (open-output-file _filename_ [_append?_]) ###

Takes a _filename_ referring to an output file to be created and returns an
output port capable of writing characters to a new file by that name.

If _append?_ is given and not `#f`, the file is opened in append mode. In this
mode, the contents of the file are not overwritten; instead any characters
written to the file are appended to the end of the existing contents. If the
file does not exist, append mode creates the file and writes to it in the normal
way.

### (close-port _port_) ###

Closes _port_ and returns an unspecified value. The associated file is also
closed.

### (write-bytes _byte-array_ _output-port_) ###

Writes _byte-array_ to _output-port_ as a stream of raw bytes. Most usefull for
interacting with external devices via serial/usb ports.

### (write-string _string_ [_output-port_]) ###

Writes _string_ to _output-port_, performs discretionary output flushing, and
returns an unspecified value.

### (newline [_output-port_]) ###

Writes an end-of-line to _output-port_, performs discretionary output flushing,
and returns an unspecified value.

### (write _object_ [_output-port_]) ###

Writes a written representation of _object_ to _output-port_, and returns an
unspecified value. If _object_ has a standard external representation, then the
written representation generated by `write` shall be parsable by `read` into an
equivalent object. Thus strings that appear in the written representation are
enclosed in doublequotes, and within those strings backslash and doublequote are
escaped by backslashes. `write` performs discretionary output flushing and
returns an unspecified value.

### (read-string [_input-port_]) ###

Reads characters from _input-port_ until it finds a terminating character or
encounters end of line. The port is updated to point to the terminating
character, or to end of line if no terminating character was found.
`read-string` returns the characters, up to but excluding the terminating
character, as a newly allocated string.

### (read [_input-port_]) ###

Converts external representations of Scheme objects into the objects themselves.
read returns the next object parsable from _input-port_, updating _input-port_
to point to the first character past the end of the written representation of
the object. If an end of file is encountered in the input before any characters
are found that can begin an object, read returns an end-of-file object. The
_input-port_ remains open, and further attempts to read will also return an
end-of-file object. If an end of file is encountered after the beginning of an
object’s written representation, but the written representation is incomplete
and therefore not parsable, an error is signalled.

### (eof-object? _object_) ###

Returns `#t` if _object_ is an end-of-file object; otherwise returns `#f`.

### (format _destination_ _control-string_ _argument_...) ###

Writes the characters of _control-string_ to _destination_, except that a tilde
(`~`) introduces a format directive. The character after the tilde, possibly
preceded by prefix parameters and modifiers, specifies what kind of formatting
is desired. Some directives use an _argument_ to create their output; the
typical directive puts the next _argument_ into the output, formatted in some
special way. It is an error if no _argument_ remains for a directive requiring
an _argument_.

The output is sent to _destination_. If _destination_ is `#f`, a string is
created that contains the output; this string is returned as the value of the
call to `format`. If _destination_ is `#t`, the output is sent to `stdout`. In
all other cases `format` returns an unspecified value. Otherwise, destination
must be an output port, and the output is sent there.

A format directive consists of a tilde (~), an optional prefix parameter, an
optional at-sign (`@`) modifier, and a single character indicating what kind of
directive this is. The alphabetic case of the directive character is ignored.
The prefix parameters are generally integers, notated as optionally signed
decimal numbers.

In place of a prefix parameter to a directive, you can put the letter `V` (or
`v`), which takes an argument for use as a parameter to the directive. Normally
this should be an integer. This feature allows variable-width fields and the
like. You can also use the character `#` in place of a parameter; it represents
the number of arguments remaining to be processed.

~A: The next argument, which may be any object, is printed as if by write-line.
`~mincolA` inserts spaces on the right, if necessary, to make the width at least
mincol columns. The `@` modifier causes the spaces to be inserted on the left
rather than the right.

~S: The next argument, which may be any object, is printed as if by write (in as
read-able format as possible). `~mincolS` inserts spaces on the right, if
necessary, to make the width at least mincol columns. The `@` modifier causes
the spaces to be inserted on the left rather than the right.

~%: This outputs a newline character. This outputs a #\newline character. `~n%`
outputs n newlines. No argument is used. Simply putting a newline in
control-string would work, but `~%` is often used because it makes the control
string look nicer in the middle of a program.

~~: This outputs a tilde. `~n~` outputs n tildes.

~newline: Tilde immediately followed by a newline ignores the newline and any
following whitespace characters. With an `@`, the newline is left in place, but
any following whitespace is ignored. This directive is typically used when
control-string is too long to fit nicely into one line of the program:

    (define (type-clash-error procedure arg spec actual)
       (format
        #t
        "~%Procedure ~S~%requires its %A argument ~
         to be of type ~S,~%but it was called with ~
         an argument of type ~S.~%"
        procedure arg spec actual))
    (type-clash-error ’vector-ref
                      "first"
                      ’integer
                      ’vector)

prints

    Procedure vector-ref
    requires its first argument to be of type integer,
    but it was called with an argument of type vector.

Note that in this example newlines appear in the output only as specified by the
~% directives; the actual newline characters in the control string are
suppressed because each is preceded by a tilde.

# Testing

Golisp has a builtin testing framework, completely written in GoLisp.

## Structure ##

Contests and it-clauses divide up the testing of a system under test into
fixtures and focused sets of assertions.

### (context _tag-string_ _fixture_ _it_...) ###

_tag-string_ is a string used to identify the context in the test run's output.
This should describe what the context focusses on. _fixture_ is a sequence of
expressions that typically add definitions (symbol bindings) to the environment
created by `context`. Each _it_ expression (defined using `it`) performs
speific tests relevant to the context.

### (it _tag_ _assertion_...) ###

This defines a cohesive block of assertions. The context's fixture code will be
run in a new environment for each _it_ block, thus isolating each it.

## Assertions ##

Assertions are used to make provable (via execution) statements about the system
under test.

### (assert-true _expression_) ###

Passes if _expression_ evaluates to a truthy value, fails otherwise.

### (assert-false _expression_) ###

Passes if _expression_ evaluates to a falsy value, fails otherwise.

### (assert-eq _actual_ _expected_) ###

Passes if the result of evaluating _actual_ is equal (using `(equal? _actual_
_expected_)`) to the result of evaluating _expected_, fails otherwise.

### (assert-neq _actual_ _expected_) ###

Passes if the result of evaluating _actual_ is **not** equal (using `(not
(equal? _actual_ _expected_))`to the result of evaluating _expected_, fails
otherwise.

### (assert-nil _expression_) ###

Passes if _expression_ evaluates to nil, fails otherwise.

### (assert-not-nil _expression_) ###

Passes if _expression_ evaluates to anything **other than** nil, fails otherwise.

### (assert-error _expression_) ###

Passes if evaluating _expression_ results in an error being signalled, fails if it
evaluates without problems.

### (assert-nerror _expression_) ###

Passes if evaluating _expression_ does not result in an error being signalled, fails if it
the evaluation signals an error.

## Usage ##

Generally you should create a test file for each feature you are testing. The
file is a plain lisp file and can contain any lisp code, including global
variable and function definitions.

For example, here is the test file for scoping:

    (context "environments"

      ((define a 5)
       (define (foo a)
         (lambda (x)
          (+ a x))))

      (it "can access a in the global env"
          (assert-eq a 5))

      (it "gets a from the function's local env"
          (assert-eq ((foo 1) 5) 6)
          (assert-eq ((foo 2) 5) 7)
          (assert-eq ((foo 10) 7) 17)))

Running a test results in a stream of status output for each test, followed at
the very end by a summary. Running the above results in the following:

    environments

      can access a in the global env
        (assert-eq a 5)

      gets a from the function's local env
        (assert-eq ((foo 1) 5) 6)
        (assert-eq ((foo 2) 5) 7)
        (assert-eq ((foo 10) 7) 17)

    Ran 4 tests in 0.003 seconds
    4 passes, 0 failures, 0 errors

If we introduce a failure, the output would be:

    environments

      can access a in the global env
        (assert-eq a 5)

      gets a from the function's local env
        (assert-eq ((foo 1) 5) 6)
        (assert-eq ((foo 2) 5) 8)
          - expected 8, but was 7
        (assert-eq ((foo 10) 7) 17)

    Ran 4 tests in 0.002 seconds
    3 passes, 1 failures, 0 errors

    Failures:
      environments gets a from the function's local env:
        (assert-eq ((foo 2) 5) 8)
          - expected 8, but was 7

Errors are also reported. Errors are problems that occur while evaluating the
clauses, that aren't failures. Essentially they indicate bugs of some sort.

    environments

      can access a in the global env
        (assert-eq a 5)

      gets a from the function's local env
        (assert-eq ((foo 1) 5) 6)
        ERROR: Quotent: (7 0) -> Divide by zero.

    Ran 3 tests in 0.002 seconds
    2 passes, 0 failures, 1 errors

    Errors:
      environments gets a from the function's local env:
        ERROR: Quotent: (7 0) -> Divide by zero.


The above output was generated by the testing framwork running in verbose mode.
You can also run in quiet mode which only outputs the summary:

    Ran 4 tests in 0.003 seconds
    4 passes, 0 failures, 0 errors

You run tests by running the golisp repl in test mode, providing either a
directory or filename. If you provide a directory all files in it that match
`*_test.scm` will be run. If you provide a filename, only that file will be run.

    $golisp -t tests/scope_test.scm

    Ran 4 tests in 0.002 seconds
    4 passes, 0 failures, 0 errors


    $golisp -t tests

    Ran 935 tests in 0.273 seconds
    935 passes, 0 failures, 0 errors

Adding the `-v` flag will produce the detailed output above.

# Extending GoLisp #

## Defining primitives ##

The Go function `MakePrimitiveFunction` allows you to create primitive functions.

    MakePrimitiveFunction(name string, argCount string,
                          function func(*Data, *SymbolTableFrame)(*Data, error))

The arguments are:

1. The function name. This is the name of a symbol which will be used to
   reference the function.

2. An argument count expectation. This is a string that specifies how many
   arguments the primitive expects. It can take several forms:
* A single, specific number. E.g. exactly two: `"2"`
* A minimum number. E.g. at least two: `">=2"`
* A range of values. E.g. between two and five, inclusive: `"(2,5)"`
* One of a selection of the above: E.g. `"2|3|>=5"`
* An unspecified number, any checking must be done in the primitive definition: `"*"`

3. The Go function which implements the primitive. This function **must** have
the signature

        func <Name>(args *Data, env *SymbolTableFrame) (*Data, error)

The implementing function takes two parameters as seen above:

1.  A Lisp list containing the arguments

2. The environment in which the primitive is being evaluated. This is used when
calling `Eval` or `Apply`, as well as for any symbol lookups or bindings.

Primitives use, like functions defined in LISP, applicative evaluation order.
That means that all arguments are evaluated and the resulting values passed to
the function. This frees you from having to evaluate the arguments and handle
errors. You still have to verify the number of arguments (only if you used -1 as
trhe argument cound in the `MakePrimitiveFunction` call) and their type, if
aplicable.

    MakeSpecialForm(name string,
                    argCount int,
                    function func(*Data, *SymbolTableFrame) (*Data, error))

An example:

    MakePrimitiveFunction("!", "1", BooleanNot)

    func BooleanNot(args *Data, env *SymbolTableFrame) (result *Data, err error) {
        val := BooleanValue(First(args))
        return BooleanWithValue(!val), nil
     }

You can extend the goLisp runtime without changing any of it's code. You simply
import the golisp package (typically aliased to `.` to make the code less noisy)
and place calls to `MakePrimitiveFunction` in your package's `init` block.

## Defining primitives with argument type checking ##

There is also the `MakeTypedPrimitiveFunction` function that takes an additional argument which is an array of `uint32`s, one element for each argument. If the defined function accepts an arbitrary number of arguments, the final type specification is used for the remainder. For example, if there are 3 argument type specifications and the function is passed 5 arguments, the final specification is used for the 3rd, 4th, and 5th arguments.

```
MakeTypedPrimitiveFunction("mqtt/publish", "3", mqttPublishImpl, []uint32{StringType, IntegerType, StringType})
```

There is not currently a way to provide a type specification for a primitive's return value.

## Defining special forms ##

There is another, very similar function that you will typically not need unless
you are hacking on the language itself (as opposed to adding builting
functions):

    MakeSpecialForm(name string,
                    argCount int,
                    function func(*Data, *SymbolTableFrame) (*Data, error))

Arguments and the signature of the implementing function are identical to
`MakePrimitiveFunction`. The only difference is that this defines a _special
form_ which uses normal evaluation order. I.e. arguments are not evaluated
before calling the function; the raw sexpressions are passed in. Thus the
implementing function has full control over what gets evaluated and when. For
example:

    MakeSpecialForm("if", "2|3", IfImpl)

    func IfImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
        c, err := Eval(First(args), env)
        if err != nil {
            return
        }

        if BooleanValue(c) {
            return Eval(Second(args), env)
        } else {
            return Eval(Third(args), env)
        }
    }

## Data ##

The core lisp data element is the data type which logically contains a type tag
and a value. The type tags are defined by the constants: `ConsCellType`,
`NumberType`, `BooleanType`, `StringType`, `SymbolType`, `FunctionType`,
`PrimitiveType`, `ObjectType`. As the languae evolves this list (and the
selection of functions described below) will change. Refer to the file `data.go`
for the definitive information.

The types are described earlier. If you need to check the type of a piece of
data you can fetch it’s type using the `TypeOf(*Data) int` function and then
compare it to a type tag constant. Additionally there are predicate functions
for the most common types that have the general form:

<Tyle>P(*Data) bool** returns whether the argument is of the type <Type>. E.g.

    SymbolP(*Data)


Two other very handy functions are:

**NilP(*Data) bool** returns whether the data is nil

**NotNilP(*Data) bool** returns whether the data is non-nil

## Creating and accessing data ##

There are various convenience functions that you can use to create data:

**Cons(car *Data, cdr *Data) *Data** creates a cons cell with the provided values
for it’s `car` and `cdr`.

For other types you can use functions with the general form:

    <Type>WithValue(...) *Data

These construct the corresponding Lisp data object. Each of these functions take an argument of the appropriate type.  Again, see `data.go` for up-to-date specifics.

There are two of these that don't fully conform. Uninterned systems can be created with

    SymbolWithName(s string) *Data

but that isn't overly useful. If you need to create symbol you should use the `Intern` function which ensures that each interns symbol is a singleton.  I.e. interning the same symbol name multiple times always results in the same symbol object. This has several advantages, not the least of which is efficient symbol comparison.

The other differing function is for wrapping Go objects. With this you need to supply a type name (as a string) and a pointer to the data:

    ObjectWithTypeAndValue(typeName string, o unsafe.Pointer) *Data

An example use from the `list->bytearry` primitive:

    ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&bytes))

Similarly there are functions for extracting information from data elements, their return types are whatever is appropriate for the data:

    <Type>Value(*Data) ...

Once again, wrapped Go objects are handled specially with two function to extract the type and pointer, respectively:

    TypeOfObject(*Data) string
    
    ObjectValue(*Data) unsafe.Pointer

Any of the above `...Value` functions return the corresponding zero value if the
data is not of the appropriate type.

To convert a Lisp data object to a string, use:

    String(*Data) string

To find the _length_ of a piece of Lisp data, use:

    Length(*Data) int

This will return 0 in all cases except lists, vectors, or frames.  For those three it will be the number of cons cells, elements, or slots, respecively .

There are three equality predicates that are availavle. These are the low level implementation of the corresponding equality predicates.

    IsEqv(*Data, o *Data) bool
    IsEq(*Data, o *Data) bool
    IsEqual(*Data, o *Data) bool

## Working with lists

**Car(*Data) *Data** return the `car` pointer from a cons cell (nil otherwise)

**Cdr(*Data) *Data)** return the `cdr` pointer from a cons cell (nil otherwise)

**Caar** through **Cddddr** returns a piece of the list as expected

**First(*Data) *Data** Return the first item in a list (equivalent to `car`)
...
**Tenth(*Data) *Data** return the fifth item in a list

**Nth(*Data, int) *Data** return the nth item in a list (starting at 0)

**ArrayToList(sexprs []*Data) *Data** converts an array of Lisp data objects to a
Lisp list.

**ToArray(list *Data)** converts a proper list to an `[]*Data`.

## Evaluation ##

These functions are the entry points into code evaluation. The main reason to
use these, and especially `Eval`, is to evaluate arguments to primitives as
required.

**Eval(d *Data, env *SymbolTableFrame) (*Data, error)** evaluates the expression
in `d` in the provided environment and returns the result along with any error
that occurred during evaluation. If an error is returned, the value of the data
result is indeterminate.

**Apply(function *Data, args *Data, env *SymbolTableFrame) (*Data, error)** takes
a function element, either user defined or primitive, and applies it to the
provided arguments in the provided environment. This is actually how `Eval`
deals with a list: the evaluation of the `car` of the list is the function, the
`cdr` is the list of arguments. It’s up to the primitive implementation to
evaluate arguments as required.

The implementation of `if` serves as an example:

    func IfImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
      c, err := Eval(Car(args), env)
      if err != nil {
          return
      }

      if BooleanValue(c) {
          return Eval(Cadr(args), env)
      } else {
          return Eval(Caddr(args), env)
      }
    }

Note that **Cadr** and **Caddr** return `nil` if the corresponding element is
missing from the argument list. Also, passing a `nil` to **Eval** returns `nil`.

## Embedded GoLisp ##

Now that you know what’s supported in GoLisp, and how to manipulate it’s
structures and code in from Go, you need to get code into it. There are two ways
to do this.

## Single expression strings ##

If you have a Go `string` that contains a single sexpr, you can pass it to
**Parse** which returns the list containing the parsed string and an error value.
The returned structure can be passed to **Eval** to evaluate it. The resulting
sexpr is returned.

For example, here is the REPL, slightly simplified:

    prompt := "> "
    LoadHistoryFromFile(".golisp_history")
    lastInput := ""
    for true {
        input := *ReadLine(&prompt)
        if input != "" {
            if input != lastInput {
                AddHistory(input)
            }
            lastInput = input
            code, err := Parse(input)
            if err != nil {
                fmt.Printf("Error: %s\n", err)
            } else {
                d, err := Eval(code, Global)
                if err != nil {
                    fmt.Printf("Error in evaluation: %s\n", err)
                } else {
                    fmt.Printf("⇒ %s\n", String(d))
                }
            }
        }
    }

## Loading a file ##

If you have GoLisp code in a plain text file you can pass the filename to
**ProcessFile**. This will parse and evaluate each sexpr contained in the file.
The result of **ProcessFile** is the value of the evaluation of the final sexpr in
the file and an error value.

Generally, you load a file for it’s side effects: defining values and functions.

# The REPL #

You typically import the `golisp` package into your Go app and use it. However,
you can run the `golisp/main/golisp` package, which puts you into the REPL where
you can enter and evaluate one line at a time. You can use the `load` function
to load/evaluate files of golisp code you have written. If you provide a list of
filenames on the command line they will be loaded and evaluated before dropping
you into the REPL.

Note that you can easily make the REPL available to the user from within your
application.

# References #

[1] Harold Abelson, Gerald Jay Sussman, and Julie Sussman. *Structure and
Interpretation of Computer Programs*. MIT Press, Cambridge, Mass., 1985.

[2] Richard P. Gabriel and Kent M. Pitman. Endpaper: Technical issues of
separation in function cells and value cells. *Lisp and Symbolic Computation*,
1(1):81–101, June 1988.

[3] Guy L. Steele. *COMMON LISP: the language*. Digital Press, 12 Crosby Drive,
Bedford, MA 01730, USA, 1984. With contributions by Scott E. Fahlman and Richard
P. Gabriel and David A. Moon and Daniel L. Weinreb.

[4] David Ungar and Randall B. Smith. self: the power of simplicity. In *OOPSLA
87 Conference Proceedings*, pages 227–241, Orlando, Florida, October 1987.

[5] Apple Computer. *The NewtonScript Programming Language*. Apple Computer,
Cupertino, Ca., 1996.

[6] Chris Hanson and the MIT Scheme Team. *MIT/GNU Scheme Reference Manual*,
Edition 1.105 for release 9.2. Massachusetts Institute of Technology. May 2014.
