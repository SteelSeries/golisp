// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file pre-loads primitive builtin functions

// Basic arithmetic is implimented, with skeletons for the rest of the "special" symbols.
// Flesh out as required. Remember to add tests to builtins_test.go

package golisp

import (
    "container/list"
    "errors"
    "fmt"
)

func init() {
    symbolTable = &SymbolTable{list.New()}
    PushLocalBindings()
    InitBuiltins()
}

func InitBuiltins() {
    // MakePrimitiveFunction(<symbol>, <required # args, -1 means >= 1>, <function>)
    Intern("nil")
    MakePrimitiveFunction("+", -1, Add)
    MakePrimitiveFunction("-", -1, Subtract)
    MakePrimitiveFunction("*", -1, Multiply)
    MakePrimitiveFunction("/", -1, Quotient)
    MakePrimitiveFunction("%", 2, Remainder)
    MakePrimitiveFunction("<", -1, LessThan)
    MakePrimitiveFunction(">", -1, GreaterThan)
    MakePrimitiveFunction("==", 2, EqualTo)
    MakePrimitiveFunction("!=", 2, NotEqual)
    MakePrimitiveFunction("<=", -1, LessThanOrEqualTo)
    MakePrimitiveFunction(">=", -1, GreaterThanOrEqualTo)
    MakePrimitiveFunction("!", 1, BooleanNot)
    MakePrimitiveFunction("if", -1, If)
    MakePrimitiveFunction("lambda", -1, Lambda)
    MakePrimitiveFunction("define", 2, Define)
    MakePrimitiveFunction("dump", 0, DumpSymbolTable)
    MakePrimitiveFunction("map", 2, Map)
    MakePrimitiveFunction("quote", 1, Quote)
    // list access
    MakePrimitiveFunction("car", 1, ExposedCar)
    MakePrimitiveFunction("cdr", 1, ExposedCdr)

    MakePrimitiveFunction("caar", 1, ExposedCaar)
    MakePrimitiveFunction("cadr", 1, ExposedCadr)
    MakePrimitiveFunction("cdar", 1, ExposedCdar)
    MakePrimitiveFunction("cddr", 1, ExposedCddr)

    MakePrimitiveFunction("caaar", 1, ExposedCaaar)
    MakePrimitiveFunction("caadr", 1, ExposedCaadr)
    MakePrimitiveFunction("cadar", 1, ExposedCadar)
    MakePrimitiveFunction("caddr", 1, ExposedCaddr)
    MakePrimitiveFunction("cdaar", 1, ExposedCdaar)
    MakePrimitiveFunction("cdadr", 1, ExposedCdadr)
    MakePrimitiveFunction("cddar", 1, ExposedCddar)
    MakePrimitiveFunction("cdddr", 1, ExposedCdddr)

    MakePrimitiveFunction("caaaar", 1, ExposedCaaaar)
    MakePrimitiveFunction("caaadr", 1, ExposedCaaadr)
    MakePrimitiveFunction("caadar", 1, ExposedCaadar)
    MakePrimitiveFunction("caaddr", 1, ExposedCaaddr)
    MakePrimitiveFunction("cadaar", 1, ExposedCadaar)
    MakePrimitiveFunction("cadadr", 1, ExposedCadadr)
    MakePrimitiveFunction("caddar", 1, ExposedCaddar)
    MakePrimitiveFunction("cadddr", 1, ExposedCadddr)
    MakePrimitiveFunction("cdaaar", 1, ExposedCdaaar)
    MakePrimitiveFunction("cdaadr", 1, ExposedCdaadr)
    MakePrimitiveFunction("cdadar", 1, ExposedCdadar)
    MakePrimitiveFunction("cdaddr", 1, ExposedCdaddr)
    MakePrimitiveFunction("cddaar", 1, ExposedCddaar)
    MakePrimitiveFunction("cddadr", 1, ExposedCddadr)
    MakePrimitiveFunction("cdddar", 1, ExposedCdddar)
    MakePrimitiveFunction("cddddr", 1, ExposedCddddr)

    MakePrimitiveFunction("first", 1, ExposedFirst)
    MakePrimitiveFunction("second", 1, ExposedSecond)
    MakePrimitiveFunction("third", 1, ExposedThird)
    MakePrimitiveFunction("fourth", 1, ExposedFourth)
    MakePrimitiveFunction("fifth", 1, ExposedFifth)

    MakePrimitiveFunction("nth", 2, ExposedNth)
}

func Add(args *Data) (result *Data, err error) {
    var acc int = 0
    var n *Data
    for c := args; NotNilP(c); c = Cdr(c) {
        n, err = Eval(Car(c))
        if err != nil {
            return
        } else if !NumberP(n) {
            err = errors.New("Number expected")
            return
        }
        acc += IntValue(n)
    }
    return NumberWithValue(acc), nil
}

func Subtract(args *Data) (result *Data, err error) {
    var n *Data
    n, err = Eval(Car(args))
    if err != nil {
        return
    }
    if !NumberP(n) {
        err = errors.New("Number expected")
        return
    }
    var acc int = IntValue(n)
    if Length(args) == 1 { //negation
        acc = -acc
    } else {
        for c := Cdr(args); NotNilP(c); c = Cdr(c) {
            n, err = Eval(Car(c))
            if err != nil {
                return
            }
            if !NumberP(n) {
                err = errors.New("Number expected")
                return
            }
            acc -= IntValue(n)
        }
    }
    return NumberWithValue(acc), nil
}

func Multiply(args *Data) (result *Data, err error) {
    var n *Data
    var acc int = 1
    for c := args; NotNilP(c); c = Cdr(c) {
        n, err = Eval(Car(c))
        if err != nil {
            return
        } else if !NumberP(n) {
            err = errors.New("Number expected")
            return
        }
        acc *= IntValue(n)
    }
    return NumberWithValue(acc), nil
}

func Quotient(args *Data) (result *Data, err error) {
    var n *Data
    n, err = Eval(Car(args))
    if err != nil {
        return
    }
    if !NumberP(n) {
        err = errors.New("Number expected")
        return
    }
    var acc int = IntValue(n)
    for c := Cdr(args); NotNilP(c); c = Cdr(c) {
        n, err = Eval(Car(c))
        if err != nil {
            return
        }
        if !NumberP(n) {
            err = errors.New("Number expected")
            return
        }
        acc /= IntValue(n)
    }
    return NumberWithValue(acc), nil
}

func Remainder(args *Data) (result *Data, err error) {
    if Length(args) != 2 {
        err = errors.New("2 args expected")
        return
    }

    var dividend *Data
    dividend, err = Eval(Car(args))
    if err != nil {
        return
    }
    if TypeOf(dividend) != NumberType {
        err = errors.New("Number expected")
        return
    }

    var divisor *Data
    divisor, err = Eval(Cadr(args))
    if err != nil {
        return
    }
    if TypeOf(divisor) != NumberType {
        err = errors.New("Number expected")
        return
    }

    val := IntValue(dividend) % IntValue(divisor)
    return NumberWithValue(val), nil
}

func LessThan(args *Data) (result *Data, err error) {
    if Length(args) != 2 {
        err = errors.New("2 args expected")
        return
    }

    var arg1 *Data
    arg1, err = Eval(Car(args))
    if err != nil {
        return
    }
    if TypeOf(arg1) != NumberType {
        err = errors.New("Number expected")
        return
    }

    var arg2 *Data
    arg2, err = Eval(Cadr(args))
    if err != nil {
        return
    }
    if TypeOf(arg2) != NumberType {
        err = errors.New("Number expected")
        return
    }

    val := IntValue(arg1) < IntValue(arg2)
    return BooleanWithValue(val), nil
}

func GreaterThan(args *Data) (result *Data, err error) {
    if Length(args) != 2 {
        err = errors.New("2 args expected")
        return
    }

    var arg1 *Data
    arg1, err = Eval(Car(args))
    if err != nil {
        return
    }
    if TypeOf(arg1) != NumberType {
        err = errors.New("Number expected")
        return
    }

    var arg2 *Data
    arg2, err = Eval(Cadr(args))
    if err != nil {
        return
    }
    if TypeOf(arg2) != NumberType {
        err = errors.New("Number expected")
        return
    }

    val := IntValue(arg1) > IntValue(arg2)
    return BooleanWithValue(val), nil
}

func EqualTo(args *Data) (result *Data, err error) {
    if Length(args) != 2 {
        err = errors.New("2 args expected")
        return
    }

    var arg1 *Data
    arg1, err = Eval(Car(args))
    if err != nil {
        return
    }

    var arg2 *Data
    arg2, err = Eval(Cadr(args))
    if err != nil {
        return
    }

    val := *arg1 == *arg2
    return BooleanWithValue(val), nil
}

func NotEqual(args *Data) (result *Data, err error) {
    if Length(args) != 2 {
        err = errors.New("2 args expected")
        return
    }

    var arg1 *Data
    arg1, err = Eval(Car(args))
    if err != nil {
        return
    }

    var arg2 *Data
    arg2, err = Eval(Cadr(args))
    if err != nil {
        return
    }

    val := *arg1 != *arg2
    return BooleanWithValue(val), nil
}

func LessThanOrEqualTo(args *Data) (result *Data, err error) {
    if Length(args) != 2 {
        err = errors.New("2 args expected")
        return
    }

    var arg1 *Data
    arg1, err = Eval(Car(args))
    if err != nil {
        return
    }
    if TypeOf(arg1) != NumberType {
        err = errors.New("Number expected")
        return
    }

    var arg2 *Data
    arg2, err = Eval(Cadr(args))
    if err != nil {
        return
    }
    if TypeOf(arg2) != NumberType {
        err = errors.New("Number expected")
        return
    }

    val := IntValue(arg1) <= IntValue(arg2)
    return BooleanWithValue(val), nil
}

func GreaterThanOrEqualTo(args *Data) (result *Data, err error) {
    if Length(args) != 2 {
        err = errors.New("2 args expected")
        return
    }

    var arg1 *Data
    arg1, err = Eval(Car(args))
    if err != nil {
        return
    }
    if TypeOf(arg1) != NumberType {
        err = errors.New("Number expected")
        return
    }

    var arg2 *Data
    arg2, err = Eval(Cadr(args))
    if err != nil {
        return
    }
    if TypeOf(arg2) != NumberType {
        err = errors.New("Number expected")
        return
    }

    val := IntValue(arg1) >= IntValue(arg2)
    return BooleanWithValue(val), nil
}

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

func If(args *Data) (result *Data, err error) {
    if Length(args) < 2 || Length(args) > 3 {
        err = errors.New(fmt.Sprintf("IF requires 2 or 3 arguments. Received %d.", Length(args)))
        return
    }

    c, err := Eval(Car(args))
    if err != nil {
        return
    }
    condition := BooleanValue(c)
    thenClause := Cadr(args)
    elseClause := Caddr(args)

    if condition {
        return Eval(thenClause)
    } else {
        return Eval(elseClause)
    }
}

func Lambda(args *Data) (result *Data, err error) {
    params := Car(args)
    body := Cdr(args)
    return FunctionWithNameParamsAndBody("", params, body), nil
}

func Define(args *Data) (result *Data, err error) {
    var value *Data
    thing := Car(args)
    if SymbolP(thing) {
        value = Cadr(args)
    } else if PairP(thing) {
        name := Car(thing)
        params := Cdr(thing)
        thing = name
        if !SymbolP(name) {
            err = errors.New("Function name has to be a symbol")
            return
        }
        body := Cdr(args)
        value = FunctionWithNameParamsAndBody(StringValue(name), params, body)
    } else {
        err = errors.New("Invalid definition")
        return
    }
    BindLocallyTo(thing, value)
    return value, nil
}

func DumpSymbolTable(args *Data) (result *Data, err error) {
    symbolTable.Dump()
    return
}

func Map(args *Data) (result *Data, err error) {
    f, err := Eval(Car(args))
    if err != nil {
        return
    }
    if !FunctionP(f) {
        err = errors.New("Map needs a function as its first argument")
        return
    }

    col, err := Eval(Cadr(args))
    if err != nil {
        return
    }
    if !ListP(col) {
        err = errors.New("Map needs a list as its second argument")
        return
    }

    var d []*Data = make([]*Data, 0, Length(col))
    var v *Data
    for c := col; NotNilP(c); c = Cdr(c) {
        v, err = Apply(f, Cons(Car(c), nil))
        if err != nil {
            return
        }
        d = append(d, v)
    }

    return ArrayToList(d), nil
}

func Quote(args *Data) (result *Data, err error) {
    return Car(args), nil
}

func ExposedCar(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Car(a), nil
}

func ExposedCdr(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Cdr(a), nil
}

func ExposedCaar(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Caar(a), nil
}

func ExposedCadr(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Cadr(a), nil
}

func ExposedCdar(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Cdar(a), nil
}

func ExposedCddr(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Cddr(a), nil
}

func ExposedCaaar(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Caaar(a), nil
}

func ExposedCaadr(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Caadr(a), nil
}

func ExposedCadar(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Cadar(a), nil
}

func ExposedCaddr(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Caddr(a), nil
}

func ExposedCdaar(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Cdaar(a), nil
}

func ExposedCdadr(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Cdadr(a), nil
}

func ExposedCddar(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Cddar(a), nil
}

func ExposedCdddr(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Cdddr(a), nil
}

func ExposedCaaaar(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Caaaar(a), nil
}

func ExposedCaaadr(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Caaadr(a), nil
}

func ExposedCaadar(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Caadar(a), nil
}

func ExposedCaaddr(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Caaddr(a), nil
}

func ExposedCadaar(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Cadaar(a), nil
}

func ExposedCadadr(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Cadadr(a), nil
}

func ExposedCaddar(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Caddar(a), nil
}

func ExposedCadddr(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Cadddr(a), nil
}

func ExposedCdaaar(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Cdaaar(a), nil
}

func ExposedCdaadr(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Cdaadr(a), nil
}

func ExposedCdadar(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Cdadar(a), nil
}

func ExposedCdaddr(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Cdaddr(a), nil
}

func ExposedCddaar(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Cddaar(a), nil
}

func ExposedCddadr(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Cddadr(a), nil
}

func ExposedCdddar(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Cdddar(a), nil
}

func ExposedCddddr(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Cddddr(a), nil
}

func ExposedFirst(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return First(a), nil
}

func ExposedSecond(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Second(a), nil
}

func ExposedThird(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Third(a), nil
}

func ExposedFourth(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Fourth(a), nil
}

func ExposedFifth(args *Data) (result *Data, err error) {
    a, err := Eval(Car(args))
    if err != nil {
        return
    }
    return Fifth(a), nil
}

func ExposedNth(args *Data) (result *Data, err error) {
    col, err := Eval(Car(args))
    if err != nil {
        return
    }
    if !ListP(col) {
        err = errors.New("First arg to nth must be a list")
        return
    }
    count, err := Eval(Cadr(args))
    if err != nil {
        return
    }
    if !NumberP(count) {
        err = errors.New("Second arg to nth must be a number")
        return
    }

    return Nth(col, IntValue(count)), nil
}
/// Function template
// func <function>(args *Data) (result *Data, err error) {
// }
