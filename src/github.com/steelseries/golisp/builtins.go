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
    InitDeviceBuiltins()
}

func InitBuiltins() {
    // MakePrimitiveFunction(<symbol>, <required # args, -1 means >= 1>, <function>)
    Intern("nil")

    // type tests

    MakePrimitiveFunction("list?", 1, IsPair)
    MakePrimitiveFunction("pair?", 1, IsPair)
    MakePrimitiveFunction("nil?", 1, ExposedNilP)
    MakePrimitiveFunction("notnil?", 1, ExposedNotNilP)
    MakePrimitiveFunction("symbol?", 1, IsSymbol)
    MakePrimitiveFunction("string?", 1, IsString)
    MakePrimitiveFunction("number?", 1, IsNumber)
    MakePrimitiveFunction("function?", 1, IsFunction)

    // math
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
    MakePrimitiveFunction("and", -1, BooleanAnd)
    MakePrimitiveFunction("or", -1, BooleanOr)

    // special forms
    MakePrimitiveFunction("if", -1, If)
    MakePrimitiveFunction("lambda", -1, Lambda)
    MakePrimitiveFunction("define", -1, Define)
    MakePrimitiveFunction("map", 2, Map)
    MakePrimitiveFunction("quote", 1, Quote)
    MakePrimitiveFunction("set!", 2, SetVar)
    MakePrimitiveFunction("let", -1, Let)
    MakePrimitiveFunction("begin", -1, Begin)

    // list access
    MakePrimitiveFunction("list", -1, MakeList)
    MakePrimitiveFunction("length", 1, ListLength)

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

    // system
    MakePrimitiveFunction("load", 1, LoadFile)
    MakePrimitiveFunction("dump", 0, DumpSymbolTable)

    // testing
    MakePrimitiveFunction("describe", -1, Describe)

}

func IsPair(args *Data) (result *Data, err error) {
    return BooleanWithValue(PairP(Car(args))), nil
}

func ExposedNilP(args *Data) (result *Data, err error) {
    return BooleanWithValue(NilP(Car(args))), nil
}

func ExposedNotNilP(args *Data) (result *Data, err error) {
    return BooleanWithValue(NotNilP(Car(args))), nil
}

func IsSymbol(args *Data) (result *Data, err error) {
    return BooleanWithValue(SymbolP(Car(args))), nil
}

func IsString(args *Data) (result *Data, err error) {
    return BooleanWithValue(StringP(Car(args))), nil
}

func IsNumber(args *Data) (result *Data, err error) {
    return BooleanWithValue(NumberP(Car(args))), nil
}

func IsFunction(args *Data) (result *Data, err error) {
    return BooleanWithValue(FunctionP(Car(args))), nil
}

func Add(args *Data) (result *Data, err error) {
    var acc int = 0
    var n *Data
    for c := args; NotNilP(c); c = Cdr(c) {
        n, err = Eval(Car(c))
        if err != nil {
            return
        } else if !NumberP(n) {
            err = errors.New(fmt.Sprintf("Number expected, received %s", String(n)))
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
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(n)))
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
                err = errors.New(fmt.Sprintf("Number expected, received %s", String(n)))
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
            err = errors.New(fmt.Sprintf("Number expected, received %s", String(n)))
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
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(n)))
        return
    }
    var acc int = IntValue(n)
    for c := Cdr(args); NotNilP(c); c = Cdr(c) {
        n, err = Eval(Car(c))
        if err != nil {
            return
        }
        if !NumberP(n) {
            err = errors.New(fmt.Sprintf("Number expected, received %s", String(n)))
            return
        }
        acc /= IntValue(n)
    }
    return NumberWithValue(acc), nil
}

func Remainder(args *Data) (result *Data, err error) {
    if Length(args) != 2 {
        err = errors.New(fmt.Sprintf("2 args expected, %d received", Length(args)))
        return
    }

    var dividend *Data
    dividend, err = Eval(Car(args))
    if err != nil {
        return
    }
    if TypeOf(dividend) != NumberType {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(dividend)))
        return
    }

    var divisor *Data
    divisor, err = Eval(Cadr(args))
    if err != nil {
        return
    }
    if TypeOf(divisor) != NumberType {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(divisor)))
        return
    }

    val := IntValue(dividend) % IntValue(divisor)
    return NumberWithValue(val), nil
}

func LessThan(args *Data) (result *Data, err error) {
    if Length(args) != 2 {
        err = errors.New(fmt.Sprintf("2 args expected, %d received", Length(args)))
        return
    }

    var arg1 *Data
    arg1, err = Eval(Car(args))
    if err != nil {
        return
    }
    if TypeOf(arg1) != NumberType {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(arg1)))
        return
    }

    var arg2 *Data
    arg2, err = Eval(Cadr(args))
    if err != nil {
        return
    }
    if TypeOf(arg2) != NumberType {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(arg2)))
        return
    }

    val := IntValue(arg1) < IntValue(arg2)
    return BooleanWithValue(val), nil
}

func GreaterThan(args *Data) (result *Data, err error) {
    if Length(args) != 2 {
        err = errors.New(fmt.Sprintf("2 args expected, %d received", Length(args)))
        return
    }

    var arg1 *Data
    arg1, err = Eval(Car(args))
    if err != nil {
        return
    }
    if TypeOf(arg1) != NumberType {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(arg1)))
        return
    }

    var arg2 *Data
    arg2, err = Eval(Cadr(args))
    if err != nil {
        return
    }
    if TypeOf(arg2) != NumberType {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(arg2)))
        return
    }

    val := IntValue(arg1) > IntValue(arg2)
    return BooleanWithValue(val), nil
}

func EqualTo(args *Data) (result *Data, err error) {
    if Length(args) != 2 {
        err = errors.New(fmt.Sprintf("2 args expected, %d received", Length(args)))
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

    return BooleanWithValue(IsEqual(arg1, arg2)), nil
}

func NotEqual(args *Data) (result *Data, err error) {
    if Length(args) != 2 {
        err = errors.New(fmt.Sprintf("2 args expected, %d received", Length(args)))
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

    return BooleanWithValue(!IsEqual(arg1, arg2)), nil
}

func LessThanOrEqualTo(args *Data) (result *Data, err error) {
    if Length(args) != 2 {
        err = errors.New(fmt.Sprintf("2 args expected, %d received", Length(args)))
        return
    }

    var arg1 *Data
    arg1, err = Eval(Car(args))
    if err != nil {
        return
    }
    if TypeOf(arg1) != NumberType {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(arg1)))
        return
    }

    var arg2 *Data
    arg2, err = Eval(Cadr(args))
    if err != nil {
        return
    }
    if TypeOf(arg2) != NumberType {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(arg2)))
        return
    }

    val := IntValue(arg1) <= IntValue(arg2)
    return BooleanWithValue(val), nil
}

func GreaterThanOrEqualTo(args *Data) (result *Data, err error) {
    if Length(args) != 2 {
        err = errors.New(fmt.Sprintf("2 args expected, %d received", Length(args)))
        return
    }

    var arg1 *Data
    arg1, err = Eval(Car(args))
    if err != nil {
        return
    }
    if TypeOf(arg1) != NumberType {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(arg1)))
        return
    }

    var arg2 *Data
    arg2, err = Eval(Cadr(args))
    if err != nil {
        return
    }
    if TypeOf(arg2) != NumberType {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(arg2)))
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

func BooleanAnd(args *Data) (result *Data, err error) {
    for c := args; NotNilP(c); c = Cdr(c) {
        result, err = Eval(Car(c))
        if !BooleanValue(result) {
            return
        }
    }
    result = True
    return
}

func BooleanOr(args *Data) (result *Data, err error) {
    for c := args; NotNilP(c); c = Cdr(c) {
        result, err = Eval(Car(c))
        if BooleanValue(result) {
            return
        }
    }
    result = False
    return
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

func ListLength(args *Data) (result *Data, err error) {
    d, err := Eval(Car(args))
    if err != nil {
        return
    }
    return NumberWithValue(Length(d)), nil
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
        value, err = Eval(Cadr(args))
        if err != nil {
            return
        }
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
    if !PairP(col) {
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

func MakeList(args *Data) (result *Data, err error) {
    var items []*Data = make([]*Data, 0, Length(args))
    var item *Data
    for cell := args; NotNilP(cell); cell = Cdr(cell) {
        item, err = Eval(Car(cell))
        if err != nil {
            return
        }
        items = append(items, item)
    }
    result = ArrayToList(items)
    return
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
    if !PairP(col) {
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

func LoadFile(args *Data) (result *Data, err error) {
    filename := Car(args)
    if !StringP(filename) {
        err = errors.New("Filename must be a string")
        return
    }

    return ProcessFile(StringValue(filename))
}

func Describe(args *Data) (d *Data, e error) {
    if !(StringP(Car(args)) || SymbolP(Car(args))) {
        e = errors.New("The describe tag must be a string or symbol")
        return
    }
    fmt.Printf("%s\n", StringValue(Car(args)))

    for clauses := Cdr(args); NotNilP(clauses); clauses = Cdr(clauses) {
        clause := Car(clauses)
        fmt.Printf("   %s - ", String(clause))
        result, err := Eval(clause)
        if err != nil {
            fmt.Printf("error: %s\n", err)
            return
        }
        if BooleanValue(result) {
            fmt.Printf("ok\n")
        } else {
            value, _ := Eval(Cadr(clause))
            fmt.Printf("failed: %s is %s\n", String(Cadr(clause)), String(value))
            return
        }
    }
    return
}

func SetVar(args *Data) (result *Data, err error) {
    symbol, err := Eval(Car(args))
    if err != nil {
        return
    }
    value, err := Eval(Cadr(args))
    if err != nil {
        return
    }
    return SetTo(symbol, value)
}

func BindLetLocals(bindingForms *Data) (err error) {
    var name *Data
    var value *Data

    for cell := bindingForms; NotNilP(cell); cell = Cdr(cell) {
        bindingPair := Car(cell)
        if !PairP(bindingPair) {
            err = errors.New("Let requires a list of bindings (with are pairs) as it's first argument")
            return
        }
        name = Car(bindingPair)
        if !SymbolP(name) {
            err = errors.New("First part of a let binding pair must be a symbol")
        }
        value, err = Eval(Cadr(bindingPair))
        if err != nil {
            return
        }
        BindLocallyTo(name, value)
    }
    return
}

func Let(args *Data) (result *Data, err error) {
    if Length(args) < 1 {
        err = errors.New("Let requires at least a list of bindings")
        return
    }

    if !PairP(Car(args)) {
        err = errors.New("Let requires a list of bindings as it's first argument")
        return
    }

    PushLocalBindings()
    BindLetLocals(Car(args))

    for cell := Cdr(args); NotNilP(cell); cell = Cdr(cell) {
        sexpr := Car(cell)
        result, err = Eval(sexpr)
        if err != nil {
            return
        }
    }
    PopLocalBindings()

    return
}

func Begin(args *Data) (result *Data, err error) {
    for cell := args; NotNilP(cell); cell = Cdr(cell) {
        sexpr := Car(cell)
        result, err = Eval(sexpr)
        if err != nil {
            return
        }
    }
    return
}

/// Function template
// func <function>(args *Data) (result *Data, err error) {
// }
