// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file pre-loads primitive builtin functions

// Flesh out as required. Remember to add tests to builtins_test.go

package golisp

import (
    "errors"
    "fmt"
    "math/rand"
    "os"
    "time"
)

func init() {
    Global = NewSymbolTableFrameBelow(nil)
    InitBuiltins()
}

func InitBuiltins() {
    // MakePrimitiveFunction(<symbol>, <required # args, -1 means >= 1>, <function>)
    Global.Intern("nil")

    MakePrimitiveFunction("quit", 0, DefQuit)

    // type tests

    MakePrimitiveFunction("list?", 1, IsPair)
    MakePrimitiveFunction("pair?", 1, IsPair)
    MakePrimitiveFunction("nil?", 1, ExposedNilP)
    MakePrimitiveFunction("notnil?", 1, ExposedNotNilP)
    MakePrimitiveFunction("symbol?", 1, IsSymbol)
    MakePrimitiveFunction("string?", 1, IsString)
    MakePrimitiveFunction("number?", 1, IsNumber)
    MakePrimitiveFunction("function?", 1, IsFunction)
    MakePrimitiveFunction("even?", 1, IsEven)
    MakePrimitiveFunction("odd?", 1, IsOdd)

    // math
    MakePrimitiveFunction("+", -1, Add)
    MakePrimitiveFunction("-", -1, Subtract)
    MakePrimitiveFunction("*", -1, Multiply)
    MakePrimitiveFunction("/", -1, Quotient)
    MakePrimitiveFunction("%", 2, Remainder)
    MakePrimitiveFunction("random-byte", 0, RandomByte)
    MakePrimitiveFunction("interval", 2, Interval)

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
    MakePrimitiveFunction("cond", -1, Cond)
    MakePrimitiveFunction("case", -1, Case)
    MakePrimitiveFunction("if", -1, If)
    MakePrimitiveFunction("lambda", -1, Lambda)
    MakePrimitiveFunction("define", -1, Define)
    MakePrimitiveFunction("map", 2, Map)
    MakePrimitiveFunction("quote", 1, Quote)
    MakePrimitiveFunction("let", -1, Let)
    MakePrimitiveFunction("begin", -1, Begin)
    MakePrimitiveFunction("do", -1, Do)

    // setters
    MakePrimitiveFunction("set!", 2, SetVar)
    MakePrimitiveFunction("set-car!", 2, SetCar)
    MakePrimitiveFunction("set-cdr!", 2, SetCdr)

    // list access
    MakePrimitiveFunction("list", -1, MakeList)
    MakePrimitiveFunction("length", 1, ListLength)
    MakePrimitiveFunction("cons", 2, ExposedCons)
    MakePrimitiveFunction("reverse", 1, ExposedReverse)

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

    // associatioon lists

    MakePrimitiveFunction("acons", -1, ExposedAcons)
    MakePrimitiveFunction("pairlis", -1, Pairlis)
    MakePrimitiveFunction("assoc", 2, ExposedAssoc)
    MakePrimitiveFunction("rassoc", 2, Rassoc)
    MakePrimitiveFunction("alist", 1, ExposedAlist)

    // system
    MakePrimitiveFunction("load", 1, LoadFile)
    MakePrimitiveFunction("dump", 0, DumpSymbolTable)
    MakePrimitiveFunction("sleep", 1, DefSleep)
    MakePrimitiveFunction("write-line", 1, WriteLine)

    // testing
    MakePrimitiveFunction("describe", -1, Describe)

}

func IsPair(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    return BooleanWithValue(PairP(Car(args))), nil
}

func ExposedNilP(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    return BooleanWithValue(NilP(Car(args))), nil
}

func ExposedNotNilP(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    return BooleanWithValue(NotNilP(Car(args))), nil
}

func IsSymbol(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    return BooleanWithValue(SymbolP(Car(args))), nil
}

func IsString(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    // Evaluate the Car(args) first, in case args is a symbol or ConsCell
    evaluated, _ := Eval(Car(args), env)
    // Now just check the evaluated
    return BooleanWithValue(StringP(evaluated)), nil
}

func IsNumber(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    // Evaluate the Car(args) first, in case args is a symbol or ConsCell
    evaluated, _ := Eval(Car(args), env)
    // Now just check the evaluated
    return BooleanWithValue(NumberP(evaluated)), nil
}

func IsEven(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    evaluated, _ := Eval(Car(args), env)
    if !NumberP(evaluated) {
        return False, nil
    }
    return BooleanWithValue((NumericValue(evaluated) % 2) == 0), nil
}

func IsOdd(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    evaluated, _ := Eval(Car(args), env)
    if !NumberP(evaluated) {
        return False, nil
    }
    return BooleanWithValue((NumericValue(evaluated) % 2) == 1), nil
}

func IsFunction(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    // Evaluate the Car(args) first, in case args is a symbol or ConsCell
    evaluated, _ := Eval(Car(args), env)
    // Now just check the evaluated
    return BooleanWithValue(FunctionP(evaluated)), nil
}

func Add(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var acc uint32 = 0
    var n *Data
    for c := args; NotNilP(c); c = Cdr(c) {
        n, err = Eval(Car(c), env)
        if err != nil {
            return
        } else if !NumberP(n) {
            err = errors.New(fmt.Sprintf("Number expected, received %s", String(n)))
            return
        }
        acc += NumericValue(n)
    }
    return NumberWithValue(acc), nil
}

func Subtract(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var n *Data
    n, err = Eval(Car(args), env)
    if err != nil {
        return
    }
    if !NumberP(n) {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(n)))
        return
    }
    var acc uint32 = NumericValue(n)
    for c := Cdr(args); NotNilP(c); c = Cdr(c) {
        n, err = Eval(Car(c), env)
        if err != nil {
            return
        }
        if !NumberP(n) {
            err = errors.New(fmt.Sprintf("Number expected, received %s", String(n)))
            return
        }
        if NumericValue(n) > acc {
            return NumberWithValue(0), nil
        } else {
            acc -= NumericValue(n)
        }

    }
    return NumberWithValue(acc), nil
}

func Multiply(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var n *Data
    var acc uint32 = 1
    for c := args; NotNilP(c); c = Cdr(c) {
        n, err = Eval(Car(c), env)
        if err != nil {
            return
        } else if !NumberP(n) {
            err = errors.New(fmt.Sprintf("Number expected, received %s", String(n)))
            return
        }
        acc *= NumericValue(n)
    }
    return NumberWithValue(acc), nil
}

func Quotient(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var n *Data
    n, err = Eval(Car(args), env)
    if err != nil {
        return
    }
    if !NumberP(n) {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(n)))
        return
    }
    var acc uint32 = NumericValue(n)
    for c := Cdr(args); NotNilP(c); c = Cdr(c) {
        n, err = Eval(Car(c), env)
        if err != nil {
            return
        }
        if !NumberP(n) {
            err = errors.New(fmt.Sprintf("Number expected, received %s", String(n)))
            return
        }
        acc /= NumericValue(n)
    }
    return NumberWithValue(acc), nil
}

func Remainder(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    if Length(args) != 2 {
        err = errors.New(fmt.Sprintf("2 args expected, %d received", Length(args)))
        return
    }

    var dividend *Data
    dividend, err = Eval(Car(args), env)
    if err != nil {
        return
    }
    if TypeOf(dividend) != NumberType {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(dividend)))
        return
    }

    var divisor *Data
    divisor, err = Eval(Cadr(args), env)
    if err != nil {
        return
    }
    if TypeOf(divisor) != NumberType {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(divisor)))
        return
    }

    val := NumericValue(dividend) % NumericValue(divisor)
    return NumberWithValue(val), nil
}

func RandomByte(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    r := uint8(rand.Int())
    result = NumberWithValue(uint32(r))
    return
}

func Interval(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    startObj, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    start := NumericValue(startObj)

    endObj, err := Eval(Cadr(args), env)
    if err != nil {
        return
    }
    end := NumericValue(endObj)

    var items []*Data = make([]*Data, 0, end-start+1)

    for i := start; i <= end; i = i + 1 {
        items = append(items, NumberWithValue(i))
    }
    result = ArrayToList(items)
    return
}

func LessThan(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    if Length(args) != 2 {
        err = errors.New(fmt.Sprintf("2 args expected, %d received", Length(args)))
        return
    }

    var arg1 *Data
    arg1, err = Eval(Car(args), env)
    if err != nil {
        return
    }
    if TypeOf(arg1) != NumberType {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(arg1)))
        return
    }

    var arg2 *Data
    arg2, err = Eval(Cadr(args), env)
    if err != nil {
        return
    }
    if TypeOf(arg2) != NumberType {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(arg2)))
        return
    }

    val := NumericValue(arg1) < NumericValue(arg2)
    return BooleanWithValue(val), nil
}

func GreaterThan(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    if Length(args) != 2 {
        err = errors.New(fmt.Sprintf("2 args expected, %d received", Length(args)))
        return
    }

    var arg1 *Data
    arg1, err = Eval(Car(args), env)
    if err != nil {
        return
    }
    if TypeOf(arg1) != NumberType {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(arg1)))
        return
    }

    var arg2 *Data
    arg2, err = Eval(Cadr(args), env)
    if err != nil {
        return
    }
    if TypeOf(arg2) != NumberType {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(arg2)))
        return
    }

    val := NumericValue(arg1) > NumericValue(arg2)
    return BooleanWithValue(val), nil
}

func EqualTo(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    if Length(args) != 2 {
        err = errors.New(fmt.Sprintf("2 args expected, %d received", Length(args)))
        return
    }

    var arg1 *Data
    arg1, err = Eval(Car(args), env)
    if err != nil {
        return
    }

    var arg2 *Data
    arg2, err = Eval(Cadr(args), env)
    if err != nil {
        return
    }

    return BooleanWithValue(IsEqual(arg1, arg2)), nil
}

func NotEqual(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    if Length(args) != 2 {
        err = errors.New(fmt.Sprintf("2 args expected, %d received", Length(args)))
        return
    }

    var arg1 *Data
    arg1, err = Eval(Car(args), env)
    if err != nil {
        return
    }

    var arg2 *Data
    arg2, err = Eval(Cadr(args), env)
    if err != nil {
        return
    }

    return BooleanWithValue(!IsEqual(arg1, arg2)), nil
}

func LessThanOrEqualTo(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    if Length(args) != 2 {
        err = errors.New(fmt.Sprintf("2 args expected, %d received", Length(args)))
        return
    }

    var arg1 *Data
    arg1, err = Eval(Car(args), env)
    if err != nil {
        return
    }
    if TypeOf(arg1) != NumberType {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(arg1)))
        return
    }

    var arg2 *Data
    arg2, err = Eval(Cadr(args), env)
    if err != nil {
        return
    }
    if TypeOf(arg2) != NumberType {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(arg2)))
        return
    }

    val := NumericValue(arg1) <= NumericValue(arg2)
    return BooleanWithValue(val), nil
}

func GreaterThanOrEqualTo(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    if Length(args) != 2 {
        err = errors.New(fmt.Sprintf("2 args expected, %d received", Length(args)))
        return
    }

    var arg1 *Data
    arg1, err = Eval(Car(args), env)
    if err != nil {
        return
    }
    if TypeOf(arg1) != NumberType {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(arg1)))
        return
    }

    var arg2 *Data
    arg2, err = Eval(Cadr(args), env)
    if err != nil {
        return
    }
    if TypeOf(arg2) != NumberType {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(arg2)))
        return
    }

    val := NumericValue(arg1) >= NumericValue(arg2)
    return BooleanWithValue(val), nil
}

func BooleanNot(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    if Length(args) != 1 {
        err = errors.New(fmt.Sprintf("! requires 1 argument. Received %d.", Length(args)))
        return
    }

    arg, err := Eval(Car(args), env)
    if err != nil {
        return
    }

    val := BooleanValue(arg)
    return BooleanWithValue(!val), nil
}

func BooleanAnd(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    for c := args; NotNilP(c); c = Cdr(c) {
        result, err = Eval(Car(c), env)
        if !BooleanValue(result) {
            return
        }
    }
    result = True
    return
}

func BooleanOr(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    for c := args; NotNilP(c); c = Cdr(c) {
        result, err = Eval(Car(c), env)
        if BooleanValue(result) {
            return
        }
    }
    result = False
    return
}

func Cond(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var condition *Data
    for c := args; NotNilP(c); c = Cdr(c) {
        clause := Car(c)
        if !PairP(clause) {
            err = errors.New("Cond expect a sequence of clauses that are lists")
            return
        }
        condition, err = Eval(Car(clause), env)
        if err != nil {
            return
        }
        if BooleanValue(condition) || StringValue(Car(clause)) == "else" {
            for e := Cdr(clause); NotNilP(e); e = Cdr(e) {
                result, err = Eval(Car(e), env)
                if err != nil {
                    return
                }
            }
            return
        }
    }
    return
}

func EvalList(l *Data, env *SymbolTableFrame) (result *Data, err error) {
    for sexpr := l; NotNilP(sexpr); sexpr = Cdr(sexpr) {
        result, err = Eval(Car(sexpr), env)
        if err != nil {
            return
        }
    }
    return
}

func Case(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var keyValue *Data
    var targetValue *Data

    keyValue, err = Eval(Car(args), env)
    if err != nil {
        return
    }

    for clauseCell := Cdr(args); NotNilP(clauseCell); clauseCell = Cdr(clauseCell) {
        clause := Car(clauseCell)
        if PairP(clause) {
            if IsEqual(Car(clause), SymbolWithName("else")) {
                return EvalList(Cdr(clause), env)
            } else {
                targetValue, err = Eval(Car(clause), env)
                if IsEqual(targetValue, keyValue) {
                    return EvalList(Cdr(clause), env)
                }
            }
        } else {
            err = errors.New("Case requires non-atomic clauses")
            return
        }
    }

    return
}

func If(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    if Length(args) < 2 || Length(args) > 3 {
        err = errors.New(fmt.Sprintf("IF requires 2 or 3 arguments. Received %d.", Length(args)))
        return
    }

    c, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    condition := BooleanValue(c)
    thenClause := Cadr(args)
    elseClause := Caddr(args)

    if condition {
        return Eval(thenClause, env)
    } else {
        return Eval(elseClause, env)
    }
}

func ListLength(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    d, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return NumberWithValue(uint32(Length(d))), nil
}

func ExposedCons(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var car *Data
    car, err = Eval(Car(args), env)
    if err != nil {
        return
    }

    var cdr *Data
    cdr, err = Eval(Cadr(args), env)
    if err != nil {
        return
    }

    result = Cons(car, cdr)
    return
}

func ExposedReverse(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var val *Data
    val, err = Eval(Car(args), env)
    if err != nil {
        return
    }
    result = Reverse(val)
    return
}

func Lambda(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    params := Car(args)
    body := Cdr(args)
    return FunctionWithNameParamsBodyAndParent("anonymous", params, body, env), nil
}

func Define(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var value *Data
    thing := Car(args)
    if SymbolP(thing) {
        value, err = Eval(Cadr(args), env)
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
        value = FunctionWithNameParamsBodyAndParent(StringValue(name), params, body, env)
    } else {
        err = errors.New("Invalid definition")
        return
    }
    env.BindLocallyTo(thing, value)
    return value, nil
}

func DumpSymbolTable(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    env.Dump()
    return
}

func Map(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    f, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    if !FunctionP(f) {
        err = errors.New("Map needs a function as its first argument")
        return
    }

    col, err := Eval(Cadr(args), env)
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
        v, err = Apply(f, Cons(Car(c), nil), env)
        if err != nil {
            return
        }
        d = append(d, v)
    }

    return ArrayToList(d), nil
}

func Quote(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    return Car(args), nil
}

func MakeList(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var items []*Data = make([]*Data, 0, Length(args))
    var item *Data
    for cell := args; NotNilP(cell); cell = Cdr(cell) {
        item, err = Eval(Car(cell), env)
        if err != nil {
            return
        }
        items = append(items, item)
    }
    result = ArrayToList(items)
    return
}

func ExposedCar(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Car(a), nil
}

func ExposedCdr(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Cdr(a), nil
}

func ExposedCaar(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Caar(a), nil
}

func ExposedCadr(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Cadr(a), nil
}

func ExposedCdar(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Cdar(a), nil
}

func ExposedCddr(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Cddr(a), nil
}

func ExposedCaaar(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Caaar(a), nil
}

func ExposedCaadr(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Caadr(a), nil
}

func ExposedCadar(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Cadar(a), nil
}

func ExposedCaddr(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Caddr(a), nil
}

func ExposedCdaar(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Cdaar(a), nil
}

func ExposedCdadr(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Cdadr(a), nil
}

func ExposedCddar(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Cddar(a), nil
}

func ExposedCdddr(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Cdddr(a), nil
}

func ExposedCaaaar(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Caaaar(a), nil
}

func ExposedCaaadr(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Caaadr(a), nil
}

func ExposedCaadar(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Caadar(a), nil
}

func ExposedCaaddr(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Caaddr(a), nil
}

func ExposedCadaar(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Cadaar(a), nil
}

func ExposedCadadr(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Cadadr(a), nil
}

func ExposedCaddar(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Caddar(a), nil
}

func ExposedCadddr(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Cadddr(a), nil
}

func ExposedCdaaar(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Cdaaar(a), nil
}

func ExposedCdaadr(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Cdaadr(a), nil
}

func ExposedCdadar(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Cdadar(a), nil
}

func ExposedCdaddr(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Cdaddr(a), nil
}

func ExposedCddaar(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Cddaar(a), nil
}

func ExposedCddadr(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Cddadr(a), nil
}

func ExposedCdddar(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Cdddar(a), nil
}

func ExposedCddddr(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Cddddr(a), nil
}

func ExposedFirst(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return First(a), nil
}

func ExposedSecond(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Second(a), nil
}

func ExposedThird(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Third(a), nil
}

func ExposedFourth(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Fourth(a), nil
}

func ExposedFifth(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Fifth(a), nil
}

func ExposedNth(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    col, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    if !PairP(col) {
        err = errors.New("First arg to nth must be a list")
        return
    }
    count, err := Eval(Cadr(args), env)
    if err != nil {
        return
    }
    if !NumberP(count) {
        err = errors.New("Second arg to nth must be a number")
        return
    }

    return Nth(col, int(NumericValue(count))), nil
}

func ExposedAlist(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    l, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    result = Alist(l)
    return
}

func ExposedAcons(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var key *Data
    var value *Data
    var alist *Data

    if Length(args) < 2 || Length(args) > 3 {
        err = errors.New("acons must have 2 or 3 arguments")
        return
    }

    key, err = Eval(Car(args), env)
    if err != nil {
        return
    }

    if PairP(key) {
        err = errors.New("Alist key can not be a list")
        return
    }

    value, err = Eval(Cadr(args), env)
    if err != nil {
        return
    }

    if Length(args) == 3 {
        alist, err = Eval(Caddr(args), env)
        if err != nil {
            return
        }
    }

    result = Acons(key, value, alist)
    return
}

func Pairlis(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var keys *Data
    var values *Data
    if Length(args) > 3 {
        err = errors.New("pairlis takes at most three arguments")
        return
    }

    keys, err = Eval(Car(args), env)
    if err != nil {
        return
    }
    if !PairP(keys) {
        err = errors.New("First arg of pairlis must be a list")
        return
    }

    values, err = Eval(Cadr(args), env)
    if err != nil {
        return
    }

    if !PairP(values) {
        err = errors.New("Second arg of Pairlis must be a list")
        return
    }

    if Length(keys) != Length(values) {
        err = errors.New("Pairlis requires the same number of keys and values")
        return
    }

    result, err = Eval(Caddr(args), env)
    if err != nil {
        return
    }

    if NotNilP(result) {
        if !PairP(result) {
            err = errors.New("Third arg of pairlis must be an association list (if provided)")
            return
        }
    }

    for keyCell, valueCell := keys, values; NotNilP(keyCell); keyCell, valueCell = Cdr(keyCell), Cdr(valueCell) {
        key := Car(keyCell)
        if NilP(keyCell) {
            err = errors.New("Assoc list keys can not be nil")
        }
        value := Car(valueCell)
        result = Acons(key, value, result)
    }

    return
}

func ExposedAssoc(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var key *Data
    var list *Data

    key, err = Eval(Car(args), env)
    if err != nil {
        return
    }

    list, err = Eval(Cadr(args), env)
    if err != nil {
        return
    }

    result, err = Assoc(key, list)
    return
}

func Rassoc(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var value *Data
    var list *Data

    value, err = Eval(Car(args), env)
    if err != nil {
        return
    }

    list, err = Eval(Cadr(args), env)
    if err != nil {
        return
    }

    for c := list; NotNilP(c); c = Cdr(c) {
        pair := Car(c)
        if !PairP(pair) && !DottedPairP(pair) {
            err = errors.New("Assoc list must consist of dotted pairs")
            return
        }
        if IsEqual(Cdr(pair), value) {
            result = pair
            return
        }
    }
    return
}

func LoadFile(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    filename := Car(args)
    if !StringP(filename) {
        err = errors.New("Filename must be a string")
        return
    }

    return ProcessFile(StringValue(filename))
}

func SetVar(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    symbol := Car(args)
    if !SymbolP(symbol) {
        err = errors.New("set! requires a raw (unevaluated) symbol as it's first argument.")
    }
    value, err := Eval(Cadr(args), env)
    if err != nil {
        return
    }
    return env.SetTo(symbol, value)
}

func SetCar(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    pair, err := Eval(Car(args), env)
    symbol := Car(args)
    if !PairP(symbol) {
        err = errors.New("set-car! requires a pair as it's first argument.")
    }
    value, err := Eval(Cadr(args), env)
    if err != nil {
        return
    }
    pair.Car = value
    return value, nil
}

func SetCdr(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    pair, err := Eval(Car(args), env)
    symbol := Car(args)
    if !PairP(symbol) {
        err = errors.New("set-cdr! requires a pair as it's first argument.")
    }
    value, err := Eval(Cadr(args), env)
    if err != nil {
        return
    }
    pair.Cdr = value
    return value, nil
}

func BindLetLocals(bindingForms *Data, env *SymbolTableFrame) (err error) {
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
        value, err = Eval(Cadr(bindingPair), env)
        if err != nil {
            return
        }
        env.BindLocallyTo(name, value)
    }
    return
}

func Let(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    if Length(args) < 1 {
        err = errors.New("Let requires at least a list of bindings")
        return
    }

    if !PairP(Car(args)) {
        err = errors.New("Let requires a list of bindings as it's first argument")
        return
    }

    localFrame := NewSymbolTableFrameBelow(env)
    BindLetLocals(Car(args), localFrame)

    for cell := Cdr(args); NotNilP(cell); cell = Cdr(cell) {
        sexpr := Car(cell)
        result, err = Eval(sexpr, localFrame)
        if err != nil {
            return
        }
    }

    return
}

func Begin(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    for cell := args; NotNilP(cell); cell = Cdr(cell) {
        sexpr := Car(cell)
        result, err = Eval(sexpr, env)
        if err != nil {
            return
        }
    }
    return
}

func RebindDoLocals(bindingForms *Data, env *SymbolTableFrame) (err error) {
    var name *Data
    var value *Data

    for cell := bindingForms; NotNilP(cell); cell = Cdr(cell) {
        bindingTuple := Car(cell)
        name = Car(bindingTuple)
        value, err = Eval(Caddr(bindingTuple), env)
        if err != nil {
            return
        }
        env.BindLocallyTo(name, value)
    }
    return
}

func Do(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    if Length(args) < 2 {
        err = errors.New("Do requires at least a list of bindings and a test clause")
        return
    }

    bindings := Car(args)
    if !PairP(bindings) {
        err = errors.New("Do requires a list of bindings as it's first argument")
        return
    }

    testClause := Cadr(args)
    if !PairP(testClause) {
        err = errors.New("Do requires a list as it's second argument")
        return
    }

    localFrame := NewSymbolTableFrameBelow(env)
    BindLetLocals(bindings, localFrame)

    body := Cddr(args)

    var shouldExit *Data

    for true {
        shouldExit, err = Eval(Car(testClause), localFrame)
        if err != nil {
            return
        }

        if BooleanValue(shouldExit) {
            for cell := Cdr(testClause); NotNilP(cell); cell = Cdr(cell) {
                sexpr := Car(cell)
                result, err = Eval(sexpr, localFrame)
                if err != nil {
                    return
                }
            }
            return
        }

        for cell := body; NotNilP(cell); cell = Cdr(cell) {
            sexpr := Car(cell)
            result, err = Eval(sexpr, localFrame)
            if err != nil {
                return
            }
        }

        RebindDoLocals(bindings, localFrame)
    }
    return
}

func DefQuit(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    WriteHistoryToFile(".golisp_history")
    os.Exit(0)
    return
}

func DefSleep(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    n := Car(args)
    if !NumberP(n) {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(n)))
        return
    }
    millis := NumericValue(n)
    time.Sleep(time.Duration(millis) * time.Millisecond)
    return
}

func WriteLine(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    data, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    println(StringValue(data))
    return
}

/// Function template
// func <function>(args *Data, env *SymbolTableFrame) (result *Data, err error) {
// }
