// Copyright 2013 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
// This file pre-loads primitive builtin functions
package golisp

import (
    "errors"
    "fmt"
    "math/rand"
    "os"
    "strings"
    "time"
    "unsafe"
)

var DebugTrace = false

func init() {
    Global = NewSymbolTableFrameBelow(nil)
    InitBuiltins()
}

func InitBuiltins() {
    // MakePrimitiveFunction(<symbol>, <required # args, -1 means >= 1>, <function>)
    Global.Intern("nil")

    MakePrimitiveFunction("quit", 0, DefQuit)
    MakePrimitiveFunction("debug", -1, DefDebug)

    // type tests

    MakePrimitiveFunction("list?", 1, IsPair)
    MakePrimitiveFunction("pair?", 1, IsPair)
    MakePrimitiveFunction("alist?", 1, IsAlist)
    MakePrimitiveFunction("nil?", 1, ExposedNilP)
    MakePrimitiveFunction("notnil?", 1, ExposedNotNilP)
    MakePrimitiveFunction("symbol?", 1, IsSymbol)
    MakePrimitiveFunction("string?", 1, IsString)
    MakePrimitiveFunction("number?", 1, IsNumber)
    MakePrimitiveFunction("float?", 1, IsFloat)
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
    MakePrimitiveFunction("integer", 1, ToInt)
    MakePrimitiveFunction("float", 1, ToFloat)

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
    MakePrimitiveFunction("apply", 2, DefApply)
    MakePrimitiveFunction("->", -1, DefChain)
    MakePrimitiveFunction("=>", -1, DefTap)

    // setters
    MakePrimitiveFunction("set!", 2, SetVar)
    MakePrimitiveFunction("set-car!", 2, SetCar)
    MakePrimitiveFunction("set-cdr!", 2, SetCdr)
    MakePrimitiveFunction("set-nth!", 3, SetNth)

    // list access
    MakePrimitiveFunction("list", -1, MakeList)
    MakePrimitiveFunction("length", 1, ListLength)
    MakePrimitiveFunction("cons", 2, ExposedCons)
    MakePrimitiveFunction("reverse", 1, ExposedReverse)
    MakePrimitiveFunction("flatten", 1, ExposedFlatten)
    MakePrimitiveFunction("flatten*", 1, ExposedRecursiveFlatten)
    MakePrimitiveFunction("append", 2, ExposeAppend)
    MakePrimitiveFunction("append!", 2, ExposeAppendBang)
    MakePrimitiveFunction("copy", 1, ExposeCopy)
    MakePrimitiveFunction("partition", 2, Partition)

    MakePrimitiveFunction("car", 1, ExposedCar)
    MakePrimitiveFunction("head", 1, ExposedCar)
    MakePrimitiveFunction("cdr", 1, ExposedCdr)
    MakePrimitiveFunction("rest", 1, ExposedCdr)
    MakePrimitiveFunction("tail", 1, ExposedCdr)

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
    MakePrimitiveFunction("sixth", 1, ExposedSixth)
    MakePrimitiveFunction("seventh", 1, ExposedSeventh)
    MakePrimitiveFunction("eighth", 1, ExposedEighth)
    MakePrimitiveFunction("ninth", 1, ExposedNinth)
    MakePrimitiveFunction("tenth", 1, ExposedTenth)

    MakePrimitiveFunction("nth", 2, ExposedNth)

    // association lists

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
    MakePrimitiveFunction("str", -1, MakeString)
    MakePrimitiveFunction("time", 1, DefTime)

    // bytearrays
    MakePrimitiveFunction("list-to-bytearray", 1, ListToBytes)
    MakePrimitiveFunction("bytearray-to-list", 1, BytesToList)
    MakePrimitiveFunction("replace-byte", 3, ReplaceByte)
    MakePrimitiveFunction("replace-byte!", 3, ReplaceByteBang)
    MakePrimitiveFunction("extract-byte", 2, ExtractByte)
    MakePrimitiveFunction("append-bytes", -1, AppendBytes)
    MakePrimitiveFunction("append-bytes!", -1, AppendBytesBang)

    // testing
    MakePrimitiveFunction("describe", -1, Describe)

}

func IsPair(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    val, err := Eval(Car(args), env)
    return BooleanWithValue(PairP(val)), nil
}

func IsAlist(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    val, err := Eval(Car(args), env)
    return BooleanWithValue(AlistP(val)), nil
}

func ExposedNilP(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    val, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return BooleanWithValue(NilP(val)), nil
}

func ExposedNotNilP(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    val, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return BooleanWithValue(NotNilP(val)), nil
}

func IsSymbol(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    val, err := Eval(Car(args), env)
    return BooleanWithValue(SymbolP(val)), nil
}

func IsString(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    val, _ := Eval(Car(args), env)
    return BooleanWithValue(StringP(val)), nil
}

func IsNumber(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    val, _ := Eval(Car(args), env)
    return BooleanWithValue(NumberP(val)), nil
}

func IsFloat(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    val, _ := Eval(Car(args), env)
    return BooleanWithValue(FloatP(val)), nil
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
    val, _ := Eval(Car(args), env)
    return BooleanWithValue(FunctionP(val)), nil
}

func addFloats(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var acc float32 = 0
    var n *Data
    for c := args; NotNilP(c); c = Cdr(c) {
        n, err = Eval(Car(c), env)
        acc += FloatValue(n)
    }
    return FloatWithValue(acc), nil
}

func addInts(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var acc uint32 = 0
    var n *Data
    for c := args; NotNilP(c); c = Cdr(c) {
        n, err = Eval(Car(c), env)
        acc += NumericValue(n)
    }
    return NumberWithValue(acc), nil
}

func anyFloats(args *Data, env *SymbolTableFrame) (result bool, err error) {
    var n *Data
    for c := args; NotNilP(c); c = Cdr(c) {
        n, err = Eval(Car(c), env)
        if err != nil {
            return
        } else if !NumberP(n) && !FloatP(n) {
            err = errors.New(fmt.Sprintf("Number expected, received %s", String(n)))
            return
        }
        if FloatP(n) {
            return true, nil
        }
    }
    return false, nil
}

func Add(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    areFloats, err := anyFloats(args, env)
    if err != nil {
        return
    }
    if areFloats {
        return addFloats(args, env)
    } else {
        return addInts(args, env)
    }
}

func subtractInts(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var n *Data
    n, err = Eval(Car(args), env)
    acc := NumericValue(n)
    for c := Cdr(args); NotNilP(c); c = Cdr(c) {
        n, err = Eval(Car(c), env)
        if NumericValue(n) > acc {
            return NumberWithValue(0), nil
        } else {
            acc -= NumericValue(n)
        }

    }
    return NumberWithValue(acc), nil
}

func subtractFloats(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var n *Data
    n, _ = Eval(Car(args), env)
    acc := FloatValue(n)
    for c := Cdr(args); NotNilP(c); c = Cdr(c) {
        n, _ = Eval(Car(c), env)
        acc -= FloatValue(n)
    }
    return FloatWithValue(acc), nil
}

func Subtract(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    areFloats, err := anyFloats(args, env)
    if err != nil {
        return
    }
    if areFloats {
        return subtractFloats(args, env)
    } else {
        return subtractInts(args, env)
    }
}

func multiplyInts(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var n *Data
    var acc uint32 = 1
    for c := args; NotNilP(c); c = Cdr(c) {
        n, err = Eval(Car(c), env)
        acc *= NumericValue(n)
    }
    return NumberWithValue(acc), nil
}

func multiplyFloats(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var n *Data
    var acc float32 = 1.0
    for c := args; NotNilP(c); c = Cdr(c) {
        n, err = Eval(Car(c), env)
        acc *= FloatValue(n)
    }
    return FloatWithValue(acc), nil
}

func Multiply(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    areFloats, err := anyFloats(args, env)
    if err != nil {
        return
    }
    if areFloats {
        return multiplyFloats(args, env)
    } else {
        return multiplyInts(args, env)
    }
}

func quotientInts(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var n *Data
    n, err = Eval(Car(args), env)
    var acc uint32 = NumericValue(n)
    for c := Cdr(args); NotNilP(c); c = Cdr(c) {
        n, err = Eval(Car(c), env)
        acc /= NumericValue(n)
    }
    return NumberWithValue(acc), nil
}

func quotientFloats(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var n *Data
    n, err = Eval(Car(args), env)
    var acc float32 = FloatValue(n)
    for c := Cdr(args); NotNilP(c); c = Cdr(c) {
        n, err = Eval(Car(c), env)
        acc /= FloatValue(n)
    }
    return FloatWithValue(acc), nil
}

func Quotient(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    areFloats, err := anyFloats(args, env)
    if err != nil {
        return
    }
    if areFloats {
        return quotientFloats(args, env)
    } else {
        return quotientInts(args, env)
    }
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

func ToInt(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    n, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    if TypeOf(n) != NumberType && TypeOf(n) != FloatType {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(n)))
        return
    }

    return NumberWithValue(NumericValue(n)), nil
}

func ToFloat(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    n, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    if TypeOf(n) != NumberType && TypeOf(n) != FloatType {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(n)))
        return
    }

    return FloatWithValue(FloatValue(n)), nil
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
    if TypeOf(arg1) != NumberType && TypeOf(arg1) != FloatType {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(arg1)))
        return
    }

    var arg2 *Data
    arg2, err = Eval(Cadr(args), env)
    if err != nil {
        return
    }
    if TypeOf(arg2) != NumberType && TypeOf(arg2) != FloatType {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(arg2)))
        return
    }

    val := FloatValue(arg1) < FloatValue(arg2)
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
    if TypeOf(arg1) != NumberType && TypeOf(arg1) != FloatType {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(arg1)))
        return
    }

    var arg2 *Data
    arg2, err = Eval(Cadr(args), env)
    if err != nil {
        return
    }
    if TypeOf(arg2) != NumberType && TypeOf(arg2) != FloatType {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(arg2)))
        return
    }

    val := FloatValue(arg1) > FloatValue(arg2)
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
    if TypeOf(arg1) != NumberType && TypeOf(arg1) != FloatType {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(arg1)))
        return
    }

    var arg2 *Data
    arg2, err = Eval(Cadr(args), env)
    if err != nil {
        return
    }
    if TypeOf(arg2) != NumberType && TypeOf(arg2) != FloatType {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(arg2)))
        return
    }

    val := FloatValue(arg1) <= FloatValue(arg2)
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
    if TypeOf(arg1) != NumberType && TypeOf(arg1) != FloatType {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(arg1)))
        return
    }

    var arg2 *Data
    arg2, err = Eval(Cadr(args), env)
    if err != nil {
        return
    }
    if TypeOf(arg2) != NumberType && TypeOf(arg2) != FloatType {
        err = errors.New(fmt.Sprintf("Number expected, received %s", String(arg2)))
        return
    }

    val := FloatValue(arg1) >= FloatValue(arg2)
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
    thenClause := Second(args)
    elseClause := Third(args)

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

func ExposedFlatten(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var val *Data
    val, err = Eval(Car(args), env)
    if err != nil {
        return
    }
    result, err = Flatten(val)
    return
}

func ExposedRecursiveFlatten(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var val *Data
    val, err = Eval(Car(args), env)
    if err != nil {
        return
    }
    result, err = RecursiveFlatten(val)
    return
}

func ExposeAppendBang(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    firstList, err := Eval(Car(args), env)
    if err != nil {
        return
    }

    secondList, err := Eval(Cadr(args), env)
    if err != nil {
        return
    }

    result = AppendBangList(firstList, secondList)

    if SymbolP(Car(args)) {
        env.BindTo(Car(args), result)
    }

    return
}

func ExposeAppend(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    firstList, err := Eval(Car(args), env)
    if err != nil {
        return
    }

    secondList, err := Eval(Cadr(args), env)
    if err != nil {
        return
    }

    result = AppendList(Copy(firstList), secondList)
    return
}

func ExposeCopy(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    d, err := Eval(Car(args), env)
    if err != nil {
        return
    }

    return Copy(d), nil
}

func Partition(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    n, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    size := int(NumericValue(n))

    l, err := Eval(Cadr(args), env)
    if err != nil {
        return
    }
    if !ListP(l) {
        err = errors.New("partition requires a list as it's second argument.")
    }

    var pieces []*Data = make([]*Data, 0, 5)
    var chunk []*Data = make([]*Data, 0, 5)
    for c := l; NotNilP(c); c = Cdr(c) {
        if len(chunk) < size {
            chunk = append(chunk, Car(c))
        } else {
            pieces = append(pieces, ArrayToList(chunk))
            chunk = make([]*Data, 0, 5)
            chunk = append(chunk, Car(c))
        }
    }
    if len(chunk) > 0 {
        pieces = append(pieces, ArrayToList(chunk))
    }

    return ArrayToList(pieces), nil
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
    if !ListP(col) {
        err = errors.New("Map needs a list as its second argument")
        return
    }

    var d []*Data = make([]*Data, 0, Length(col))
    var v *Data
    for c := col; NotNilP(c); c = Cdr(c) {
        v, err = ApplyWithoutEval(f, Cons(Car(c), nil), env)
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
    return WalkList(a, "a"), nil
}

func ExposedCdr(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "d"), nil
}

func ExposedCaar(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "aa"), nil
}

func ExposedCadr(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "ad"), nil
}

func ExposedCdar(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "da"), nil
}

func ExposedCddr(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "dd"), nil
}

func ExposedCaaar(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "aaa"), nil
}

func ExposedCaadr(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "aad"), nil
}

func ExposedCadar(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "ada"), nil
}

func ExposedCaddr(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "add"), nil
}

func ExposedCdaar(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "daa"), nil
}

func ExposedCdadr(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "dad"), nil
}

func ExposedCddar(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "dda"), nil
}

func ExposedCdddr(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "ddd"), nil
}

func ExposedCaaaar(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "aaaa"), nil
}

func ExposedCaaadr(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "aaad"), nil
}

func ExposedCaadar(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "aada"), nil
}

func ExposedCaaddr(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "aadd"), nil
}

func ExposedCadaar(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "adaa"), nil
}

func ExposedCadadr(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "adad"), nil
}

func ExposedCaddar(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "adda"), nil
}

func ExposedCadddr(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "addd"), nil
}

func ExposedCdaaar(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "daaa"), nil
}

func ExposedCdaadr(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "daad"), nil
}

func ExposedCdadar(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "dada"), nil
}

func ExposedCdaddr(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "dadd"), nil
}

func ExposedCddaar(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "ddaa"), nil
}

func ExposedCddadr(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "ddad"), nil
}

func ExposedCdddar(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "ddda"), nil
}

func ExposedCddddr(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "dddd"), nil
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

func ExposedSixth(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Sixth(a), nil
}

func ExposedSeventh(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Seventh(a), nil
}

func ExposedEighth(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Eighth(a), nil
}

func ExposedNinth(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Ninth(a), nil
}

func ExposedTenth(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Tenth(a), nil
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

    key, err = Eval(First(args), env)
    if err != nil {
        return
    }

    if PairP(key) {
        err = errors.New("Alist key can not be a list")
        return
    }

    value, err = Eval(Second(args), env)
    if err != nil {
        return
    }

    if Length(args) == 3 {
        alist, err = Eval(Third(args), env)
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

    result, err = Eval(Third(args), env)
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
    if !PairP(pair) {
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
    if !PairP(pair) {
        err = errors.New("set-cdr! requires a pair as it's first argument.")
    }
    value, err := Eval(Cadr(args), env)
    if err != nil {
        return
    }
    pair.Cdr = value
    return value, nil
}

func SetNth(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    l, err := Eval(First(args), env)
    if !ListP(l) {
        err = errors.New("set-nth! requires a list as it's first argument.")
    }
    index, err := Eval(Second(args), env)
    if err != nil {
        return
    }
    value, err := Eval(Third(args), env)
    if err != nil {
        return
    }

    for i := NumericValue(index); i > 1; l, i = Cdr(l), i-1 {
    }
    if !NilP(l) {
        l.Car = value
    }

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
        name = First(bindingTuple)
        if NotNilP(Third(bindingTuple)) {
            value, err = Eval(Third(bindingTuple), env)
            if err != nil {
                return
            }
            env.BindLocallyTo(name, value)
        }
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

func DefApply(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    f, err := Eval(Car(args), env)
    if err != nil {
        return
    }

    vals, err := Eval(Cadr(args), env)
    if err != nil {
        return
    }

    return Apply(f, vals, env)
}

func DefQuit(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    WriteHistoryToFile(".golisp_history")
    os.Exit(0)
    return
}

func DefDebug(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    if Length(args) == 1 {
        DebugTrace = BooleanValue(Car(args))
    }
    return BooleanWithValue(DebugTrace), nil
}

func DefSleep(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    n, err := Eval(Car(args), env)
    if err != nil {
        return
    }
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
    println(PrintString(data))
    return
}

func MakeString(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    pieces := make([]string, 2)
    for cell := args; NotNilP(cell); cell = Cdr(cell) {
        sexpr := Car(cell)
        s, err := Eval(sexpr, env)
        if err != nil {
            break
        }
        pieces = append(pieces, PrintString(s))
    }
    return StringWithValue(strings.Join(pieces, "")), nil
}

func DefTime(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    fmt.Printf("Starting timer.\n")
    startTime := time.Now()

    for cell := args; NotNilP(cell); cell = Cdr(cell) {
        sexpr := Car(cell)
        result, err = Eval(sexpr, env)
        if err != nil {
            break
        }
    }

    d := time.Since(startTime)
    fmt.Printf("Stopped timer.\nTook %v to run.\n", d)
    result = NumberWithValue(uint32(d.Nanoseconds() / 1000000))
    return
}

func ListToBytes(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    if NilP(Car(args)) {
        err = errors.New("Argument to ListToByutes can not be nil.")
        return
    }
    list, err := Eval(Car(args), env)
    if err != nil {
        return
    }

    bytes := make([]byte, 0, int(Length(list)))
    for c := list; NotNilP(c); c = Cdr(c) {
        var n *Data
        n, err = Eval(Car(c), env)
        if !NumberP(n) {
            err = errors.New(fmt.Sprintf("Byte arrays can only contain numbers, but found %v.", n))
            return
        }
        b := NumericValue(n)
        if b > 255 {
            err = errors.New(fmt.Sprintf("Byte arrays can only contain bytes, but found %d.", b))
            return
        }
        bytes = append(bytes, byte(b))
    }
    return ObjectWithTypeAndValue("[]byte", unsafe.Pointer(&bytes)), nil
}

func BytesToList(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    dataByteObject, err := Eval(Car(args), env)
    if err != nil {
        panic(err)
    }
    if !ObjectP(dataByteObject) || TypeOfObject(dataByteObject) != "[]byte" {
        err = errors.New(fmt.Sprintf("Bytearray object should return []byte but returned %s.", TypeOfObject(dataByteObject)))
        return
    }

    dataBytes := (*[]byte)(ObjectValue(dataByteObject))
    var bytes = make([]*Data, 0, len(*dataBytes))

    for _, b := range *dataBytes {
        bytes = append(bytes, NumberWithValue(uint32(b)))
    }

    result = ArrayToList(bytes)
    return
}

func internalReplaceByte(args *Data, env *SymbolTableFrame, makeCopy bool) (result *Data, err error) {

    if First(args) == nil {
        err = errors.New("replace-byte requires a non-nil bytearray argument.")
        return
    }
    if Second(args) == nil {
        err = errors.New("replace-byte requires a non-nil index argument.")
        return
    }
    if Third(args) == nil {
        err = errors.New("replace-byte requires a non-nil value argument.")
        return
    }

    dataByteObject, err := Eval(First(args), env)
    if err != nil {
        panic(err)
    }
    if !ObjectP(dataByteObject) || TypeOfObject(dataByteObject) != "[]byte" {
        err = errors.New(fmt.Sprintf("Bytearray object should return []byte but returned %s.", TypeOfObject(dataByteObject)))
        return
    }

    dataBytes := (*[]byte)(ObjectValue(dataByteObject))
    var newBytes *[]byte
    if makeCopy {
        temp := make([]byte, len(*dataBytes))
        newBytes = &temp
        copy(*newBytes, *dataBytes)
    } else {
        newBytes = dataBytes
    }

    indexObject, err := Eval(Second(args), env)
    if err != nil {
        panic(err)
    }
    if !NumberP(indexObject) {
        panic(errors.New("Bytearray index should be a number."))
    }
    index := int(NumericValue(indexObject))

    if index >= len(*dataBytes) {
        err = errors.New(fmt.Sprintf("replace-byte index was out of range. Was %d but bytearray has length of %d.", index, len(*dataBytes)))
        return
    }

    if WalkList(args, "add") == nil {
        err = errors.New("replace-byte requires a non-nil value argument.")
        return
    }

    valueObject, err := Eval(Third(args), env)
    if err != nil {
        panic(err)
    }
    if !NumberP(valueObject) {
        panic(errors.New("Bytearray value should be a number."))
    }

    value := byte(NumericValue(valueObject))

    if value > 255 {
        err = errors.New(fmt.Sprintf("replace-byte value was not a byte. Was %d.", index))
        return
    }

    (*newBytes)[index] = value

    if makeCopy {
        result = ObjectWithTypeAndValue("[]byte", unsafe.Pointer(newBytes))
    } else {
        result = dataByteObject
    }
    return
}

func ReplaceByte(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    return internalReplaceByte(args, env, true)
}

func ReplaceByteBang(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    return internalReplaceByte(args, env, false)
}

func ExtractByte(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    if Car(args) == nil {
        err = errors.New("extract-byte requires a non-nil bytearray argument.")
        return
    }
    if Cadr(args) == nil {
        err = errors.New("extract-byte requires a non-nil index argument.")
        return
    }

    dataByteObject, err := Eval(Car(args), env)
    if err != nil {
        panic(err)
    }
    if !ObjectP(dataByteObject) || TypeOfObject(dataByteObject) != "[]byte" {
        panic(errors.New(fmt.Sprintf("Bytearray object should return []byte but returned %s.", TypeOfObject(dataByteObject))))
    }

    dataBytes := (*[]byte)(ObjectValue(dataByteObject))

    indexObject, err := Eval(Cadr(args), env)
    if err != nil {
        panic(err)
    }
    if !NumberP(indexObject) {
        panic(errors.New("Bytearray index should be a number."))
    }
    index := int(NumericValue(indexObject))

    if index >= len(*dataBytes) {
        err = errors.New(fmt.Sprintf("extract-byte index was out of range. Was %d but bytearray has length of %d.", index, len(*dataBytes)))
        return
    }

    extractedValue := (*dataBytes)[index]
    result = NumberWithValue(uint32(extractedValue))
    return
}

func internalAppendBytes(args *Data, env *SymbolTableFrame) (newBytes *[]byte, err error) {
    if Car(args) == nil {
        err = errors.New("append-bytes requires a non-nil bytearray argument.")
        return
    }
    if Cadr(args) == nil {
        err = errors.New("append-bytes requires a non-nil list of bytes to append.")
        return
    }

    dataByteObject, err := Eval(Car(args), env)
    if err != nil {
        panic(err)
    }
    if !ObjectP(dataByteObject) || TypeOfObject(dataByteObject) != "[]byte" {
        panic(errors.New(fmt.Sprintf("Bytearray object should return []byte but returned %s.", TypeOfObject(dataByteObject))))
    }

    dataBytes := (*[]byte)(ObjectValue(dataByteObject))

    var extraByteObj *Data
    var evaledArg *Data
    if NilP(Cddr(args)) {
        evaledArg, err = Eval(Cadr(args), env)
        if err != nil {
            return
        }
        if ObjectP(evaledArg) && TypeOfObject(evaledArg) == "[]byte" {
            extraByteObj = evaledArg
        } else if ListP(evaledArg) {
            extraByteObj, err = ListToBytes(InternalMakeList(QuoteIt(evaledArg)), env)
        } else {
            extraByteObj, err = ListToBytes(InternalMakeList(QuoteIt(Cdr(args))), env)
        }
    } else {
        extraByteObj, err = ListToBytes(InternalMakeList(QuoteIt(Cdr(args))), env)
    }

    if err != nil {
        return
    }

    extraBytes := (*[]byte)(ObjectValue(extraByteObj))

    temp := make([]byte, len(*dataBytes)+len(*extraBytes))
    newBytes = &temp
    copy(*newBytes, *dataBytes)
    copy((*newBytes)[len(*dataBytes):], *extraBytes)

    return
}

func AppendBytes(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    newBytesPtr, err := internalAppendBytes(args, env)
    if err != nil {
        return
    }
    result = ObjectWithTypeAndValue("[]byte", unsafe.Pointer(newBytesPtr))
    return
}

func AppendBytesBang(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    newBytesPtr, err := internalAppendBytes(args, env)
    dataByteObject, _ := Eval(Car(args), env)
    dataByteObject.Obj = unsafe.Pointer(newBytesPtr)
    result = dataByteObject
    return
}

func DefChain(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    if Length(args) == 0 {
        err = errors.New("-> requires at least an initial value.")
        return
    }

    var value *Data

    value, err = Eval(Car(args), env)
    if err != nil {
        return
    }

    for cell := Cdr(args); NotNilP(cell); cell = Cdr(cell) {
        sexpr := Car(cell)
        var newExpr *Data
        if ListP(sexpr) {
            newExpr = Cons(Car(sexpr), Cons(value, Cdr(sexpr)))
        } else {
            newExpr = Cons(sexpr, Cons(value, nil))
        }
        value, err = Eval(newExpr, env)
        if err != nil {
            return
        }
    }
    result = value
    return
}

func DefTap(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    if Length(args) == 0 {
        err = errors.New("tap requires at least an initial value.")
        return
    }

    var value *Data

    value, err = Eval(Car(args), env)
    if err != nil {
        return
    }
    result = value

    for cell := Cdr(args); NotNilP(cell); cell = Cdr(cell) {
        sexpr := Car(cell)
        var newExpr *Data
        if ListP(sexpr) {
            newExpr = Cons(Car(sexpr), Cons(value, Cdr(sexpr)))
        } else {
            newExpr = Cons(sexpr, Cons(value, nil))
        }
        _, err = Eval(newExpr, env)
        if err != nil {
            return
        }
    }
    return
}

/// Function template
// func <function>(args *Data, env *SymbolTableFrame) (result *Data, err error) {
// }
