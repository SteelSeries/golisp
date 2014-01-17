// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the built-in primitive functions.

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
var quasiquoteLevel = 1

func init() {
    Global = NewSymbolTableFrameBelow(nil)
    InitBuiltins()
}

func InitBuiltins() {
    // MakePrimitiveFunction(<symbol>, <required # args, -1 means >= 1>, <function>)
    Global.Intern("nil")

    // type tests

    MakePrimitiveFunction("list?", 1, IsPairImpl)
    MakePrimitiveFunction("pair?", 1, IsPairImpl)
    MakePrimitiveFunction("alist?", 1, IsAlistImpl)
    MakePrimitiveFunction("nil?", 1, NilPImpl)
    MakePrimitiveFunction("notnil?", 1, NotNilPImpl)
    MakePrimitiveFunction("symbol?", 1, IsSymbolImpl)
    MakePrimitiveFunction("string?", 1, IsStringImpl)
    MakePrimitiveFunction("number?", 1, IsNumberImpl)
    MakePrimitiveFunction("float?", 1, IsFloatImpl)
    MakePrimitiveFunction("function?", 1, IsFunctionImpl)

    // math
    MakePrimitiveFunction("+", -1, AddImpl)
    MakePrimitiveFunction("-", -1, SubtractImpl)
    MakePrimitiveFunction("*", -1, MultiplyImpl)
    MakePrimitiveFunction("/", -1, QuotientImpl)
    MakePrimitiveFunction("%", 2, RemainderImpl)
    MakePrimitiveFunction("random-byte", 0, RandomByteImpl)
    MakePrimitiveFunction("interval", 2, IntervalImpl)
    MakePrimitiveFunction("integer", 1, ToIntImpl)
    MakePrimitiveFunction("float", 1, ToFloatImpl)

    // conditional/relative
    MakePrimitiveFunction("<", -1, LessThanImpl)
    MakePrimitiveFunction(">", -1, GreaterThanImpl)
    MakePrimitiveFunction("==", 2, EqualToImpl)
    MakePrimitiveFunction("!=", 2, NotEqualImpl)
    MakePrimitiveFunction("<=", -1, LessThanOrEqualToImpl)
    MakePrimitiveFunction(">=", -1, GreaterThanOrEqualToImpl)
    MakePrimitiveFunction("!", 1, BooleanNotImpl)
    MakePrimitiveFunction("and", -1, BooleanAndImpl)
    MakePrimitiveFunction("or", -1, BooleanOrImpl)
    MakePrimitiveFunction("even?", 1, IsEvenImpl)
    MakePrimitiveFunction("odd?", 1, IsOddImpl)

    // special forms
    MakePrimitiveFunction("cond", -1, CondImpl)
    MakePrimitiveFunction("case", -1, CaseImpl)
    MakePrimitiveFunction("if", -1, IfImpl)
    MakePrimitiveFunction("lambda", -1, LambdaImpl)
    MakePrimitiveFunction("define", -1, DefineImpl)
    MakePrimitiveFunction("defun", -1, DefunImpl)
    MakePrimitiveFunction("defmacro", -1, DefmacroImpl)
    MakePrimitiveFunction("map", 2, MapImpl)
    MakePrimitiveFunction("quote", 1, QuoteImpl)
    MakePrimitiveFunction("quasiquote", 1, QuasiquoteImpl)
    MakePrimitiveFunction("unquote", 1, UnquoteImpl)
    MakePrimitiveFunction("unquote-splicing", 1, UnquoteSplicingImpl)
    MakePrimitiveFunction("let", -1, LetImpl)
    MakePrimitiveFunction("begin", -1, BeginImpl)
    MakePrimitiveFunction("do", -1, DoImpl)
    MakePrimitiveFunction("apply", 2, ApplyImpl)
    MakePrimitiveFunction("eval", 1, EvalImpl)
    MakePrimitiveFunction("->", -1, ChainImpl)
    MakePrimitiveFunction("=>", -1, TapImpl)

    // setters
    MakePrimitiveFunction("set!", 2, SetVarImpl)
    MakePrimitiveFunction("set-car!", 2, SetCarImpl)
    MakePrimitiveFunction("set-cdr!", 2, SetCdrImpl)
    MakePrimitiveFunction("set-nth!", 3, SetNthImpl)

    // lists
    MakePrimitiveFunction("list", -1, MakeListImpl)
    MakePrimitiveFunction("length", 1, ListLengthImpl)
    MakePrimitiveFunction("cons", 2, ConsImpl)
    MakePrimitiveFunction("reverse", 1, ReverseImpl)
    MakePrimitiveFunction("flatten", 1, FlattenImpl)
    MakePrimitiveFunction("flatten*", 1, RecursiveFlattenImpl)
    MakePrimitiveFunction("append", 2, AppendImpl)
    MakePrimitiveFunction("append!", 2, AppendBangImpl)
    MakePrimitiveFunction("copy", 1, CopyImpl)
    MakePrimitiveFunction("partition", 2, PartitionImpl)

    MakePrimitiveFunction("car", 1, CarImpl)
    MakePrimitiveFunction("head", 1, CarImpl)
    MakePrimitiveFunction("cdr", 1, CdrImpl)
    MakePrimitiveFunction("rest", 1, CdrImpl)
    MakePrimitiveFunction("tail", 1, CdrImpl)

    MakePrimitiveFunction("caar", 1, CaarImpl)
    MakePrimitiveFunction("cadr", 1, CadrImpl)
    MakePrimitiveFunction("cdar", 1, CdarImpl)
    MakePrimitiveFunction("cddr", 1, CddrImpl)

    MakePrimitiveFunction("caaar", 1, CaaarImpl)
    MakePrimitiveFunction("caadr", 1, CaadrImpl)
    MakePrimitiveFunction("cadar", 1, CadarImpl)
    MakePrimitiveFunction("caddr", 1, CaddrImpl)
    MakePrimitiveFunction("cdaar", 1, CdaarImpl)
    MakePrimitiveFunction("cdadr", 1, CdadrImpl)
    MakePrimitiveFunction("cddar", 1, CddarImpl)
    MakePrimitiveFunction("cdddr", 1, CdddrImpl)

    MakePrimitiveFunction("caaaar", 1, CaaaarImpl)
    MakePrimitiveFunction("caaadr", 1, CaaadrImpl)
    MakePrimitiveFunction("caadar", 1, CaadarImpl)
    MakePrimitiveFunction("caaddr", 1, CaaddrImpl)
    MakePrimitiveFunction("cadaar", 1, CadaarImpl)
    MakePrimitiveFunction("cadadr", 1, CadadrImpl)
    MakePrimitiveFunction("caddar", 1, CaddarImpl)
    MakePrimitiveFunction("cadddr", 1, CadddrImpl)
    MakePrimitiveFunction("cdaaar", 1, CdaaarImpl)
    MakePrimitiveFunction("cdaadr", 1, CdaadrImpl)
    MakePrimitiveFunction("cdadar", 1, CdadarImpl)
    MakePrimitiveFunction("cdaddr", 1, CdaddrImpl)
    MakePrimitiveFunction("cddaar", 1, CddaarImpl)
    MakePrimitiveFunction("cddadr", 1, CddadrImpl)
    MakePrimitiveFunction("cdddar", 1, CdddarImpl)
    MakePrimitiveFunction("cddddr", 1, CddddrImpl)

    MakePrimitiveFunction("first", 1, FirstImpl)
    MakePrimitiveFunction("second", 1, SecondImpl)
    MakePrimitiveFunction("third", 1, ThirdImpl)
    MakePrimitiveFunction("fourth", 1, FourthImpl)
    MakePrimitiveFunction("fifth", 1, FifthImpl)
    MakePrimitiveFunction("sixth", 1, SixthImpl)
    MakePrimitiveFunction("seventh", 1, SeventhImpl)
    MakePrimitiveFunction("eighth", 1, EighthImpl)
    MakePrimitiveFunction("ninth", 1, NinthImpl)
    MakePrimitiveFunction("tenth", 1, TenthImpl)

    MakePrimitiveFunction("nth", 2, NthImpl)

    // association lists

    MakePrimitiveFunction("acons", -1, AconsImpl)
    MakePrimitiveFunction("pairlis", -1, PairlisImpl)
    MakePrimitiveFunction("assoc", 2, AssocImpl)
    MakePrimitiveFunction("rassoc", 2, RassocImpl)
    MakePrimitiveFunction("alist", 1, AlistImpl)

    // system
    MakePrimitiveFunction("load", 1, LoadFileImpl)
    MakePrimitiveFunction("dump", 0, DumpSymbolTableImpl)
    MakePrimitiveFunction("sleep", 1, SleepImpl)
    MakePrimitiveFunction("write-line", 1, WriteLineImpl)
    MakePrimitiveFunction("str", -1, MakeStringImpl)
    MakePrimitiveFunction("time", 1, TimeImpl)
    MakePrimitiveFunction("quit", 0, QuitImpl)
    MakePrimitiveFunction("debug", -1, DebugImpl)

    // bytearrays
    MakePrimitiveFunction("list-to-bytearray", 1, ListToBytesImpl)
    MakePrimitiveFunction("bytearray-to-list", 1, BytesToListImpl)
    MakePrimitiveFunction("replace-byte", 3, ReplaceByteImpl)
    MakePrimitiveFunction("replace-byte!", 3, ReplaceByteBangImpl)
    MakePrimitiveFunction("extract-byte", 2, ExtractByteImpl)
    MakePrimitiveFunction("append-bytes", -1, AppendBytesImpl)
    MakePrimitiveFunction("append-bytes!", -1, AppendBytesBangImpl)

    // testing
    MakePrimitiveFunction("describe", -1, DescribeImpl)

}

func IsPairImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    val, err := Eval(Car(args), env)
    return BooleanWithValue(PairP(val)), nil
}

func IsAlistImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    val, err := Eval(Car(args), env)
    return BooleanWithValue(AlistP(val)), nil
}

func NilPImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    val, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return BooleanWithValue(NilP(val)), nil
}

func NotNilPImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    val, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return BooleanWithValue(NotNilP(val)), nil
}

func IsSymbolImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    val, err := Eval(Car(args), env)
    return BooleanWithValue(SymbolP(val)), nil
}

func IsStringImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    val, _ := Eval(Car(args), env)
    return BooleanWithValue(StringP(val)), nil
}

func IsNumberImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    val, _ := Eval(Car(args), env)
    return BooleanWithValue(NumberP(val)), nil
}

func IsFloatImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    val, _ := Eval(Car(args), env)
    return BooleanWithValue(FloatP(val)), nil
}

func IsEvenImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    evaluated, _ := Eval(Car(args), env)
    if !NumberP(evaluated) {
        return False, nil
    }
    return BooleanWithValue((NumericValue(evaluated) % 2) == 0), nil
}

func IsOddImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    evaluated, _ := Eval(Car(args), env)
    if !NumberP(evaluated) {
        return False, nil
    }
    return BooleanWithValue((NumericValue(evaluated) % 2) == 1), nil
}

func IsFunctionImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func AddImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func SubtractImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func MultiplyImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func QuotientImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func RemainderImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func RandomByteImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    r := uint8(rand.Int())
    result = NumberWithValue(uint32(r))
    return
}

func IntervalImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func ToIntImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func ToFloatImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func LessThanImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func GreaterThanImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func EqualToImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func NotEqualImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func LessThanOrEqualToImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func GreaterThanOrEqualToImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func BooleanNotImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func BooleanAndImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    for c := args; NotNilP(c); c = Cdr(c) {
        result, err = Eval(Car(c), env)
        if !BooleanValue(result) {
            return
        }
    }
    result = True
    return
}

func BooleanOrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    for c := args; NotNilP(c); c = Cdr(c) {
        result, err = Eval(Car(c), env)
        if BooleanValue(result) {
            return
        }
    }
    result = False
    return
}

func CondImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func evalList(l *Data, env *SymbolTableFrame) (result *Data, err error) {
    for sexpr := l; NotNilP(sexpr); sexpr = Cdr(sexpr) {
        result, err = Eval(Car(sexpr), env)
        if err != nil {
            return
        }
    }
    return
}

func CaseImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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
                return evalList(Cdr(clause), env)
            } else {
                targetValue, err = Eval(Car(clause), env)
                if IsEqual(targetValue, keyValue) {
                    return evalList(Cdr(clause), env)
                }
            }
        } else {
            err = errors.New("Case requires non-atomic clauses")
            return
        }
    }

    return
}

func IfImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func ListLengthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    d, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return NumberWithValue(uint32(Length(d))), nil
}

func ConsImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func ReverseImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var val *Data
    val, err = Eval(Car(args), env)
    if err != nil {
        return
    }
    result = Reverse(val)
    return
}

func FlattenImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var val *Data
    val, err = Eval(Car(args), env)
    if err != nil {
        return
    }
    result, err = Flatten(val)
    return
}

func RecursiveFlattenImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var val *Data
    val, err = Eval(Car(args), env)
    if err != nil {
        return
    }
    result, err = RecursiveFlatten(val)
    return
}

func AppendBangImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func AppendImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func CopyImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    d, err := Eval(Car(args), env)
    if err != nil {
        return
    }

    return Copy(d), nil
}

func PartitionImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func LambdaImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    params := Car(args)
    body := Cdr(args)
    return FunctionWithNameParamsBodyAndParent("anonymous", params, body, env), nil
}

func DefineImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func DefunImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var value *Data
    thing := Car(args)
    if PairP(thing) {
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
        err = errors.New("Invalid function definition")
        return
    }
    env.BindLocallyTo(thing, value)
    return value, nil
}

func DefmacroImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var value *Data
    thing := Car(args)
    if PairP(thing) {
        name := Car(thing)
        params := Cdr(thing)
        thing = name
        if !SymbolP(name) {
            err = errors.New("Macro name has to be a symbol")
            return
        }
        body := Cadr(args)
        value = MacroWithNameParamsBodyAndParent(StringValue(name), params, body, env)
    } else {
        err = errors.New("Invalid macro definition")
        return
    }
    env.BindLocallyTo(thing, value)
    return value, nil
}

func DumpSymbolTableImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    env.Dump()
    return
}

func MapImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func QuoteImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    return Car(args), nil
}

func processQuasiquoted(sexpr *Data, env *SymbolTableFrame) (result *Data, err error) {
    if !ListP(sexpr) {
        return Cons(sexpr, nil), nil
    } else if SymbolP(Car(sexpr)) && StringValue(Car(sexpr)) == "quasiquote" {
        quasiquoteLevel += 1
        parts := make([]*Data, 0, Length(Cdr(sexpr)))
        for _, exp := range ToArray(Cdr(sexpr)) {
            processed, err := processQuasiquoted(exp, env)
            if err != nil {
                return nil, err
            }
            parts = append(parts, Car(processed))
        }
        return Cons(ArrayToList(parts), nil), nil
    } else if SymbolP(Car(sexpr)) && StringValue(Car(sexpr)) == "unquote" {
        if quasiquoteLevel == 1 {
            processed, err := processQuasiquoted(Cadr(sexpr), env)
            if err != nil {
                return nil, err
            }
            r, err := Eval(Car(processed), env)
            if err != nil {
                return nil, err
            }
            return Cons(r, nil), nil
        } else {
            quasiquoteLevel -= 1
        }
    } else if SymbolP(Car(sexpr)) && StringValue(Car(sexpr)) == "unquote-splicing" {
        if quasiquoteLevel == 1 {
            processed, err := processQuasiquoted(Cadr(sexpr), env)
            if err != nil {
                return nil, err
            }
            r, err := Eval(Car(processed), env)
            if err != nil {
                return nil, err
            }
            return r, nil
        } else {
            quasiquoteLevel -= 1
        }
    } else {
        parts := make([]*Data, 0, Length(Cdr(sexpr)))
        for _, exp := range ToArray(sexpr) {
            processed, err := processQuasiquoted(exp, env)
            if err != nil {
                return nil, err
            }
            parts = append(parts, processed)
        }
        flat, err := Flatten(ArrayToList(parts))
        if err != nil {
            return nil, err
        }
        return Cons(flat, nil), nil
    }
    return
}

func QuasiquoteImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    quasiquoteLevel = 1
    r, err := processQuasiquoted(Car(args), env)
    if err != nil {
        return nil, err
    }
    return Car(r), nil
}

func UnquoteImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    err = errors.New("unquote should not be used outside of a quasiquoted expression.")
    return
}

func UnquoteSplicingImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    err = errors.New("unquote-splicing should not be used outside of a quasiquoted expression.")
    return
}

func MakeListImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func CarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "a"), nil
}

func CdrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "d"), nil
}

func CaarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "aa"), nil
}

func CadrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "ad"), nil
}

func CdarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "da"), nil
}

func CddrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "dd"), nil
}

func CaaarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "aaa"), nil
}

func CaadrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "aad"), nil
}

func CadarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "ada"), nil
}

func CaddrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "add"), nil
}

func CdaarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "daa"), nil
}

func CdadrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "dad"), nil
}

func CddarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "dda"), nil
}

func CdddrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "ddd"), nil
}

func CaaaarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "aaaa"), nil
}

func CaaadrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "aaad"), nil
}

func CaadarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "aada"), nil
}

func CaaddrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "aadd"), nil
}

func CadaarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "adaa"), nil
}

func CadadrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "adad"), nil
}

func CaddarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "adda"), nil
}

func CadddrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "addd"), nil
}

func CdaaarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "daaa"), nil
}

func CdaadrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "daad"), nil
}

func CdadarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "dada"), nil
}

func CdaddrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "dadd"), nil
}

func CddaarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "ddaa"), nil
}

func CddadrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "ddad"), nil
}

func CdddarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "ddda"), nil
}

func CddddrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "dddd"), nil
}

func FirstImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return First(a), nil
}

func SecondImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Second(a), nil
}

func ThirdImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Third(a), nil
}

func FourthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Fourth(a), nil
}

func FifthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Fifth(a), nil
}

func SixthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Sixth(a), nil
}

func SeventhImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Seventh(a), nil
}

func EighthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Eighth(a), nil
}

func NinthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Ninth(a), nil
}

func TenthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Tenth(a), nil
}

func NthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func AlistImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    l, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    result = Alist(l)
    return
}

func AconsImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func PairlisImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func AssocImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func RassocImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func LoadFileImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    filename := Car(args)
    if !StringP(filename) {
        err = errors.New("Filename must be a string")
        return
    }

    return ProcessFile(StringValue(filename))
}

func SetVarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func SetCarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func SetCdrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func SetNthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func bindLetLocals(bindingForms *Data, env *SymbolTableFrame) (err error) {
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

func LetImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    if Length(args) < 1 {
        err = errors.New("Let requires at least a list of bindings")
        return
    }

    if !PairP(Car(args)) {
        err = errors.New("Let requires a list of bindings as it's first argument")
        return
    }

    localFrame := NewSymbolTableFrameBelow(env)
    bindLetLocals(Car(args), localFrame)

    for cell := Cdr(args); NotNilP(cell); cell = Cdr(cell) {
        sexpr := Car(cell)
        result, err = Eval(sexpr, localFrame)
        if err != nil {
            return
        }
    }

    return
}

func BeginImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    for cell := args; NotNilP(cell); cell = Cdr(cell) {
        sexpr := Car(cell)
        result, err = Eval(sexpr, env)
        if err != nil {
            return
        }
    }
    return
}

func rebindDoLocals(bindingForms *Data, env *SymbolTableFrame) (err error) {
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

func DoImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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
    bindLetLocals(bindings, localFrame)

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

        rebindDoLocals(bindings, localFrame)
    }
    return
}

func ApplyImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func EvalImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    val, err := Eval(Car(args), env)
    if err != nil {
        return
    }

    return Eval(val, env)
}

func QuitImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    WriteHistoryToFile(".golisp_history")
    os.Exit(0)
    return
}

func DebugImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    if Length(args) == 1 {
        DebugTrace = BooleanValue(Car(args))
    }
    return BooleanWithValue(DebugTrace), nil
}

func SleepImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func WriteLineImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    data, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    println(PrintString(data))
    return
}

func MakeStringImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func TimeImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func ListToBytesImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func BytesToListImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func ReplaceByteImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    return internalReplaceByte(args, env, true)
}

func ReplaceByteBangImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    return internalReplaceByte(args, env, false)
}

func ExtractByteImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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
            extraByteObj, err = ListToBytesImpl(InternalMakeList(QuoteIt(evaledArg)), env)
        } else {
            extraByteObj, err = ListToBytesImpl(InternalMakeList(QuoteIt(Cdr(args))), env)
        }
    } else {
        extraByteObj, err = ListToBytesImpl(InternalMakeList(QuoteIt(Cdr(args))), env)
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

func AppendBytesImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    newBytesPtr, err := internalAppendBytes(args, env)
    if err != nil {
        return
    }
    result = ObjectWithTypeAndValue("[]byte", unsafe.Pointer(newBytesPtr))
    return
}

func AppendBytesBangImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    newBytesPtr, err := internalAppendBytes(args, env)
    dataByteObject, _ := Eval(Car(args), env)
    dataByteObject.Obj = unsafe.Pointer(newBytesPtr)
    result = dataByteObject
    return
}

func ChainImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func TapImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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
