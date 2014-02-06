// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the built-in primitive functions.

package golisp

func RegisterTypePredicatePrimitives() {
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

func IsFunctionImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    val, _ := Eval(Car(args), env)
    return BooleanWithValue(FunctionP(val)), nil
}

