// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the mutator primitive functions.

package golisp

import (
    "errors"
)

func RegisterMutatorPrimitives() {
    MakePrimitiveFunction("set!", 2, SetVarImpl)
    MakePrimitiveFunction("set-car!", 2, SetCarImpl)
    MakePrimitiveFunction("set-cdr!", 2, SetCdrImpl)
    MakePrimitiveFunction("set-nth!", 3, SetNthImpl)
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

