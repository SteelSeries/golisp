// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the mutator primitive functions.

package golisp

import (
	"fmt"
)

func RegisterMutatorPrimitives() {
	MakeSpecialForm("set!", "2", SetVarImpl)
	MakePrimitiveFunction("set-car!", "2", SetCarImpl)
	MakePrimitiveFunction("set-cdr!", "2", SetCdrImpl)
	MakePrimitiveFunction("set-nth!", "3", SetNthImpl)
}

func SetVarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	symbol := Car(args)
	if !SymbolP(symbol) {
		err = ProcessError(fmt.Sprintf("set! requires a raw (unevaluated) symbol as it's first argument, but got %s.", String(symbol)), env)
	}
	value, err := Eval(Cadr(args), env)
	if err != nil {
		return
	}
	return env.SetTo(symbol, value)
}

func SetCarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	pair := First(args)
	if !PairP(pair) {
		err = ProcessError(fmt.Sprintf("set-car! requires a pair as it's first argument, but got %s.", String(pair)), env)
	}
	value := Second(args)
	ConsValue(pair).Car = value
	return value, nil
}

func SetCdrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	pair := First(args)
	if !PairP(pair) {
		err = ProcessError(fmt.Sprintf("set-cdr! requires a pair as it's first argument, but got %s.", String(pair)), env)
	}
	value := Second(args)
	ConsValue(pair).Cdr = value
	return value, nil
}

func SetNthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	index := First(args)
	if !IntegerP(index) {
		err = ProcessError(fmt.Sprintf("set-nth! requires an integer index as it's first argument, but got %s.", String(index)), env)
		return
	}

	col := Second(args)
	if !ListP(col) && !VectorP(col) {
		err = ProcessError(fmt.Sprintf("set-nth! requires a list or vector as it's second argument, but got %s.", String(col)), env)
		return
	}

	value := Third(args)

	if VectorP(col) {
		return VectorSetImpl(InternalMakeList(col, index, value), env)
	}

	return SetNth(col, int(IntegerValue(index)), value), nil
}
