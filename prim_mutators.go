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
	MakeTypedPrimitiveFunction("set-car!", "2", SetCarImpl, []uint32{ConsCellType, AnyType})
	MakeTypedPrimitiveFunction("set-cdr!", "2", SetCdrImpl, []uint32{ConsCellType, AnyType})
	MakeTypedPrimitiveFunction("set-nth!", "3", SetNthImpl, []uint32{IntegerType, ConsCellType, AnyType})
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
	value := Second(args)
	ConsValue(pair).Car = value
	return value, nil
}

func SetCdrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	pair := First(args)
	value := Second(args)
	ConsValue(pair).Cdr = value
	return value, nil
}

func SetNthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	index := First(args)
	col := Second(args)
	value := Third(args)
	return SetNth(col, int(IntegerValue(index)), value), nil
}
