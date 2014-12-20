// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the list access primitive functions.

package golisp

import (
	"fmt"
)

func RegisterListFunctionsPrimitives() {
	MakePrimitiveFunction("map", -1, MapImpl)
	MakePrimitiveFunction("reduce", 3, ReduceImpl)
	MakePrimitiveFunction("filter", 2, FilterImpl)
	MakePrimitiveFunction("memq", 2, MemqImpl)
	MakePrimitiveFunction("memp", 2, MempImpl)
}

func intMin(x, y int) int {
	if x < y {
		return x
	} else {
		return y
	}
}

func MapImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f, err := Eval(First(args), env)
	if err != nil {
		return
	}
	if !FunctionP(f) {
		err = ProcessError(fmt.Sprintf("map needs a function as its first argument, but got %s.", String(f)), env)
		return
	}

	var collections []*Data = make([]*Data, 0, Length(args)-1)
	var loopCount = 30000
	var col *Data
	for a := Cdr(args); NotNilP(a); a = Cdr(a) {
		col, err = Eval(Car(a), env)
		if err != nil {
			return
		}
		if !ListP(col) {
			err = ProcessError(fmt.Sprintf("map needs lists as its other arguments, but got %s.", String(col)), env)
			return
		}
		collections = append(collections, col)
		loopCount = intMin(loopCount, Length(col))
	}

	var d []*Data = make([]*Data, 0, loopCount)
	var v *Data
	var a *Data
	for index := 1; index <= loopCount; index++ {
		mapArgs := make([]*Data, 0, len(collections))
		for _, mapArgCollection := range collections {
			a = Nth(mapArgCollection, index)
			mapArgs = append(mapArgs, a)
		}
		v, err = ApplyWithoutEval(f, ArrayToList(mapArgs), env)
		if err != nil {
			return
		}
		d = append(d, v)
	}

	return ArrayToList(d), nil
}

func ReduceImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f, err := Eval(First(args), env)
	if err != nil {
		return
	}
	if !FunctionP(f) {
		err = ProcessError("reduce needs a function as its first argument", env)
		return
	}

	initial, err := Eval(Second(args), env)
	if err != nil {
		return
	}

	col, err := Eval(Third(args), env)
	if err != nil {
		return
	}
	if !ListP(col) {
		err = ProcessError("map needs a list as its third argument", env)
		return
	}

	if Length(col) == 0 {
		return initial, nil
	}

	if Length(col) == 1 {
		return Car(col), nil
	}

	result = Car(col)
	for c := Cdr(col); NotNilP(c); c = Cdr(c) {
		result, err = ApplyWithoutEval(f, InternalMakeList(result, Car(c)), env)
		if err != nil {
			return
		}
	}

	return
}

func FilterImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f, err := Eval(First(args), env)
	if err != nil {
		return
	}
	if !FunctionP(f) {
		err = ProcessError(fmt.Sprintf("filter needs a function as its first argument, but got %s.", String(f)), env)
		return
	}

	col, err := Eval(Second(args), env)
	if err != nil {
		return
	}
	if !ListP(col) {
		err = ProcessError(fmt.Sprintf("filter needs a list as its second argument, but got %s.", String(col)), env)
		return
	}

	var d []*Data = make([]*Data, 0, Length(col))
	var v *Data
	for c := col; NotNilP(c); c = Cdr(c) {
		v, err = ApplyWithoutEval(f, Cons(Car(c), nil), env)
		if err != nil {
			return
		}
		if !BooleanP(v) {
			err = ProcessError("filter needs a predicate function as its first argument.", env)
			return
		}

		if BooleanValue(v) {
			d = append(d, Car(c))
		}
	}

	return ArrayToList(d), nil
}

func MemqImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	key, err := Eval(First(args), env)
	if err != nil {
		return
	}

	l, err := Eval(Second(args), env)
	if err != nil {
		return
	}

	for c := l; NotNilP(c); c = Cdr(c) {
		if IsEqual(key, Car(c)) {
			return c, nil
		}
	}

	return False, nil
}

func MempImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f, err := Eval(First(args), env)
	if err != nil {
		return
	}
	if !FunctionP(f) {
		err = ProcessError("memp needs a function as its first argument", env)
		return
	}

	l, err := Eval(Second(args), env)
	if err != nil {
		return
	}

	var found *Data
	for c := l; NotNilP(c); c = Cdr(c) {
		found, err = ApplyWithoutEval(f, InternalMakeList(Car(c)), env)
		if BooleanValue(found) {
			return c, nil
		}
	}

	return False, nil
}
