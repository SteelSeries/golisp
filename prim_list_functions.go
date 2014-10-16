// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the list access primitive functions.

package golisp

import (
	"errors"
)

func RegisterListFunctionsPrimitives() {
	MakePrimitiveFunction("map", 2, MapImpl)
	MakePrimitiveFunction("reduce", 3, ReduceImpl)
	MakePrimitiveFunction("memq", 2, MemqImpl)
}

func MapImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f, err := Eval(First(args), env)
	if err != nil {
		return
	}
	if !FunctionP(f) {
		err = errors.New("Map needs a function as its first argument")
		return
	}

	col, err := Eval(Second(args), env)
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

func ReduceImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f, err := Eval(First(args), env)
	if err != nil {
		return
	}
	if !FunctionP(f) {
		err = errors.New("Map needs a function as its first argument")
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
		err = errors.New("map needs a list as its third argument")
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
