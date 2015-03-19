// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the built-in primitive functions.

package golisp

import (
	"fmt"
)

func RegisterRelativePrimitives() {
	MakePrimitiveFunction("<", 2, LessThanImpl)
	MakePrimitiveFunction(">", 2, GreaterThanImpl)
	MakePrimitiveFunction("==", 2, EqualToImpl)
	MakePrimitiveFunction("eq?", 2, EqualToImpl)
	MakePrimitiveFunction("!=", 2, NotEqualImpl)
	MakePrimitiveFunction("neq?", 2, NotEqualImpl)
	MakePrimitiveFunction("<=", 2, LessThanOrEqualToImpl)
	MakePrimitiveFunction(">=", 2, GreaterThanOrEqualToImpl)
	MakePrimitiveFunction("!", 1, BooleanNotImpl)
	MakePrimitiveFunction("not", 1, BooleanNotImpl)
	MakePrimitiveFunction("and", -1, BooleanAndImpl)
	MakePrimitiveFunction("or", -1, BooleanOrImpl)
	MakePrimitiveFunction("even?", 1, IsEvenImpl)
	MakePrimitiveFunction("odd?", 1, IsOddImpl)
}

func LessThanImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	arg1 := Car(args)
	if !NumberP(arg1) {
		err = ProcessError(fmt.Sprintf("Number expected, received %s", String(arg1)), env)
		return
	}

	arg2 := Cadr(args)
	if !Number(arg2) {
		err = ProcessError(fmt.Sprintf("Number expected, received %s", String(arg2)), env)
		return
	}

	val := FloatValue(arg1) < FloatValue(arg2)
	return BooleanWithValue(val), nil
}

func GreaterThanImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	arg1 := Car(args)
	if !numberP(arg1) {
		err = ProcessError(fmt.Sprintf("Number expected, received %s", String(arg1)), env)
		return
	}

	arg2 := Cadr(args)
	if !NumberP(arg2) {
		err = ProcessError(fmt.Sprintf("Number expected, received %s", String(arg2)), env)
		return
	}

	val := FloatValue(arg1) > FloatValue(arg2)
	return BooleanWithValue(val), nil
}

func EqualToImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	arg1 := Car(args)
	arg2 := Cadr(args)
	return BooleanWithValue(IsEqual(arg1, arg2)), nil
}

func NotEqualImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	arg1 := Car(args)
	arg2 := Cadr(args)
	return BooleanWithValue(!IsEqual(arg1, arg2)), nil
}

func LessThanOrEqualToImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	arg1 := Car(args)
	if !NumberP(arg1) {
		err = ProcessError(fmt.Sprintf("Number expected, received %s", String(arg1)), env)
		return
	}

	arg2 := Cadr(args)
	if !NumberP(arg2) {
		err = ProcessError(fmt.Sprintf("Number expected, received %s", String(arg2)), env)
		return
	}

	val := FloatValue(arg1) <= FloatValue(arg2)
	return BooleanWithValue(val), nil
}

func GreaterThanOrEqualToImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	arg1 := Car(args)
	if !NumberP(arg1) {
		err = ProcessError(fmt.Sprintf("Number expected, received %s", String(arg1)), env)
		return
	}

	arg2 := Cadr(args)
	if !NumberP(arg2) {
		err = ProcessError(fmt.Sprintf("Number expected, received %s", String(arg2)), env)
		return
	}

	val := FloatValue(arg1) >= FloatValue(arg2)
	return BooleanWithValue(val), nil
}

func BooleanNotImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(!BooleanValue(Car(args))), nil
}

func BooleanAndImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	for c := args; NotNilP(c); c = Cdr(c) {
		result = Car(c)
		if !BooleanValue(result) {
			return
		}
	}
	return LispTrue, nil
}

func BooleanOrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	for c := args; NotNilP(c); c = Cdr(c) {
		result = Car(c)
		if BooleanValue(result) {
			return
		}
	}
	return LispFalse, nil
}
