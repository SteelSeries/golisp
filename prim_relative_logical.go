// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the built-in primitive functions.

package golisp

import (
	"errors"
	"fmt"
)

func RegisterRelativePrimitives() {
	MakePrimitiveFunction("<", -1, LessThanImpl)
	MakePrimitiveFunction(">", -1, GreaterThanImpl)
	MakePrimitiveFunction("==", 2, EqualToImpl)
	MakePrimitiveFunction("eq?", 2, EqualToImpl)
	MakePrimitiveFunction("!=", 2, NotEqualImpl)
	MakePrimitiveFunction("<=", -1, LessThanOrEqualToImpl)
	MakePrimitiveFunction(">=", -1, GreaterThanOrEqualToImpl)
	MakePrimitiveFunction("!", 1, BooleanNotImpl)
	MakePrimitiveFunction("and", -1, BooleanAndImpl)
	MakePrimitiveFunction("or", -1, BooleanOrImpl)
	MakePrimitiveFunction("even?", 1, IsEvenImpl)
	MakePrimitiveFunction("odd?", 1, IsOddImpl)
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
	if !IntegerP(arg1) && !FloatP(arg1) {
		err = errors.New(fmt.Sprintf("Number expected, received %s", String(arg1)))
		return
	}

	var arg2 *Data
	arg2, err = Eval(Cadr(args), env)
	if err != nil {
		return
	}
	if !IntegerP(arg2) && !FloatP(arg2) {
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
	if !IntegerP(arg1) && !FloatP(arg1) {
		err = errors.New(fmt.Sprintf("Number expected, received %s", String(arg1)))
		return
	}

	var arg2 *Data
	arg2, err = Eval(Cadr(args), env)
	if err != nil {
		return
	}
	if !IntegerP(arg2) && !FloatP(arg2) {
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
	if !IntegerP(arg1) && !FloatP(arg1) {
		err = errors.New(fmt.Sprintf("Number expected, received %s", String(arg1)))
		return
	}

	var arg2 *Data
	arg2, err = Eval(Cadr(args), env)
	if err != nil {
		return
	}
	if !IntegerP(arg2) && !FloatP(arg2) {
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
	if !IntegerP(arg1) && !FloatP(arg1) {
		err = errors.New(fmt.Sprintf("Number expected, received %s", String(arg1)))
		return
	}

	var arg2 *Data
	arg2, err = Eval(Cadr(args), env)
	if err != nil {
		return
	}
	if !IntegerP(arg2) && !FloatP(arg2) {
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
