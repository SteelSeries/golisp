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
	MakePrimitiveFunction("<", ">=2", LessThanImpl)
	MakePrimitiveFunction(">", ">=2", GreaterThanImpl)
	MakePrimitiveFunction("<=", ">=2", LessThanOrEqualToImpl)
	MakePrimitiveFunction(">=", ">=2", GreaterThanOrEqualToImpl)
	MakePrimitiveFunction("==", "2", EqualImpl)
	MakePrimitiveFunction("=", "2", EqualImpl)
	MakePrimitiveFunction("eqv?", "2", EqvImpl)
	MakePrimitiveFunction("neqv?", "2", NotEqvImpl)
	MakePrimitiveFunction("eq?", "2", EqImpl)
	MakePrimitiveFunction("neq?", "2", NotEqImpl)
	MakePrimitiveFunction("equal?", "2", EqualImpl)
	MakePrimitiveFunction("nequal?", "2", NotEqualImpl)
	MakePrimitiveFunction("!=", "2", NotEqualImpl)
	MakePrimitiveFunction("/=", "2", NotEqualImpl)
	MakePrimitiveFunction("!", "1", BooleanNotImpl)
	MakePrimitiveFunction("not", "1", BooleanNotImpl)
	MakeSpecialForm("and", "*", BooleanAndImpl)
	MakeSpecialForm("or", "*", BooleanOrImpl)
}

func LessThanImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	lhs := Car(args)
	if !NumberP(lhs) {
		err = ProcessError(fmt.Sprintf("Number expected for <, received %s", String(lhs)), env)
		return
	}

	var holds = true
	for cell := Cdr(args); NotNilP(cell); cell = Cdr(cell) {
		rhs := Car(cell)
		if !NumberP(rhs) {
			err = ProcessError(fmt.Sprintf("Number expected for <, received %s", String(rhs)), env)
			return
		}

		holds = FloatValue(lhs) < FloatValue(rhs)
		if !holds {
			return LispFalse, nil
		}
		lhs = rhs
	}
	return LispTrue, nil
}

func GreaterThanImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	lhs := Car(args)
	if !NumberP(lhs) {
		err = ProcessError(fmt.Sprintf("Number expected for >, received %s", String(lhs)), env)
		return
	}

	var holds = true
	for cell := Cdr(args); NotNilP(cell); cell = Cdr(cell) {
		rhs := Car(cell)
		if !NumberP(rhs) {
			err = ProcessError(fmt.Sprintf("Number expected for >, received %s", String(rhs)), env)
			return
		}

		holds = FloatValue(lhs) > FloatValue(rhs)
		if !holds {
			return LispFalse, nil
		}
		lhs = rhs
	}
	return LispTrue, nil
}

func LessThanOrEqualToImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	lhs := Car(args)
	if !NumberP(lhs) {
		err = ProcessError(fmt.Sprintf("Number expected for <=, received %s", String(lhs)), env)
		return
	}

	var holds = true
	for cell := Cdr(args); NotNilP(cell); cell = Cdr(cell) {
		rhs := Car(cell)
		if !NumberP(rhs) {
			err = ProcessError(fmt.Sprintf("Number expected for <=, received %s", String(rhs)), env)
			return
		}

		holds = FloatValue(lhs) <= FloatValue(rhs)
		if !holds {
			return LispFalse, nil
		}
		lhs = rhs
	}
	return LispTrue, nil
}

func GreaterThanOrEqualToImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	lhs := Car(args)
	if !NumberP(lhs) {
		err = ProcessError(fmt.Sprintf("Number expected for >=, received %s", String(lhs)), env)
		return
	}

	var holds = true
	for cell := Cdr(args); NotNilP(cell); cell = Cdr(cell) {
		rhs := Car(cell)
		if !NumberP(rhs) {
			err = ProcessError(fmt.Sprintf("Number expected for >=, received %s", String(rhs)), env)
			return
		}

		holds = FloatValue(lhs) >= FloatValue(rhs)
		if !holds {
			return LispFalse, nil
		}
		lhs = rhs
	}
	return LispTrue, nil
}

func EqvImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	arg1 := Car(args)
	arg2 := Cadr(args)
	return BooleanWithValue(IsEqv(arg1, arg2)), nil
}

func NotEqvImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	arg1 := Car(args)
	arg2 := Cadr(args)
	return BooleanWithValue(!IsEqv(arg1, arg2)), nil
}

func EqImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	arg1 := Car(args)
	arg2 := Cadr(args)
	return BooleanWithValue(IsEq(arg1, arg2)), nil
}

func NotEqImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	arg1 := Car(args)
	arg2 := Cadr(args)
	return BooleanWithValue(!IsEq(arg1, arg2)), nil
}

func EqualImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	arg1 := Car(args)
	arg2 := Cadr(args)
	return BooleanWithValue(IsEqual(arg1, arg2)), nil
}

func NotEqualImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	arg1 := Car(args)
	arg2 := Cadr(args)
	return BooleanWithValue(!IsEqual(arg1, arg2)), nil
}

func BooleanNotImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(!BooleanValue(Car(args))), nil
}

func BooleanAndImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	for c := args; NotNilP(c); c = Cdr(c) {
		result, err = Eval(Car(c), env)
		if err != nil || !BooleanValue(result) {
			return
		}
	}
	return
}

func BooleanOrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	for c := args; NotNilP(c); c = Cdr(c) {
		result, err = Eval(Car(c), env)
		if err != nil || BooleanValue(result) {
			return
		}
	}
	return
}
