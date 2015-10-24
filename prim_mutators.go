// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the mutator primitive functions.

package golisp

import ()

func RegisterMutatorPrimitives() {
	MakeSpecialForm("set!", "2", SetVarImpl)
	MakeSpecialForm("set-car!", "2", SetCarImpl)
	MakeSpecialForm("set-cdr!", "2", SetCdrImpl)
	MakeSpecialForm("set-nth!", "3", SetNthImpl)
}

func SetVarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	symbol := Car(args)
	if !SymbolP(symbol) {
		err = ProcessError("set! requires a raw (unevaluated) symbol as it's first argument.", env)
	}
	value, err := Eval(Cadr(args), env)
	if err != nil {
		return
	}
	return env.SetTo(symbol, value)
}

func SetCarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !ListP(l) {
		err = ProcessError("set-car! requires a list as it's first argument.", env)
	}
	value, err := Eval(Cadr(args), env)
	if err != nil {
		return
	}

	if VectorizedListP(l) {
		vect := VectorizedListValue(l)
		if len(vect) == 0 { // if we want to set the car of an empty vectorized list, convert to a cons list
			l = EmptyCons()
			if SymbolP(Car(args)) { // it is was bound to something, update the binding
				env.SetTo(Car(args), l)
			}
		} else { // otherwise just change the contexts of the underlying array
			vect[0] = value
			return value, nil
		}
	}

	ConsValue(l).Car = value

	return value, nil
}

func SetCdrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !ListP(l) {
		err = ProcessError("set-cdr! requires a list as it's first argument.", env)
	}
	if VectorizedListP(l) {
		l = ArrayToList(VectorizedListValue(l))
		if SymbolP(Car(args)) { // it is was bound to something, update the binding
			env.SetTo(Car(args), l)
		}
	}

	value, err := Eval(Cadr(args), env)
	if err != nil {
		return
	}
	ConsValue(l).Cdr = value

	return value, nil
}

func SetNthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	index, err := Eval(First(args), env)
	if err != nil {
		return
	}
	if !IntegerP(index) {
		err = ProcessError("set-nth! requires an integer as it's first argument.", env)
	}

	l, err := Eval(Second(args), env)
	if err != nil {
		return
	}
	if !ListP(l) {
		err = ProcessError("set-nth! requires a list as it's second argument.", env)
	}

	i := int(IntegerValue(index))
	if i < 0 || i >= Length(l) {
		err = ProcessError("set-nth! was given an out of bound index.", env)
	}

	value, err := Eval(Third(args), env)
	if err != nil {
		return
	}

	return SetNth(l, i, value), nil

}
