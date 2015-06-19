// Copyright 2015 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the list set-like primitive functions.

package golisp

import (
	"fmt"
)

func RegisterListSetPrimitives() {
	MakePrimitiveFunction("union", "*", UnionImpl)
	MakePrimitiveFunction("intersection", "*", IntersectionImpl)
}

func memp(i *Data, l *Data) bool {
	for c := l; NotNilP(c); c = Cdr(c) {
		if IsEqual(i, Car(c)) {
			return true
		}
	}
	return false
}

func UnionImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var col *Data
	for a := args; NotNilP(a); a = Cdr(a) {
		col = Car(a)
		if !ListP(col) {
			err = ProcessError(fmt.Sprintf("union needs lists as its arguments, but got %s.", String(col)), env)
			return
		}
		for cell := col; NotNilP(cell); cell = Cdr(cell) {
			if !memp(Car(cell), result) {
				result = Append(result, Car(cell))
			}
		}
	}
	return
}

func IntersectionImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var col *Data
	firstList := Car(args)

	if !ListP(firstList) {
		err = ProcessError(fmt.Sprintf("intersection needs lists as its arguments, but got %s.", String(firstList)), env)
		return
	}

	result = Copy(firstList)
	for a := Cdr(args); NotNilP(a); a = Cdr(a) {
		col = Car(a)
		if !ListP(col) {
			err = ProcessError(fmt.Sprintf("intersection needs lists as its arguments, but got %s.", String(col)), env)
			return
		}
		for cell := result; NotNilP(cell); cell = Cdr(cell) {
			if !memp(Car(cell), col) {
				result = RemoveFromListBang(result, Car(cell))
			}
		}
	}
	return
}
