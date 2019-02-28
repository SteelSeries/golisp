// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the list access primitive functions.

package golisp

import (
	"fmt"
	"math"
)

func RegisterListFunctionsPrimitives() {
	MakePrimitiveFunction("map", ">=2", MapImpl)
	MakePrimitiveFunction("for-each", ">=2", ForEachImpl)
	MakePrimitiveFunction("any", ">=2", AnyImpl)
	MakePrimitiveFunction("every", ">=2", EveryImpl)
	MakePrimitiveFunction("reduce", "3", ReduceLeftImpl)
	MakePrimitiveFunction("reduce-left", "3", ReduceLeftImpl)
	MakePrimitiveFunction("reduce-right", "3", ReduceRightImpl)
	MakePrimitiveFunction("fold-left", "3", FoldLeftImpl)
	MakePrimitiveFunction("fold-right", "3", FoldRightImpl)
	MakePrimitiveFunction("filter", "2", FilterImpl)
	MakePrimitiveFunction("remove", "2", RemoveImpl)
	MakePrimitiveFunction("memq", "2", MemqImpl)
	MakePrimitiveFunction("memv", "2", MemqImpl)
	MakePrimitiveFunction("member", "2", MemqImpl)
	MakePrimitiveFunction("memp", "2", FindTailImpl)
	MakePrimitiveFunction("find-tail", "2", FindTailImpl)
	MakePrimitiveFunction("find", "2", FindImpl)
}

func intMin(x, y int64) int64 {
	if x < y {
		return x
	} else {
		return y
	}
}

func MapImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	if !FunctionOrPrimitiveP(f) {
		err = ProcessError(fmt.Sprintf("map needs a function as its first argument, but got %s.", String(f)), env)
		return
	}

	var collections []*Data = make([]*Data, 0, Length(args)-1)
	var loopCount int64 = math.MaxInt64
	var col *Data
	for a := Cdr(args); NotNilP(a); a = Cdr(a) {
		col = Car(a)
		if !ListP(col) {
			err = ProcessError(fmt.Sprintf("map needs lists as its other arguments, but got %s.", String(col)), env)
			return
		}
		if NilP(col) || col == nil {
			return
		}
		collections = append(collections, col)
		loopCount = intMin(loopCount, int64(Length(col)))
	}

	if loopCount == math.MaxInt64 {
		return
	}

	var d []*Data = make([]*Data, 0, loopCount)
	var v *Data
	var a *Data
	for index := 1; index <= int(loopCount); index++ {
		mapArgs := make([]*Data, 0, len(collections))
		for key, mapArgCollection := range collections {
			a = Car(mapArgCollection)
			collections[key] = Cdr(mapArgCollection)
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

func ForEachImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	if !FunctionOrPrimitiveP(f) {
		err = ProcessError(fmt.Sprintf("foreach needs a function as its first argument, but got %s.", String(f)), env)
		return
	}

	var collections []*Data = make([]*Data, 0, Length(args)-1)
	var loopCount int64 = math.MaxInt64
	var col *Data
	for a := Cdr(args); NotNilP(a); a = Cdr(a) {
		col = Car(a)
		if !ListP(col) {
			err = ProcessError(fmt.Sprintf("foreach needs lists as its other arguments, but got %s.", String(col)), env)
			return
		}
		collections = append(collections, col)
		loopCount = intMin(loopCount, int64(Length(col)))
	}

	if loopCount == math.MaxInt64 {
		return
	}

	var a *Data
	for index := 1; index <= int(loopCount); index++ {
		mapArgs := make([]*Data, 0, len(collections))
		for key, mapArgCollection := range collections {
			a = Car(mapArgCollection)
			collections[key] = Cdr(mapArgCollection)
			mapArgs = append(mapArgs, a)
		}
		_, err = ApplyWithoutEval(f, ArrayToList(mapArgs), env)
		if err != nil {
			return
		}
	}

	return nil, nil
}

func AnyImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	if !FunctionOrPrimitiveP(f) {
		err = ProcessError(fmt.Sprintf("any needs a function as its first argument, but got %s.", String(f)), env)
		return
	}

	var collections []*Data = make([]*Data, 0, Length(args)-1)
	var loopCount int64 = math.MaxInt64
	var col *Data
	for a := Cdr(args); NotNilP(a); a = Cdr(a) {
		col = Car(a)
		if !ListP(col) {
			err = ProcessError(fmt.Sprintf("any needs lists as its other arguments, but got %s.", String(col)), env)
			return
		}
		collections = append(collections, col)
		loopCount = intMin(loopCount, int64(Length(col)))
	}

	if loopCount == math.MaxInt64 {
		return
	}

	var a *Data
	var b *Data
	for index := 0; index < int(loopCount); index++ {
		mapArgs := make([]*Data, 0, len(collections))
		for key, mapArgCollection := range collections {
			a = Car(mapArgCollection)
			collections[key] = Cdr(mapArgCollection)
			mapArgs = append(mapArgs, a)
		}
		b, err = ApplyWithoutEval(f, ArrayToList(mapArgs), env)
		if err != nil {
			return
		}

		if BooleanValue(b) {
			return LispTrue, nil
		}
	}

	return LispFalse, nil
}

func EveryImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	if !FunctionOrPrimitiveP(f) {
		err = ProcessError(fmt.Sprintf("every needs a function as its first argument, but got %s.", String(f)), env)
		return
	}

	var collections []*Data = make([]*Data, 0, Length(args)-1)
	var loopCount int64 = math.MaxInt64
	var col *Data
	for a := Cdr(args); NotNilP(a); a = Cdr(a) {
		col = Car(a)
		if !ListP(col) {
			err = ProcessError(fmt.Sprintf("every needs lists as its other arguments, but got %s.", String(col)), env)
			return
		}
		collections = append(collections, col)
		loopCount = intMin(loopCount, int64(Length(col)))
	}

	if loopCount == math.MaxInt64 {
		return
	}

	var a *Data
	var b *Data
	for index := 0; index < int(loopCount); index++ {
		mapArgs := make([]*Data, 0, len(collections))
		for key, mapArgCollection := range collections {
			a = Car(mapArgCollection)
			collections[key] = Cdr(mapArgCollection)
			mapArgs = append(mapArgs, a)
		}
		b, err = ApplyWithoutEval(f, ArrayToList(mapArgs), env)
		if err != nil {
			return
		}

		if !BooleanValue(b) {
			return LispFalse, nil
		}
	}

	return LispTrue, nil
}

func reduceFoldProc(initial *Data, col []*Data, f *Data, env *SymbolTableFrame, rightToLeft bool) (result *Data, err error) {
	result = initial
	for i := range col {
		if rightToLeft {
			i = len(col) - 1 - i
		}
		c := col[i]

		var x, y *Data
		if rightToLeft {
			x, y = c, result
		} else {
			x, y = result, c
		}

		result, err = ApplyWithoutEval(f, InternalMakeList(x, y), env)
		if err != nil {
			return
		}
	}

	return
}

func ReduceLeftImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	if !FunctionOrPrimitiveP(f) {
		err = ProcessError("reduce-left needs a function as its first argument", env)
		return
	}

	initial := Second(args)
	col := Third(args)

	if !ListP(col) {
		err = ProcessError("reduce-left needs a list as its third argument", env)
		return
	}

	list := ToArray(col)
	if len(list) == 0 {
		return initial, nil
	}

	result, err = reduceFoldProc(list[0], list[1:], f, env, false)

	return
}

func ReduceRightImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	if !FunctionOrPrimitiveP(f) {
		err = ProcessError("reduce-right needs a function as its first argument", env)
		return
	}

	initial := Second(args)
	col := Third(args)

	if !ListP(col) {
		err = ProcessError("reduce-right needs a list as its third argument", env)
		return
	}

	list := ToArray(col)
	if len(list) == 0 {
		return initial, nil
	}

	result, err = reduceFoldProc(list[len(list)-1], list[:len(list)-1], f, env, true)

	return
}

func FoldLeftImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	if !FunctionOrPrimitiveP(f) {
		err = ProcessError("fold-left needs a function as its first argument", env)
		return
	}

	initial := Second(args)
	col := Third(args)

	if !ListP(col) {
		err = ProcessError("fold-left needs a list as its third argument", env)
		return
	}

	list := ToArray(col)

	result, err = reduceFoldProc(initial, list, f, env, false)

	return
}

func FoldRightImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	if !FunctionOrPrimitiveP(f) {
		err = ProcessError("fold-right needs a function as its first argument", env)
		return
	}

	initial := Second(args)
	col := Third(args)

	if !ListP(col) {
		err = ProcessError("fold-right needs a list as its third argument", env)
		return
	}

	list := ToArray(col)

	result, err = reduceFoldProc(initial, list, f, env, true)

	return
}

func FilterImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	if !FunctionOrPrimitiveP(f) {
		err = ProcessError(fmt.Sprintf("filter needs a function as its first argument, but got %s.", String(f)), env)
		return
	}

	col := Second(args)
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

func RemoveImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	if !FunctionOrPrimitiveP(f) {
		err = ProcessError(fmt.Sprintf("remove needs a function as its first argument, but got %s.", String(f)), env)
		return
	}

	col := Second(args)
	if !ListP(col) {
		err = ProcessError(fmt.Sprintf("remove needs a list as its second argument, but got %s.", String(col)), env)
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
			err = ProcessError("remove needs a predicate function as its first argument.", env)
			return
		}

		if !BooleanValue(v) {
			d = append(d, Car(c))
		}
	}

	return ArrayToList(d), nil
}

func MemqImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	key := First(args)

	l := Second(args)

	for c := l; NotNilP(c); c = Cdr(c) {
		if IsEqual(key, Car(c)) {
			return c, nil
		}
	}

	return LispFalse, nil
}

func FindTailImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	if !FunctionOrPrimitiveP(f) {
		err = ProcessError("find-tail/memp needs a function as its first argument", env)
		return
	}

	l := Second(args)
	if !ListP(l) {
		err = ProcessError(fmt.Sprintf("find-tail needs a list as its second argument, but got %s.", String(l)), env)
		return
	}

	var found *Data
	for c := l; NotNilP(c); c = Cdr(c) {
		found, err = ApplyWithoutEval(f, InternalMakeList(Car(c)), env)

		if !BooleanP(found) {
			err = ProcessError("find-tail needs a predicate function as its first argument.", env)
			return
		}
		if BooleanValue(found) {
			return c, nil
		}
	}

	return LispFalse, nil
}

func FindImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	if !FunctionOrPrimitiveP(f) {
		err = ProcessError("find needs a function as its first argument", env)
		return
	}

	l := Second(args)
	if !ListP(l) {
		err = ProcessError(fmt.Sprintf("find needs a list as its second argument, but got %s.", String(l)), env)
		return
	}

	var found *Data
	for c := l; NotNilP(c); c = Cdr(c) {
		found, err = ApplyWithoutEval(f, InternalMakeList(Car(c)), env)
		if !BooleanP(found) {
			err = ProcessError("find needs a predicate function as its first argument.", env)
			return
		}
		if BooleanValue(found) {
			return Car(c), nil
		}
	}

	return LispFalse, nil
}
