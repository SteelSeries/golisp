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
	MakePrimitiveFunction("reduce", "3", ReduceLeftImpl)
	MakePrimitiveFunction("reduce-left", "3", ReduceLeftImpl)
	MakePrimitiveFunction("fold-left", "3", FoldLeftImpl)
	MakePrimitiveFunction("reduce-right", "3", ReduceRightImpl)
	MakePrimitiveFunction("fold-right", "3", FoldRightImpl)
	MakePrimitiveFunction("filter", "2", FilterImpl)
	MakePrimitiveFunction("remove", "2", RemoveImpl)
	MakePrimitiveFunction("memq", "2", MemqImpl)
	MakePrimitiveFunction("memv", "2", MemvImpl)
	MakePrimitiveFunction("member", "2", MemberImpl)
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

	if VectorP(Second(args)) {
		return VectorMapImpl(args, env)
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
	for index := 0; index < int(loopCount); index++ {
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

func ForEachImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	if !FunctionOrPrimitiveP(f) {
		err = ProcessError(fmt.Sprintf("for-each needs a function as its first argument, but got %s.", String(f)), env)
		return
	}

	if VectorP(Second(args)) {
		return VectorForEachImpl(args, env)
	}

	var collections []*Data = make([]*Data, 0, Length(args)-1)
	var loopCount int64 = math.MaxInt64
	var col *Data
	for a := Cdr(args); NotNilP(a); a = Cdr(a) {
		col = Car(a)
		if !ListP(col) {
			err = ProcessError(fmt.Sprintf("for-each needs lists as its other arguments, but got %s.", String(col)), env)
			return
		}
		collections = append(collections, col)
		loopCount = intMin(loopCount, int64(Length(col)))
	}

	if loopCount == math.MaxInt64 {
		return
	}

	var a *Data
	for index := 0; index < int(loopCount); index++ {
		mapArgs := make([]*Data, 0, len(collections))
		for _, mapArgCollection := range collections {
			a = Nth(mapArgCollection, index)
			mapArgs = append(mapArgs, a)
		}
		_, err = ApplyWithoutEval(f, ArrayToList(mapArgs), env)
		if err != nil {
			return
		}
	}

	return nil, nil
}

func ReduceLeftImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	if !FunctionOrPrimitiveP(f) {
		err = ProcessError(fmt.Sprintf("reduce-left requires a function as its first argument but received %s.", String(f)), env)
		return
	}

	if VectorP(Third(args)) {
		return VectorReduceImpl(args, env)
	}

	initial := Second(args)
	col := Third(args)

	if !ListP(col) {
		err = ProcessError(fmt.Sprintf("reduce-left requires a proper list as its third argument but received %s.", String(col)), env)
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

func ReduceRightImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	if !FunctionOrPrimitiveP(f) {
		err = ProcessError(fmt.Sprintf("reduce-right requires a function as its first argument but received %s.", String(f)), env)
		return
	}

	if VectorP(Third(args)) {
		return VectorReduceImpl(args, env)
	}

	initial := Second(args)
	col := Third(args)

	if !ListP(col) {
		err = ProcessError(fmt.Sprintf("reduce-right requires a proper list as its third argument but received %s.", String(col)), env)
		return
	}

	if Length(col) == 0 {
		return initial, nil
	}

	if Length(col) == 1 {
		return Car(col), nil
	}

	ary := ToArray(col)

	result = ary[len(ary)-1]
	for i := len(ary) - 2; i >= 0; i-- {
		result, err = ApplyWithoutEval(f, InternalMakeList(ary[i], result), env)
		if err != nil {
			return
		}
	}

	return
}

func FoldLeftImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	if !FunctionOrPrimitiveP(f) {
		err = ProcessError(fmt.Sprintf("fold-left requires a function as its first argument but received %s.", String(f)), env)
		return
	}

	initial := Second(args)
	col := Third(args)

	if !ListP(col) {
		err = ProcessError(fmt.Sprintf("fold-left requires a proper list as its third argument but received %s.", String(col)), env)
		return
	}

	result = initial
	for c := col; NotNilP(c); c = Cdr(c) {
		result, err = ApplyWithoutEval(f, InternalMakeList(result, Car(c)), env)
		if err != nil {
			return
		}
	}

	return
}

func FoldRightImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	if !FunctionOrPrimitiveP(f) {
		err = ProcessError(fmt.Sprintf("fold-right requires a function as its first argument but received %s.", String(f)), env)
		return
	}

	initial := Second(args)
	col := Third(args)

	if !ListP(col) {
		err = ProcessError(fmt.Sprintf("fold-right requires a proper list as its third argument but received %s.", String(col)), env)
		return
	}

	result = initial
	for c := col; NotNilP(c); c = Cdr(c) {
		result, err = ApplyWithoutEval(f, InternalMakeList(result, Car(c)), env)
		if err != nil {
			return
		}
	}

	ary := ToArray(col)

	result = initial
	for i := len(ary) - 1; i >= 0; i-- {
		result, err = ApplyWithoutEval(f, InternalMakeList(ary[i], result), env)
		if err != nil {
			return
		}
	}
	return
}

func FilterImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	if !FunctionOrPrimitiveP(f) {
		err = ProcessError(fmt.Sprintf("filter needs a function as its first argument, but got %s.", String(f)), env)
		return
	}

	col := Second(args)
	if !ListP(col) && !VectorP(col) {
		err = ProcessError(fmt.Sprintf("filter needs a list or vector as its second argument, but got %s.", String(col)), env)
		return
	}

	if VectorP(col) {
		return VectorFilterImpl(args, env)
	}

	var d []*Data = make([]*Data, 0, Length(col))
	var v *Data
	for c := col; NotNilP(c); c = Cdr(c) {
		v, err = ApplyWithoutEval(f, InternalMakeList(Car(c)), env)
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
	if !ListP(col) && !VectorP(col) {
		err = ProcessError(fmt.Sprintf("remove needs a list as its second argument, but got %s.", String(col)), env)
		return
	}

	if VectorP(col) {
		return VectorRemoveImpl(args, env)
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
	if !ListP(l) {
		err = ProcessError(fmt.Sprintf("memq needs a list as its second argument, but got %s.", String(l)), env)
		return
	}
	for c := l; NotNilP(c); c = Cdr(c) {
		if IsEq(key, Car(c)) {
			return c, nil
		}
	}

	return LispFalse, nil
}

func MemvImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	key := First(args)

	l := Second(args)
	if !ListP(l) {
		err = ProcessError(fmt.Sprintf("memv needs a list as its second argument, but got %s.", String(l)), env)
		return
	}
	for c := l; NotNilP(c); c = Cdr(c) {
		if IsEqv(key, Car(c)) {
			return c, nil
		}
	}

	return LispFalse, nil
}

func MemberImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	key := First(args)

	l := Second(args)
	if !ListP(l) {
		err = ProcessError(fmt.Sprintf("member needs a list as its second argument, but got %s.", String(l)), env)
		return
	}
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

	col := Second(args)
	if !ListP(col) && !VectorP(col) {
		err = ProcessError(fmt.Sprintf("find needs a list or vector as its second argument, but got %s.", String(col)), env)
		return
	}

	if VectorP(col) {
		return VectorFindImpl(args, env)
	}

	var found *Data
	for c := col; NotNilP(c); c = Cdr(c) {
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
