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
	MakeTypedPrimitiveFunction("map", ">=2", MapImpl, []uint32{FunctionType | PrimitiveType | MacroType | CompiledFunctionType, ConsCellType, ConsCellType, ConsCellType, ConsCellType})
	MakeTypedPrimitiveFunction("for-each", ">=2", ForEachImpl, []uint32{FunctionType | PrimitiveType | MacroType | CompiledFunctionType, ConsCellType, ConsCellType, ConsCellType, ConsCellType})
	MakeTypedPrimitiveFunction("any", ">=2", AnyImpl, []uint32{FunctionType | PrimitiveType | MacroType | CompiledFunctionType, ConsCellType, ConsCellType, ConsCellType, ConsCellType})
	MakeTypedPrimitiveFunction("every", ">=2", EveryImpl, []uint32{FunctionType | PrimitiveType | MacroType | CompiledFunctionType, ConsCellType, ConsCellType, ConsCellType, ConsCellType})
	MakeTypedPrimitiveFunction("reduce", "3", ReduceLeftImpl, []uint32{FunctionType | PrimitiveType | MacroType | CompiledFunctionType, AnyType, ConsCellType})
	MakeTypedPrimitiveFunction("reduce-left", "3", ReduceLeftImpl, []uint32{FunctionType | PrimitiveType | MacroType | CompiledFunctionType, AnyType, ConsCellType})
	MakeTypedPrimitiveFunction("fold-left", "3", FoldLeftImpl, []uint32{FunctionType | PrimitiveType | MacroType | CompiledFunctionType, AnyType, ConsCellType})
	MakeTypedPrimitiveFunction("reduce-right", "3", ReduceRightImpl, []uint32{FunctionType | PrimitiveType | MacroType | CompiledFunctionType, AnyType, ConsCellType})
	MakeTypedPrimitiveFunction("fold-right", "3", FoldRightImpl, []uint32{FunctionType | PrimitiveType | MacroType | CompiledFunctionType, AnyType, ConsCellType})
	MakeTypedPrimitiveFunction("filter", "2", FilterImpl, []uint32{FunctionType | PrimitiveType | MacroType | CompiledFunctionType, ConsCellType})
	MakeTypedPrimitiveFunction("remove", "2", RemoveImpl, []uint32{FunctionType | PrimitiveType | MacroType | CompiledFunctionType, ConsCellType})
	MakeTypedPrimitiveFunction("memq", "2", MemqImpl, []uint32{AnyType, ConsCellType})
	MakeTypedPrimitiveFunction("memv", "2", MemvImpl, []uint32{AnyType, ConsCellType})
	MakeTypedPrimitiveFunction("member", "2", MemberImpl, []uint32{AnyType, ConsCellType})
	MakeTypedPrimitiveFunction("memp", "2", FindTailImpl, []uint32{FunctionType | PrimitiveType | MacroType | CompiledFunctionType, ConsCellType})
	MakeTypedPrimitiveFunction("find-tail", "2", FindTailImpl, []uint32{FunctionType | PrimitiveType | MacroType | CompiledFunctionType, ConsCellType})
	MakeTypedPrimitiveFunction("find", "2", FindImpl, []uint32{FunctionType | PrimitiveType | MacroType | CompiledFunctionType, ConsCellType})
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
	var collections []*Data = make([]*Data, 0, Length(args)-1)
	var loopCount int64 = math.MaxInt64
	var col *Data
	for a := Cdr(args); NotNilP(a); a = Cdr(a) {
		col = Car(a)
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
	for index := 0; index < int(loopCount); index++ {
		mapArgs := make([]*Data, 0, len(collections))
		for key, mapArgCollection := range collections {
			mapArgs = append(mapArgs, Car(mapArgCollection))
			collections[key] = Cdr(mapArgCollection)
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
	var collections []*Data = make([]*Data, 0, Length(args)-1)
	var loopCount int64 = math.MaxInt64
	var col *Data
	for a := Cdr(args); NotNilP(a); a = Cdr(a) {
		col = Car(a)
		collections = append(collections, col)
		loopCount = intMin(loopCount, int64(Length(col)))
	}

	if loopCount == math.MaxInt64 {
		return
	}

	for index := 0; index < int(loopCount); index++ {
		mapArgs := make([]*Data, 0, len(collections))
		for key, mapArgCollection := range collections {
			mapArgs = append(mapArgs, Car(mapArgCollection))
			collections[key] = Cdr(mapArgCollection)
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
	var collections []*Data = make([]*Data, 0, Length(args)-1)
	var loopCount int64 = math.MaxInt64
	var col *Data
	for a := Cdr(args); NotNilP(a); a = Cdr(a) {
		col = Car(a)
		collections = append(collections, col)
		loopCount = intMin(loopCount, int64(Length(col)))
	}

	if loopCount == math.MaxInt64 {
		return
	}

	var b *Data
	for index := 0; index < int(loopCount); index++ {
		mapArgs := make([]*Data, 0, len(collections))
		for key, mapArgCollection := range collections {
			mapArgs = append(mapArgs, Car(mapArgCollection))
			collections[key] = Cdr(mapArgCollection)
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
	var collections []*Data = make([]*Data, 0, Length(args)-1)
	var loopCount int64 = math.MaxInt64
	var col *Data
	for a := Cdr(args); NotNilP(a); a = Cdr(a) {
		col = Car(a)
		collections = append(collections, col)
		loopCount = intMin(loopCount, int64(Length(col)))
	}

	if loopCount == math.MaxInt64 {
		return
	}

	var b *Data
	for index := 0; index < int(loopCount); index++ {
		mapArgs := make([]*Data, 0, len(collections))
		for key, mapArgCollection := range collections {
			mapArgs = append(mapArgs, Car(mapArgCollection))
			collections[key] = Cdr(mapArgCollection)
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

func ReduceLeftImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	initial := Second(args)
	col := Third(args)

	if !ListP(col) {
		err = ProcessError(fmt.Sprintf("reduce-left requires a proper list as its third argument but received %s.", String(col)), env)
		return
	}

	l := Length(col)
	if l == 0 {
		return initial, nil
	}
	if l == 1 {
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
	col := Second(args)
	if !ListP(col) {
		err = ProcessError(fmt.Sprintf("filter needs a proper list as its second argument, but got %s.", String(col)), env)
		return
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
	col := Second(args)
	if !ListP(col) {
		err = ProcessError(fmt.Sprintf("remove needs a proper list as its second argument, but got %s.", String(col)), env)
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
	if !ListP(l) {
		err = ProcessError(fmt.Sprintf("memq needs a proper list as its second argument, but got %s.", String(l)), env)
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
		err = ProcessError(fmt.Sprintf("memv needs a proper list as its second argument, but got %s.", String(l)), env)
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
		err = ProcessError(fmt.Sprintf("member needs a proper list as its second argument, but got %s.", String(l)), env)
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
	l := Second(args)
	if !ListP(l) {
		err = ProcessError(fmt.Sprintf("memp/find-tail needs a proper list as its second argument, but got %s.", String(l)), env)
		return
	}

	var found *Data
	for c := l; NotNilP(c); c = Cdr(c) {
		found, err = ApplyWithoutEval(f, InternalMakeList(Car(c)), env)
		if err != nil {
			return
		}
		if !BooleanP(found) {
			err = ProcessError(fmt.Sprintf("memp/find-tail needs a predicate function as its first argument, but it returned %s.", String(found)), env)
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
	col := Second(args)
	if !ListP(col) {
		err = ProcessError(fmt.Sprintf("find needs a list as its second argument, but got %s.", String(col)), env)
		return
	}

	var found *Data
	for c := col; NotNilP(c); c = Cdr(c) {
		found, err = ApplyWithoutEval(f, InternalMakeList(Car(c)), env)
		if err != nil {
			return
		}
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
