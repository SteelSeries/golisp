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
	MakePrimitiveFunction("reduce", "3", ReduceImpl)
	MakePrimitiveFunction("filter", "2", FilterImpl)
	MakePrimitiveFunction("remove", "2", RemoveImpl)
	MakePrimitiveFunction("memq", "2", MemqImpl)
	MakePrimitiveFunction("memp", "2", FindTailImpl)
	MakePrimitiveFunction("find-tail", "2", FindTailImpl)
	MakePrimitiveFunction("find", "2", FindImpl)
	MakePrimitiveFunction("vectorize", "1", VectorizeImpl)
	MakePrimitiveFunction("consify", "1", ConsifyImpl)
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

func ReduceImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	if !FunctionOrPrimitiveP(f) {
		err = ProcessError("reduce needs a function as its first argument", env)
		return
	}

	initial := Second(args)
	col := Third(args)

	if !ListP(col) {
		err = ProcessError("map needs a list as its third argument", env)
		return
	}

	length := Length(col)

	if length == 0 {
		return initial, nil
	}

	if length == 1 {
		return Car(col), nil
	}

	if VectorizedListP(col) {
		vect := VectorizedListValue(col)
		result = vect[0]
		for _, v := range vect[1:] {
			result, err = ApplyWithoutEval(f, InternalMakeList(result, v), env)
			if err != nil {
				return
			}
		}
	} else {
		result = Car(col)
		for c := Cdr(col); NotNilP(c); c = Cdr(c) {
			result, err = ApplyWithoutEval(f, InternalMakeList(result, Car(c)), env)
			if err != nil {
				return
			}
		}
	}

	return
}

func filterElement(d []*Data, f *Data, e *Data, env *SymbolTableFrame) (result []*Data, err error) {
	v, err := ApplyWithoutEval(f, Cons(e, nil), env)
	if err != nil {
		return
	}
	if !BooleanP(v) {
		err = ProcessError("filter needs a predicate function as its first argument.", env)
		return
	}

	if BooleanValue(v) {
		d = append(d, e)
	}

	return d, nil
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

	if VectorizedListP(col) {
		for _, e := range VectorizedListValue(col) {
			d, err = filterElement(d, f, e, env)
			if err != nil {
				return
			}
		}
	} else {
		for c := col; NotNilP(c); c = Cdr(c) {
			d, err = filterElement(d, f, Car(c), env)
			if err != nil {
				return
			}
		}
	}

	if VectorizedListP(col) {
		return VectorizedListWithValue(d), nil
	} else {
		return ArrayToList(d), nil
	}
}

func removeElement(d []*Data, f *Data, e *Data, env *SymbolTableFrame) (result []*Data, err error) {
	v, err := ApplyWithoutEval(f, Cons(e, nil), env)
	if err != nil {
		return
	}
	if !BooleanP(v) {
		err = ProcessError("filter needs a predicate function as its first argument.", env)
		return
	}

	if !BooleanValue(v) {
		d = append(d, e)
	}

	return d, nil
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
	if VectorizedListP(col) {
		for _, e := range VectorizedListValue(col) {
			d, err = removeElement(d, f, e, env)
			if err != nil {
				return
			}
		}
	} else {
		for c := col; NotNilP(c); c = Cdr(c) {
			d, err = removeElement(d, f, Car(c), env)
			if err != nil {
				return
			}
		}
	}

	if VectorizedListP(col) {
		return VectorizedListWithValue(d), nil
	} else {
		return ArrayToList(d), nil
	}
}

func MemqImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	key := First(args)

	l := Second(args)

	if VectorizedListP(l) {
		vect := VectorizedListValue(l)
		for i, e := range vect {
			if IsEqual(key, e) {
				return VectorizedListWithValue(vect[i:]), nil
			}
		}
	} else {
		for c := l; NotNilP(c); c = Cdr(c) {
			if IsEqual(key, Car(c)) {
				return c, nil
			}
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
		err = ProcessError(fmt.Sprintf("find-tail/memp needs a list as its second argument, but got %s.", String(l)), env)
		return
	}

	var found *Data

	if VectorizedListP(l) {
		vect := VectorizedListValue(l)
		for i, e := range vect {
			found, err = ApplyWithoutEval(f, InternalMakeList(e), env)

			if !BooleanP(found) {
				err = ProcessError("find-tail/memp needs a predicate function as its first argument.", env)
				return
			}
			if BooleanValue(found) {
				return VectorizedListWithValue(vect[i:]), nil
			}
		}
	} else {
		for c := l; NotNilP(c); c = Cdr(c) {
			found, err = ApplyWithoutEval(f, InternalMakeList(Car(c)), env)

			if !BooleanP(found) {
				err = ProcessError("find-tail/memp needs a predicate function as its first argument.", env)
				return
			}
			if BooleanValue(found) {
				return c, nil
			}
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
	if VectorizedListP(l) {
		vect := VectorizedListValue(l)
		for _, e := range vect {
			found, err = ApplyWithoutEval(f, InternalMakeList(e), env)

			if !BooleanP(found) {
				err = ProcessError("find needs a predicate function as its first argument.", env)
				return
			}
			if BooleanValue(found) {
				return e, nil
			}
		}
	} else {
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
	}

	return LispFalse, nil
}

func VectorizeImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l := Car(args)
	if ListP(l) {
		result = Vectorize(l)
	} else {
		err = ProcessError(fmt.Sprintf("vectorize needs a list or vectorized list as its argument, but got %s.", String(l)), env)
	}
	return
}

func ConsifyImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l := Car(args)
	if ListP(l) {
		result = Consify(l)
	} else {
		err = ProcessError(fmt.Sprintf("consify needs a vectorized list or list as its argument, but got %s.", String(l)), env)
	}
	return
}
