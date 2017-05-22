// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the vector primitive functions.

package golisp

import (
	"fmt"
	"math"
)

func RegisterVectorPrimitives() {
	MakeTypedPrimitiveFunction("make-vector", "1|2", makeVectorImpl, []uint32{IntegerType, AnyType})
	MakeTypedPrimitiveFunction("vector", ">=1", vectorImpl, []uint32{AnyType})
	MakeTypedPrimitiveFunction("vector-copy", "1", vectorCopyImpl, []uint32{VectorType})
	MakeTypedPrimitiveFunction("list->vector", "1", listToVectorImpl, []uint32{ConsCellType})
	MakeTypedPrimitiveFunction("vector->list", "1", vectorToListImpl, []uint32{VectorType})
	MakeTypedPrimitiveFunction("make-initialized-vector", "2", makeInitializedVectorImpl, []uint32{IntegerType, FunctionType | CompiledFunctionType | PrimitiveType})
	MakeTypedPrimitiveFunction("vector-grow", "2", vectorGrowImpl, []uint32{VectorType, IntegerType})
	MakeTypedPrimitiveFunction("vector-map", ">=2", vectorMapImpl, []uint32{FunctionType | CompiledFunctionType | PrimitiveType, VectorType})
	MakeTypedPrimitiveFunction("vector-for-each", ">=2", vectorForEachImpl, []uint32{FunctionType | CompiledFunctionType | PrimitiveType, VectorType})
	MakeTypedPrimitiveFunction("vector-reduce", "3", vectorReduceImpl, []uint32{FunctionType | CompiledFunctionType | PrimitiveType, AnyType, VectorType})
	MakeTypedPrimitiveFunction("vector-filter", "2", vectorFilterImpl, []uint32{FunctionType | CompiledFunctionType | PrimitiveType, VectorType})
	MakeTypedPrimitiveFunction("vector-remove", "2", vectorRemoveImpl, []uint32{FunctionType | CompiledFunctionType | PrimitiveType, VectorType})
	MakeTypedPrimitiveFunction("vector?", "1", vectorPImpl, []uint32{AnyType})
	MakeTypedPrimitiveFunction("vector-length", "1", vectorLengthImpl, []uint32{VectorType})
	MakeTypedPrimitiveFunction("vector-ref", "2", vectorRefImpl, []uint32{VectorType, IntegerType})
	MakeTypedPrimitiveFunction("vector-set!", "3", vectorSetImpl, []uint32{VectorType, IntegerType, AnyType})
	MakeTypedPrimitiveFunction("vector-first", "1", vectorFirstImpl, []uint32{VectorType})
	MakeTypedPrimitiveFunction("vector-second", "1", vectorSecondImpl, []uint32{VectorType})
	MakeTypedPrimitiveFunction("vector-third", "1", vectorThirdImpl, []uint32{VectorType})
	MakeTypedPrimitiveFunction("vector-fourth", "1", vectorFourthImpl, []uint32{VectorType})
	MakeTypedPrimitiveFunction("vector-fifth", "1", vectorFifthImpl, []uint32{VectorType})
	MakeTypedPrimitiveFunction("vector-sixth", "1", vectorSixthImpl, []uint32{VectorType})
	MakeTypedPrimitiveFunction("vector-seventh", "1", vectorSeventhImpl, []uint32{VectorType})
	MakeTypedPrimitiveFunction("vector-eighth", "1", vectorEighthImpl, []uint32{VectorType})
	MakeTypedPrimitiveFunction("vector-ninth", "1", vectorNinthImpl, []uint32{VectorType})
	MakeTypedPrimitiveFunction("vector-tenth", "1", vectorTenthImpl, []uint32{VectorType})
	MakeTypedPrimitiveFunction("vector-last", "1", vectorLastImpl, []uint32{VectorType})
	MakeTypedPrimitiveFunction("vector-binary-search", "4", vectorBinarySearchImpl, []uint32{VectorType, FunctionType | CompiledFunctionType | PrimitiveType, FunctionType | CompiledFunctionType | PrimitiveType, AnyType})
	MakeTypedPrimitiveFunction("vector-find", "2", vectorFindImpl, []uint32{FunctionType | CompiledFunctionType | PrimitiveType, VectorType})
	MakeTypedPrimitiveFunction("subvector", "3", subVectorImpl, []uint32{VectorType, IntegerType, IntegerType})
	MakeTypedPrimitiveFunction("vector-head", "2", vectorHeadImpl, []uint32{VectorType, IntegerType})
	MakeTypedPrimitiveFunction("vector-tail", "2", vectorTailImpl, []uint32{VectorType, IntegerType})
	MakeTypedPrimitiveFunction("vector-fill!", "2", vectorFillImpl, []uint32{VectorType, AnyType})
	MakeTypedPrimitiveFunction("subvector-fill!", "4", subVectorFillImpl, []uint32{VectorType, IntegerType, IntegerType, AnyType})
	MakeTypedPrimitiveFunction("subvector-move-left!", "5", subVectorMoveLeftImpl, []uint32{VectorType, IntegerType, IntegerType, VectorType, IntegerType})
	MakeTypedPrimitiveFunction("subvector-move-right!", "5", subVectorMoveRightImpl, []uint32{VectorType, IntegerType, IntegerType, VectorType, IntegerType})
	MakeTypedPrimitiveFunction("vector-sort", "2", vectorSortImpl, []uint32{VectorType, FunctionType | CompiledFunctionType | PrimitiveType})
	MakeTypedPrimitiveFunction("vector-sort!", "2", vectorSortInPlaceImpl, []uint32{VectorType, FunctionType | CompiledFunctionType | PrimitiveType})
	MakeTypedPrimitiveFunction("vector-reverse", "1", vectorReverseImpl, []uint32{VectorType})
}

func makeVectorImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	size := IntegerValue(First(args))

	var value *Data = nil
	if Length(args) == 2 {
		value = Second(args)
	}

	vals := make([]*Data, size)
	for i := int64(0); i < size; i++ {
		vals[i] = value
	}

	return VectorWithValue(vals), nil
}

func vectorImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return VectorWithValue(ToArray(args)), nil
}

func vectorCopyImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	v := VectorValue(First(args))
	newV := make([]*Data, 0, len(v))
	for _, e := range v {
		newV = append(newV, e)
	}
	return VectorWithValue(newV), nil
}

func listToVectorImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return VectorWithValue(ToArray(First(args))), nil
}

func vectorToListImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return ArrayToList(VectorValue(First(args))), nil
}

func makeInitializedVectorImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	size := IntegerValue(First(args))
	f := Second(args)
	vals := make([]*Data, size)
	for i := int64(0); i < size; i++ {
		vals[i], err = Apply(f, InternalMakeList(IntegerWithValue(i)), env)
		if err != nil {
			return
		}
	}

	return VectorWithValue(vals), nil
}

func vectorGrowImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	originalValues := VectorValue(First(args))
	size := IntegerValue(Second(args))

	if int(size) <= len(originalValues) {
		err = ProcessError(fmt.Sprintf("vector-grow needs a new size that is larger than the size of its vector argument (%d), but got %s.", len(originalValues), size), env)
		return
	}

	vals := make([]*Data, size)
	for i, val := range originalValues {
		vals[i] = val
	}

	return VectorWithValue(vals), nil
}

func vectorMapImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	var collections [][]*Data = make([][]*Data, 0, Length(args)-1)
	var loopCount int64 = math.MaxInt64
	var col *Data
	for a := Cdr(args); NotNilP(a); a = Cdr(a) {
		col = Car(a)
		if NilP(col) || col == nil {
			return
		}
		collections = append(collections, VectorValue(col))
		loopCount = intMin(loopCount, int64(Length(col)))
	}

	if loopCount == math.MaxInt64 {
		return
	}

	var vals []*Data = make([]*Data, loopCount)
	var v *Data
	var a *Data
	for index := 0; index < int(loopCount); index++ {
		mapArgs := make([]*Data, 0, len(collections))
		for _, mapArgCollection := range collections {
			a = mapArgCollection[index]
			mapArgs = append(mapArgs, a)
		}
		v, err = ApplyWithoutEval(f, ArrayToList(mapArgs), env)
		if err != nil {
			return
		}
		vals[index] = v
	}

	return VectorWithValue(vals), nil
}

func vectorForEachImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	var collections [][]*Data = make([][]*Data, 0, Length(args)-1)
	var loopCount int64 = math.MaxInt64
	var col *Data
	for a := Cdr(args); NotNilP(a); a = Cdr(a) {
		col = Car(a)
		if NilP(col) || col == nil {
			return
		}
		collections = append(collections, VectorValue(col))
		loopCount = intMin(loopCount, int64(Length(col)))
	}

	if loopCount == math.MaxInt64 {
		return
	}

	var a *Data
	for index := 0; index < int(loopCount); index++ {
		mapArgs := make([]*Data, 0, len(collections))
		for _, mapArgCollection := range collections {
			a = mapArgCollection[index]
			mapArgs = append(mapArgs, a)
		}
		_, err = ApplyWithoutEval(f, ArrayToList(mapArgs), env)
		if err != nil {
			return
		}
	}

	return
}

func vectorReduceImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	initial := Second(args)
	v := VectorValue(Third(args))

	if len(v) == 0 {
		return initial, nil
	}

	if len(v) == 1 {
		return v[0], nil
	}

	result = v[0]
	for _, val := range v[1:] {
		result, err = ApplyWithoutEval(f, InternalMakeList(result, val), env)
		if err != nil {
			return
		}
	}

	return
}

func vectorFilterImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	vals := VectorValue(Second(args))

	var v *Data
	var d []*Data = make([]*Data, 0, len(vals))
	for _, val := range vals {
		v, err = ApplyWithoutEval(f, InternalMakeList(val), env)
		if err != nil {
			return
		}
		if !BooleanP(v) {
			err = ProcessError("vector-filter needs a predicate function as its first argument.", env)
			return
		}

		if BooleanValue(v) {
			d = append(d, val)
		}
	}

	return VectorWithValue(d), nil
}

func vectorRemoveImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	vals := VectorValue(Second(args))

	var v *Data
	var d []*Data = make([]*Data, 0, len(vals))
	for _, val := range vals {
		v, err = ApplyWithoutEval(f, InternalMakeList(val), env)
		if err != nil {
			return
		}
		if !BooleanP(v) {
			err = ProcessError("vector-remove needs a predicate function as its first argument.", env)
			return
		}

		if !BooleanValue(v) {
			d = append(d, val)
		}
	}

	return VectorWithValue(d), nil
}

func vectorPImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(VectorP(First(args))), nil
}

func vectorLengthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return IntegerWithValue(int64(Length(First(args)))), nil
}

func vectorRefImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	values := VectorValue(First(args))
	index := int(IntegerValue(Second(args)))

	if index >= len(values) {
		err = ProcessError(fmt.Sprintf("vector-ref needs an index less than the vector length, but got %d.", index), env)
		return
	}

	return values[index], nil
}

func vectorSetImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	values := VectorValue(First(args))
	kval := int(IntegerValue(Second(args)))
	newValue := Third(args)

	values[kval] = newValue
	return StringWithValue("OK"), nil
}

func vectorFirstImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	values := VectorValue(First(args))
	if len(values) > 0 {
		return values[0], nil
	} else {
		return nil, ProcessError(fmt.Sprintf("vector-first needs a vector with length of at least 1, but got %d.", len(values)), env)
	}
}

func vectorSecondImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	values := VectorValue(First(args))
	if len(values) > 1 {
		return values[1], nil
	} else {
		return nil, ProcessError(fmt.Sprintf("vector-second needs a vector with length of at least 2, but got %d.", len(values)), env)
	}
}

func vectorThirdImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	values := VectorValue(First(args))
	if len(values) > 2 {
		return values[2], nil
	} else {
		return nil, ProcessError(fmt.Sprintf("vector-third needs a vector with length of at least 3, but got %d.", len(values)), env)
	}
}

func vectorFourthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	values := VectorValue(First(args))
	if len(values) > 3 {
		return values[3], nil
	} else {
		return nil, ProcessError(fmt.Sprintf("vector-fourth needs a vector with length of at least 4, but got %d.", len(values)), env)
	}
}

func vectorFifthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	values := VectorValue(First(args))
	if len(values) > 4 {
		return values[4], nil
	} else {
		return nil, ProcessError(fmt.Sprintf("vector-fifth needs a vector with length of at least 5, but got %d.", len(values)), env)
	}
}

func vectorSixthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	values := VectorValue(First(args))
	if len(values) > 5 {
		return values[5], nil
	} else {
		return nil, ProcessError(fmt.Sprintf("vector-sixth needs a vector with length of at least 6, but got %d.", len(values)), env)
	}
}

func vectorSeventhImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	values := VectorValue(First(args))
	if len(values) > 6 {
		return values[6], nil
	} else {
		return nil, ProcessError(fmt.Sprintf("vector-seventh needs a vector with length of at least 7, but got %d.", len(values)), env)
	}
}

func vectorEighthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	values := VectorValue(First(args))
	if len(values) > 7 {
		return values[7], nil
	} else {
		return nil, ProcessError(fmt.Sprintf("vector-eigth needs a vector with length of at least 8, but got %d.", len(values)), env)
	}
}

func vectorNinthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	values := VectorValue(First(args))
	if len(values) > 8 {
		return values[8], nil
	} else {
		return nil, ProcessError(fmt.Sprintf("vector-ninth needs a vector with length of at least 9, but got %d.", len(values)), env)
	}
}

func vectorTenthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	values := VectorValue(First(args))
	if len(values) > 9 {
		return values[9], nil
	} else {
		return nil, ProcessError(fmt.Sprintf("vector-tenth needs a vector with length of at least 10, but got %d.", len(values)), env)
	}
}

func vectorLastImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	values := VectorValue(First(args))
	if len(values) > 0 {
		return values[len(values)-1], nil
	} else {
		return nil, ProcessError(fmt.Sprintf("vector-last needs a vector that isn't empty, but got %d.", len(values)), env)
	}
}

func vectorBinarySearchImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return
}

func vectorFindImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	var found *Data
	for _, val := range VectorValue(Second(args)) {
		found, err = ApplyWithoutEval(f, InternalMakeList(val), env)
		if !BooleanP(found) {
			return nil, ProcessError("vector-find needs a predicate function as its first argument.", env)
		}
		if BooleanValue(found) {
			return val, nil
		}
	}

	return LispFalse, nil
}

func subVectorImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	values := VectorValue(First(args))
	startIndex := int(IntegerValue(Second(args)))
	if startIndex < 0 || startIndex >= len(values) {
		return nil, ProcessError(fmt.Sprintf("subvector starting index is out of bounds (0-%d), got %d.", len(values)-1, startIndex), env)
	}

	endIndex := int(IntegerValue(Third(args)))
	if endIndex < startIndex || endIndex > len(values) {
		return nil, ProcessError(fmt.Sprintf("subvector ending index is out of bounds (%d-%d), got %d.", startIndex, len(values), startIndex), env)
	}

	return VectorWithValue(values[startIndex:endIndex]), nil
}

func vectorHeadImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	values := VectorValue(First(args))
	endIndex := int(IntegerValue(Second(args)))

	if endIndex < 0 || endIndex > len(values) {
		return nil, ProcessError(fmt.Sprintf("vector-head ending index is out of bounds (0-%d), got %d.", len(values), endIndex), env)
	}

	return VectorWithValue(values[:endIndex]), nil
}

func vectorTailImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	values := VectorValue(First(args))
	startIndex := int(IntegerValue(Second(args)))

	if startIndex < 0 || startIndex > len(values) {
		return nil, ProcessError(fmt.Sprintf("vector-tail starting index is out of bounds (0-%d), got %d.", len(values), startIndex), env)
	}

	return VectorWithValue(values[startIndex:]), nil
}

func vectorFillImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	values := VectorValue(First(args))
	newValue := Second(args)

	for i, _ := range values {
		values[i] = newValue
	}

	return
}

func subVectorFillImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	values := VectorValue(First(args))
	startIndex := int(IntegerValue(Second(args)))

	if startIndex < 0 || startIndex >= len(values) {
		return nil, ProcessError(fmt.Sprintf("subvector-fill! starting index is out of bounds (0-%d), got %d.", len(values)-1, startIndex), env)
	}

	endIndex := int(IntegerValue(Third(args)))
	if endIndex < startIndex || endIndex > len(values) {
		return nil, ProcessError(fmt.Sprintf("subvector-fill! ending index is out of bounds (%d-%d), got %d.", startIndex, len(values), startIndex), env)
	}

	newValue := Fourth(args)

	for i := startIndex; i < endIndex; i = i + 1 {
		values[i] = newValue
	}

	return
}

func subVectorMoveLeftImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	values := VectorValue(First(args))
	startIndex := int(IntegerValue(Second(args)))

	if startIndex < 0 || startIndex >= len(values) {
		return nil, ProcessError(fmt.Sprintf("subvector-move-left! starting index is out of bounds (0-%d), got %d.", len(values)-1, startIndex), env)
	}

	endIndex := int(IntegerValue(Third(args)))
	if endIndex < startIndex || endIndex > len(values) {
		return nil, ProcessError(fmt.Sprintf("subvector-move-left! ending index is out of bounds (%d-%d), got %d.", startIndex, len(values), startIndex), env)
	}

	values2 := VectorValue(Fourth(args))

	startIndex2 := int(IntegerValue(Fifth(args)))

	if startIndex < 0 || startIndex >= len(values2) {
		return nil, ProcessError(fmt.Sprintf("subvector-move-left! starting index is out of bounds (0-%d), got %d.", len(values)-1, startIndex), env)
	}

	sourceLength := endIndex - startIndex
	tailSize2 := len(values2) - startIndex2
	if sourceLength > tailSize2 {
		return nil, ProcessError(fmt.Sprintf("subvector-move-left! source subvector is longer than the available space in the destination (0-%d), got %d.", tailSize2, sourceLength), env)
	}

	for i, i2 := startIndex, startIndex2; i < endIndex; i, i2 = i+1, i2+1 {
		values2[i2] = values[i]
	}

	return Fourth(args), nil
	return
}

func subVectorMoveRightImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	values := VectorValue(First(args))
	startIndex := int(IntegerValue(Second(args)))

	if startIndex < 0 || startIndex >= len(values) {
		return nil, ProcessError(fmt.Sprintf("subvector-move-right! starting index is out of bounds (0-%d), got %d.", len(values)-1, startIndex), env)
	}

	endIndex := int(IntegerValue(Third(args)))
	if endIndex < startIndex || endIndex > len(values) {
		return nil, ProcessError(fmt.Sprintf("subvector-move-right! ending index is out of bounds (%d-%d), got %d.", startIndex, len(values), startIndex), env)
	}

	values2 := VectorValue(Fourth(args))

	startIndex2 := int(IntegerValue(Fifth(args)))

	if startIndex < 0 || startIndex >= len(values2) {
		return nil, ProcessError(fmt.Sprintf("subvector-move-right! starting index is out of bounds (0-%d), got %d.", len(values)-1, startIndex), env)
	}

	sourceLength := endIndex - startIndex
	tailSize2 := len(values2) - startIndex2
	if sourceLength > tailSize2 {
		return nil, ProcessError(fmt.Sprintf("subvector-move-right! source subvector is longer than the available space in the destination (0-%d), got %d.", tailSize2, sourceLength), env)
	}

	for i, i2 := endIndex-1, startIndex2+sourceLength-1; i >= startIndex; i, i2 = i-1, i2-1 {
		values2[i2] = values[i]
	}

	return Fourth(args), nil
}

func vectorSortImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	values := VectorValue(First(args))

	proc := Second(args)
	sorted, err := MergeSort(values, proc, env)
	if err != nil {
		return
	}

	return VectorWithValue(sorted), nil
}

func vectorSortInPlaceImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	values := VectorValue(First(args))
	proc := Second(args)
	sorted, err := MergeSort(values, proc, env)
	if err != nil {
		return
	}

	for i, val := range sorted {
		values[i] = val
	}

	return First(args), nil
}

func vectorReverseImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	result = VectorReverse(First(args))
	return
}
