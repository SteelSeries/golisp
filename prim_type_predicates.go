// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the type predicate primitive functions.

package golisp

import (
	"strings"
)

func RegisterTypePredicatePrimitives() {
	MakePrimitiveFunction("type-of", "1", TypeofImpl)
	MakePrimitiveFunction("atom?", "1", IsAtomImpl)
	MakePrimitiveFunction("list?", "1", IsListImpl)
	MakePrimitiveFunction("pair?", "1", IsPairImpl)
	MakePrimitiveFunction("dotted-pair?", "1", IsDottedPairImpl)
	MakePrimitiveFunction("circular-list?", "1", IsCircularListImpl)
	MakePrimitiveFunction("dotted-list?", "1", IsDottedListImpl)
	MakePrimitiveFunction("proper-list?", "1", IsProperListImpl)
	MakePrimitiveFunction("nil?", "1", IsNilImpl)
	MakePrimitiveFunction("null?", "1", IsNilImpl)
	MakePrimitiveFunction("notnil?", "1", IsNotNilImpl)
	MakePrimitiveFunction("notnull?", "1", IsNotNilImpl)
	MakePrimitiveFunction("symbol?", "1", IsSymbolImpl)
	MakePrimitiveFunction("naked?", "1", IsNakedImpl)
	MakePrimitiveFunction("string?", "1", IsStringImpl)
	MakePrimitiveFunction("boolean?", "1", IsBooleanImpl)
	MakePrimitiveFunction("integer?", "1", IsIntegerImpl)
	MakePrimitiveFunction("number?", "1", IsNumberImpl)
	MakePrimitiveFunction("float?", "1", IsFloatImpl)
	MakePrimitiveFunction("function?", "1", IsFunctionImpl)
	MakePrimitiveFunction("primitive?", "1", IsPrimitiveImpl)
	MakePrimitiveFunction("special-form?", "1", IsSpecialFormImpl)
	MakePrimitiveFunction("macro?", "1", IsMacroImpl)
	MakePrimitiveFunction("frame?", "1", IsFrameImpl)
	MakePrimitiveFunction("bytearray?", "1", IsByteArrayImpl)
	MakePrimitiveFunction("port?", "1", IsPortImpl)
	MakePrimitiveFunction("compiled-function?", "1", isCompiledFunctionImpl)
}

func TypeofImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	val := Car(args)
	result = Intern(strings.ToLower(TypeName(TypeOf(val))))
	return
}

func IsAtomImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	val := Car(args)
	return BooleanWithValue(NumberP(val) || StringP(val) || BooleanP(val) || CharacterP(val)), nil
}

func IsProperListImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	result = BooleanWithValue(ProperListP(First(args)))
	return
}

func IsListImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(ListP(Car(args))), nil
}

func IsPairImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(PairP(Car(args))), nil
}

func IsDottedPairImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(DottedPairP(Car(args))), nil
}

func IsCircularListImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(ListWithLoopP(Car(args))), nil
}

func IsDottedListImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(DottedListP(Car(args))), nil
}

func IsNilImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(NilP(Car(args))), nil
}

func IsNotNilImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(NotNilP(Car(args))), nil
}

func IsSymbolImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(SymbolP(Car(args))), nil
}

func IsNakedImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(NakedP(Car(args))), nil
}

func IsStringImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(StringP(Car(args))), nil
}

func IsBooleanImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(BooleanP(Car(args))), nil
}

func IsIntegerImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(IntegerP(Car(args))), nil
}

func IsNumberImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(NumberP(Car(args))), nil
}

func IsFloatImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(FloatP(Car(args))), nil
}

func IsFunctionImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(FunctionOrPrimitiveP(Car(args))), nil
}

func IsPrimitiveImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(PrimitiveP(Car(args))), nil
}

func IsSpecialFormImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
func isCompiledFunctionImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(CompiledFunctionP(Car(args))), nil
}

	return BooleanWithValue(PrimitiveP(Car(args)) && PrimitiveValue(Car(args)).Special), nil
}

func IsMacroImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(MacroP(Car(args))), nil
}

func IsFrameImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(FrameP(Car(args))), nil
}

func IsByteArrayImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(ObjectP(Car(args)) && ObjectType(Car(args)) == "[]byte"), nil
}

func IsPortImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(PortP(Car(args))), nil
}
