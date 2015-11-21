// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the type predicate primitive functions.

package golisp

func RegisterTypePredicatePrimitives() {
	MakePrimitiveFunction("atom?", "1", IsAtomImpl)
	MakePrimitiveFunction("list?", "1", IsListImpl)
	MakePrimitiveFunction("pair?", "1", IsPairImpl)
	MakePrimitiveFunction("circular-list?", "1", IsCircularListImpl)
	MakePrimitiveFunction("dotted-list?", "1", IsDottedListImpl)
	MakePrimitiveFunction("nil?", "1", IsNilImpl)
	MakePrimitiveFunction("null?", "1", IsNilImpl)
	MakePrimitiveFunction("notnil?", "1", IsNotNilImpl)
	MakePrimitiveFunction("notnull?", "1", IsNotNilImpl)
	MakePrimitiveFunction("symbol?", "1", IsSymbolImpl)
	MakePrimitiveFunction("string?", "1", IsStringImpl)
	MakePrimitiveFunction("integer?", "1", IsIntegerImpl)
	MakePrimitiveFunction("number?", "1", IsNumberImpl)
	MakePrimitiveFunction("float?", "1", IsFloatImpl)
	MakePrimitiveFunction("function?", "1", IsFunctionImpl)
	MakePrimitiveFunction("macro?", "1", IsMacroImpl)
	MakePrimitiveFunction("frame?", "1", IsFrameImpl)
	MakePrimitiveFunction("bytearray?", "1", IsByteArrayImpl)
	MakePrimitiveFunction("port?", "1", IsPortImpl)
}

func IsAtomImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	val := Car(args)
	return BooleanWithValue(NumberP(val) || SymbolP(val) || StringP(val) || BooleanP(val) || CharacterP(val)), nil
}

func IsListImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	result = BooleanWithValue(ListP(First(args)))
	return
}

func IsPairImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(PairP(Car(args))), nil
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

func IsStringImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(StringP(Car(args))), nil
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
