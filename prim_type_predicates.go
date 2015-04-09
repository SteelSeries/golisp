// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the type predicate primitive functions.

package golisp

func RegisterTypePredicatePrimitives() {
	MakePrimitiveFunction("list?", "1", IsPairImpl)
	MakePrimitiveFunction("pair?", "1", IsPairImpl)
	MakePrimitiveFunction("alist?", "1", IsAlistImpl)
	MakePrimitiveFunction("nil?", "1", NilPImpl)
	MakePrimitiveFunction("notnil?", "1", NotNilPImpl)
	MakePrimitiveFunction("symbol?", "1", IsSymbolImpl)
	MakePrimitiveFunction("string?", "1", IsStringImpl)
	MakePrimitiveFunction("integer?", "1", IsIntegerImpl)
	MakePrimitiveFunction("number?", "1", IsNumberImpl)
	MakePrimitiveFunction("float?", "1", IsFloatImpl)
	MakePrimitiveFunction("function?", "1", IsFunctionImpl)
	MakePrimitiveFunction("macro?", "1", IsMacroImpl)
}

func IsPairImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(PairP(Car(args))), nil
}

func IsAlistImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(AlistP(Car(args))), nil
}

func NilPImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(NilP(Car(args))), nil
}

func NotNilPImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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
	return BooleanWithValue(FunctionP(Car(args))), nil
}

func IsMacroImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(MacroP(Car(args))), nil
}
