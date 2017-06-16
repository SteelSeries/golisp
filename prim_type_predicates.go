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
	MakePrimitiveFunction("atom?", "1", isAtomImpl)
	MakePrimitiveFunction("list?", "1", isListImpl)
	MakePrimitiveFunction("pair?", "1", isPairImpl)
	MakePrimitiveFunction("dotted-pair?", "1", isDottedPairImpl)
	MakePrimitiveFunction("circular-list?", "1", isCircularListImpl)
	MakePrimitiveFunction("dotted-list?", "1", isDottedListImpl)
	MakePrimitiveFunction("proper-list?", "1", isProperListImpl)
	MakePrimitiveFunction("nil?", "1", isNilImpl)
	MakePrimitiveFunction("null?", "1", isNilImpl)
	MakePrimitiveFunction("notnil?", "1", isNotNilImpl)
	MakePrimitiveFunction("notnull?", "1", isNotNilImpl)
	MakePrimitiveFunction("symbol?", "1", isSymbolImpl)
	MakePrimitiveFunction("naked?", "1", isNakedImpl)
	MakePrimitiveFunction("string?", "1", isStringImpl)
	MakePrimitiveFunction("boolean?", "1", isBooleanImpl)
	MakePrimitiveFunction("integer?", "1", isIntegerImpl)
	MakePrimitiveFunction("number?", "1", isNumberImpl)
	MakePrimitiveFunction("float?", "1", isFloatImpl)
	MakePrimitiveFunction("function?", "1", isFunctionImpl)
	MakePrimitiveFunction("primitive?", "1", isPrimitiveImpl)
	MakePrimitiveFunction("compiled-function?", "1", isCompiledFunctionImpl)
	MakePrimitiveFunction("special-form?", "1", isSpecialFormImpl)
	MakePrimitiveFunction("macro?", "1", isMacroImpl)
	MakePrimitiveFunction("compiler-macro?", "1", isMacroImpl)
	MakePrimitiveFunction("frame?", "1", isFrameImpl)
	MakePrimitiveFunction("bytearray?", "1", isByteArrayImpl)
	MakePrimitiveFunction("port?", "1", isPortImpl)
}

func TypeofImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	val := Car(args)
	result = Intern(strings.ToLower(TypeName(TypeOf(val))))
	return
}

func isAtomImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	val := Car(args)
	return BooleanWithValue(NumberP(val) || StringP(val) || BooleanP(val) || CharacterP(val)), nil
}

func isProperListImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	result = BooleanWithValue(ProperListP(First(args)))
	return
}

func isListImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(ListP(Car(args))), nil
}

func isPairImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(PairP(Car(args))), nil
}

func isDottedPairImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(DottedPairP(Car(args))), nil
}

func isCircularListImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(ListWithLoopP(Car(args))), nil
}

func isDottedListImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(DottedListP(Car(args))), nil
}

func isNilImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(NilP(Car(args))), nil
}

func isNotNilImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(NotNilP(Car(args))), nil
}

func isSymbolImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(SymbolP(Car(args))), nil
}

func isNakedImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(NakedP(Car(args))), nil
}

func isStringImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(StringP(Car(args))), nil
}

func isBooleanImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(BooleanP(Car(args))), nil
}

func isIntegerImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(IntegerP(Car(args))), nil
}

func isNumberImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(NumberP(Car(args))), nil
}

func isFloatImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(FloatP(Car(args))), nil
}

func isFunctionImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(FunctionOrPrimitiveP(Car(args))), nil
}

func isPrimitiveImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(PrimitiveP(Car(args))), nil
}

func isCompiledFunctionImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(CompiledFunctionP(Car(args))), nil
}

func isSpecialFormImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(PrimitiveP(Car(args)) && PrimitiveValue(Car(args)).Special), nil
}

func isMacroImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(MacroP(Car(args))), nil
}

func isCompilerMacroImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(CompilerMacroP(Car(args))), nil
}

func isFrameImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(FrameP(Car(args))), nil
}

func isByteArrayImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(ObjectP(Car(args)) && ObjectType(Car(args)) == "[]byte"), nil
}

func isPortImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(PortP(Car(args))), nil
}

func IsBooleanImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(BooleanP(Car(args))), nil
}
