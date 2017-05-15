// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the binary primitive functions.

package golisp

func RegisterBinaryPrimitives() {
	MakeTypedPrimitiveFunction("binary-and", "2", BinaryAndImpl, []uint32{IntegerType, IntegerType})
	MakeTypedPrimitiveFunction("binary-or", "2", BinaryOrImpl, []uint32{IntegerType, IntegerType})
	MakeTypedPrimitiveFunction("binary-not", "1", BinaryNotImpl, []uint32{IntegerType, IntegerType})
	MakeTypedPrimitiveFunction("left-shift", "2", LeftShiftImpl, []uint32{IntegerType, IntegerType})
	MakeTypedPrimitiveFunction("right-shift", "2", RightShiftImpl, []uint32{IntegerType, IntegerType})
}

func BinaryAndImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	b1 := uint64(IntegerValue(First(args)))
	b2 := uint64(IntegerValue(Second(args)))
	return IntegerWithValue(int64(b1 & b2)), nil
}

func BinaryOrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	b1 := uint64(IntegerValue(First(args)))
	b2 := uint64(IntegerValue(Second(args)))
	return IntegerWithValue(int64(b1 | b2)), nil
}

func BinaryNotImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	b1 := uint64(IntegerValue(First(args)))
	return IntegerWithValue(int64(b1 ^ uint64(0xFFFFFFFF))), nil
}

func LeftShiftImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	b1 := uint64(IntegerValue(First(args)))
	b2 := uint64(IntegerValue(Second(args)))
	return IntegerWithValue(int64(b1 << b2)), nil
}

func RightShiftImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	b1 := uint64(IntegerValue(First(args)))
	b2 := uint64(IntegerValue(Second(args)))
	return IntegerWithValue(int64(b1 >> b2)), nil
}
