// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the binary primitive functions.

package golisp

import (
	"fmt"
)

func RegisterBinaryPrimitives() {
	MakePrimitiveFunction("binary-and", "2", BinaryAndImpl)
	MakePrimitiveFunction("binary-or", "2", BinaryOrImpl)
	MakePrimitiveFunction("binary-not", "1", BinaryNotImpl)
	MakePrimitiveFunction("left-shift", "2", LeftShiftImpl)
	MakePrimitiveFunction("right-shift", "2", RightShiftImpl)
}

func BinaryAndImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	arg1 := First(args)
	if !IntegerP(arg1) {
		err = ProcessError(fmt.Sprintf("Integer expected, received %s %s", TypeName(TypeOf(arg1)), String(arg1)), env)
		return
	}
	b1 := uint64(IntegerValue(arg1))

	arg2 := Second(args)
	if !IntegerP(arg2) {
		err = ProcessError(fmt.Sprintf("Integer expected, received %s %s", TypeName(TypeOf(arg2)), String(arg2)), env)
		return
	}
	b2 := uint64(IntegerValue(arg2))

	return IntegerWithValue(int64(b1 & b2)), nil
}

func BinaryOrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	arg1 := First(args)
	if !IntegerP(arg1) {
		err = ProcessError(fmt.Sprintf("Integer expected, received %s %s", TypeName(TypeOf(arg1)), String(arg1)), env)
		return
	}
	b1 := uint64(IntegerValue(arg1))

	arg2 := Second(args)
	if !IntegerP(arg2) {
		err = ProcessError(fmt.Sprintf("Integer expected, received %s %s", TypeName(TypeOf(arg2)), String(arg2)), env)
		return
	}
	b2 := uint64(IntegerValue(arg2))

	return IntegerWithValue(int64(b1 | b2)), nil
}

func BinaryNotImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	arg1 := First(args)
	if !IntegerP(arg1) {
		err = ProcessError(fmt.Sprintf("Integer expected, received %s %s", TypeName(TypeOf(arg1)), String(arg1)), env)
		return
	}
	b1 := uint64(IntegerValue(arg1))

	return IntegerWithValue(int64(b1 ^ uint64(0xFFFFFFFF))), nil
}

func LeftShiftImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	arg1 := First(args)
	if !IntegerP(arg1) {
		err = ProcessError(fmt.Sprintf("Integer expected, received %s %s", TypeName(TypeOf(arg1)), String(arg1)), env)
		return
	}
	b1 := uint64(IntegerValue(arg1))

	arg2 := Second(args)
	if !IntegerP(arg2) {
		err = ProcessError(fmt.Sprintf("Integer expected, received %s %s", TypeName(TypeOf(arg2)), String(arg2)), env)
		return
	}
	b2 := uint64(IntegerValue(arg2))

	return IntegerWithValue(int64(b1 << b2)), nil
}

func RightShiftImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	arg1 := First(args)
	if !IntegerP(arg1) {
		err = ProcessError(fmt.Sprintf("Integer expected, received %s %s", TypeName(TypeOf(arg1)), String(arg1)), env)
		return
	}
	b1 := uint64(IntegerValue(arg1))

	arg2 := Second(args)
	if !IntegerP(arg2) {
		err = ProcessError(fmt.Sprintf("Integer expected, received %s %s", TypeName(TypeOf(arg2)), String(arg2)), env)
		return
	}
	b2 := uint64(IntegerValue(arg2))

	return IntegerWithValue(int64(b1 >> b2)), nil
}
