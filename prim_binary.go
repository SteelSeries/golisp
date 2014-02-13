// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the binary primitive functions.

package golisp

import (
    "errors"
    "fmt"
)

func RegisterBinaryPrimitives() {
    MakePrimitiveFunction("binary-and", 2, BinaryAndImpl)
    MakePrimitiveFunction("binary-or", 2, BinaryOrImpl)
    MakePrimitiveFunction("binary-not", 1, BinaryNotImpl)
    MakePrimitiveFunction("left-shift", 2, LeftShiftImpl)
    MakePrimitiveFunction("right-shift", 2, RightShiftImpl)
}

func BinaryAndImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    arg1, err := Eval(First(args), env)
    if err != nil {
        return
    }
    if !IntegerP(arg1) {
        err = errors.New(fmt.Sprintf("Integer expected, received %s %s", TypeName(TypeOf(arg1)), String(arg1)))
        return
    }
    b1 := uint64(IntegerValue(arg1))

    arg2, err := Eval(Second(args), env)
    if err != nil {
        return
    }
    if !IntegerP(arg2) {
        err = errors.New(fmt.Sprintf("Integer expected, received %s %s", TypeName(TypeOf(arg2)), String(arg2)))
        return
    }
    b2 := uint64(IntegerValue(arg2))
    
    return IntegerWithValue(int64(b1 & b2)), nil
}

func BinaryOrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    arg1, err := Eval(First(args), env)
    if err != nil {
        return
    }
    if !IntegerP(arg1) {
        err = errors.New(fmt.Sprintf("Integer expected, received %s %s", TypeName(TypeOf(arg1)), String(arg1)))
        return
    }
    b1 := uint64(IntegerValue(arg1))

    arg2, err := Eval(Second(args), env)
    if err != nil {
        return
    }
    if !IntegerP(arg2) {
        err = errors.New(fmt.Sprintf("Integer expected, received %s %s", TypeName(TypeOf(arg2)), String(arg2)))
        return
    }
    b2 := uint64(IntegerValue(arg2))
    
    return IntegerWithValue(int64(b1 | b2)), nil
}

func BinaryNotImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    arg1, err := Eval(First(args), env)
    if err != nil {
        return
    }
    if !IntegerP(arg1) {
        err = errors.New(fmt.Sprintf("Integer expected, received %s %s", TypeName(TypeOf(arg1)), String(arg1)))
        return
    }
    b1 := uint64(IntegerValue(arg1))
    
    return IntegerWithValue(int64(b1 ^ uint64(0xFFFFFFFF))), nil
}

func LeftShiftImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    arg1, err := Eval(First(args), env)
    if err != nil {
        return
    }
    if !IntegerP(arg1) {
        err = errors.New(fmt.Sprintf("Integer expected, received %s %s", TypeName(TypeOf(arg1)), String(arg1)))
        return
    }
    b1 := uint64(IntegerValue(arg1))

    arg2, err := Eval(Second(args), env)
    if err != nil {
        return
    }
    if !IntegerP(arg2) {
        err = errors.New(fmt.Sprintf("Integer expected, received %s %s", TypeName(TypeOf(arg2)), String(arg2)))
        return
    }
    b2 := uint64(IntegerValue(arg2))
    
    return IntegerWithValue(int64(b1 << b2)), nil
}

func RightShiftImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    arg1, err := Eval(First(args), env)
    if err != nil {
        return
    }
    if !IntegerP(arg1) {
        err = errors.New(fmt.Sprintf("Integer expected, received %s %s", TypeName(TypeOf(arg1)), String(arg1)))
        return
    }
    b1 := uint64(IntegerValue(arg1))

    arg2, err := Eval(Second(args), env)
    if err != nil {
        return
    }
    if !IntegerP(arg2) {
        err = errors.New(fmt.Sprintf("Integer expected, received %s %s", TypeName(TypeOf(arg2)), String(arg2)))
        return
    }
    b2 := uint64(IntegerValue(arg2))
    
    return IntegerWithValue(int64(b1 >> b2)), nil
}
