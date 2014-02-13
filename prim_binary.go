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
    if TypeOf(arg1) != NumberType {
        err = errors.New(fmt.Sprintf("Integer expected, received %s", String(arg1)))
        return
    }
    b1 := NumericValue(arg1)

    arg2, err := Eval(Second(args), env)
    if err != nil {
        return
    }
    if TypeOf(arg2) != NumberType {
        err = errors.New(fmt.Sprintf("Integer expected, received %s", String(arg2)))
        return
    }
    b2 := NumericValue(arg2)
    
    return NumberWithValue(b1 & b2), nil
}

func BinaryOrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    arg1, err := Eval(First(args), env)
    if err != nil {
        return
    }
    if TypeOf(arg1) != NumberType {
        err = errors.New(fmt.Sprintf("Integer expected, received %s", String(arg1)))
        return
    }
    b1 := NumericValue(arg1)

    arg2, err := Eval(Second(args), env)
    if err != nil {
        return
    }
    if TypeOf(arg2) != NumberType {
        err = errors.New(fmt.Sprintf("Integer expected, received %s", String(arg2)))
        return
    }
    b2 := NumericValue(arg2)
    
    return NumberWithValue(b1 | b2), nil
}

func BinaryNotImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    arg1, err := Eval(First(args), env)
    if err != nil {
        return
    }
    if TypeOf(arg1) != NumberType {
        err = errors.New(fmt.Sprintf("Integer expected, received %s", String(arg1)))
        return
    }
    b1 := NumericValue(arg1)
    
    return NumberWithValue(b1 ^ uint32(0xFFFFFFFF)), nil
}

func LeftShiftImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    arg1, err := Eval(First(args), env)
    if err != nil {
        return
    }
    if TypeOf(arg1) != NumberType {
        err = errors.New(fmt.Sprintf("Integer expected, received %s", String(arg1)))
        return
    }
    b1 := NumericValue(arg1)

    arg2, err := Eval(Second(args), env)
    if err != nil {
        return
    }
    if TypeOf(arg2) != NumberType {
        err = errors.New(fmt.Sprintf("Integer expected, received %s", String(arg2)))
        return
    }
    b2 := NumericValue(arg2)
    
    return NumberWithValue(b1 << b2), nil
}

func RightShiftImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    arg1, err := Eval(First(args), env)
    if err != nil {
        return
    }
    if TypeOf(arg1) != NumberType {
        err = errors.New(fmt.Sprintf("Integer expected, received %s", String(arg1)))
        return
    }
    b1 := NumericValue(arg1)

    arg2, err := Eval(Second(args), env)
    if err != nil {
        return
    }
    if TypeOf(arg2) != NumberType {
        err = errors.New(fmt.Sprintf("Integer expected, received %s", String(arg2)))
        return
    }
    b2 := NumericValue(arg2)
    
    return NumberWithValue(b1 >> b2), nil
}
