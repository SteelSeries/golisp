// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the list access primitive functions.

package golisp

import (
    "errors"
)

func RegisterListFunctionsPrimitives() {
    MakePrimitiveFunction("map", 2, MapImpl)
}

func MapImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    f, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    if !FunctionP(f) {
        err = errors.New("Map needs a function as its first argument")
        return
    }

    col, err := Eval(Cadr(args), env)
    if err != nil {
        return
    }
    if !ListP(col) {
        err = errors.New("Map needs a list as its second argument")
        return
    }

    var d []*Data = make([]*Data, 0, Length(col))
    var v *Data
    for c := col; NotNilP(c); c = Cdr(c) {
        v, err = ApplyWithoutEval(f, Cons(Car(c), nil), env)
        if err != nil {
            return
        }
        d = append(d, v)
    }

    return ArrayToList(d), nil
}
