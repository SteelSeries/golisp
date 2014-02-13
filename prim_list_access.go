// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the list access primitive functions.

package golisp

import (
    "errors"
)

func RegisterListAccessPrimitives() {
    MakePrimitiveFunction("car", 1, CarImpl)
    MakePrimitiveFunction("head", 1, CarImpl)
    MakePrimitiveFunction("cdr", 1, CdrImpl)
    MakePrimitiveFunction("rest", 1, CdrImpl)
    MakePrimitiveFunction("tail", 1, CdrImpl)

    MakePrimitiveFunction("caar", 1, CaarImpl)
    MakePrimitiveFunction("cadr", 1, CadrImpl)
    MakePrimitiveFunction("cdar", 1, CdarImpl)
    MakePrimitiveFunction("cddr", 1, CddrImpl)

    MakePrimitiveFunction("caaar", 1, CaaarImpl)
    MakePrimitiveFunction("caadr", 1, CaadrImpl)
    MakePrimitiveFunction("cadar", 1, CadarImpl)
    MakePrimitiveFunction("caddr", 1, CaddrImpl)
    MakePrimitiveFunction("cdaar", 1, CdaarImpl)
    MakePrimitiveFunction("cdadr", 1, CdadrImpl)
    MakePrimitiveFunction("cddar", 1, CddarImpl)
    MakePrimitiveFunction("cdddr", 1, CdddrImpl)

    MakePrimitiveFunction("caaaar", 1, CaaaarImpl)
    MakePrimitiveFunction("caaadr", 1, CaaadrImpl)
    MakePrimitiveFunction("caadar", 1, CaadarImpl)
    MakePrimitiveFunction("caaddr", 1, CaaddrImpl)
    MakePrimitiveFunction("cadaar", 1, CadaarImpl)
    MakePrimitiveFunction("cadadr", 1, CadadrImpl)
    MakePrimitiveFunction("caddar", 1, CaddarImpl)
    MakePrimitiveFunction("cadddr", 1, CadddrImpl)
    MakePrimitiveFunction("cdaaar", 1, CdaaarImpl)
    MakePrimitiveFunction("cdaadr", 1, CdaadrImpl)
    MakePrimitiveFunction("cdadar", 1, CdadarImpl)
    MakePrimitiveFunction("cdaddr", 1, CdaddrImpl)
    MakePrimitiveFunction("cddaar", 1, CddaarImpl)
    MakePrimitiveFunction("cddadr", 1, CddadrImpl)
    MakePrimitiveFunction("cdddar", 1, CdddarImpl)
    MakePrimitiveFunction("cddddr", 1, CddddrImpl)

    MakePrimitiveFunction("first", 1, FirstImpl)
    MakePrimitiveFunction("second", 1, SecondImpl)
    MakePrimitiveFunction("third", 1, ThirdImpl)
    MakePrimitiveFunction("fourth", 1, FourthImpl)
    MakePrimitiveFunction("fifth", 1, FifthImpl)
    MakePrimitiveFunction("sixth", 1, SixthImpl)
    MakePrimitiveFunction("seventh", 1, SeventhImpl)
    MakePrimitiveFunction("eighth", 1, EighthImpl)
    MakePrimitiveFunction("ninth", 1, NinthImpl)
    MakePrimitiveFunction("tenth", 1, TenthImpl)

    MakePrimitiveFunction("nth", 2, NthImpl)
    MakePrimitiveFunction("take", 2, TakeImpl)
    MakePrimitiveFunction("drop", 2, DropImpl)
}

func CarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "a"), nil
}

func CdrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "d"), nil
}

func CaarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "aa"), nil
}

func CadrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "ad"), nil
}

func CdarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "da"), nil
}

func CddrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "dd"), nil
}

func CaaarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "aaa"), nil
}

func CaadrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "aad"), nil
}

func CadarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "ada"), nil
}

func CaddrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "add"), nil
}

func CdaarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "daa"), nil
}

func CdadrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "dad"), nil
}

func CddarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "dda"), nil
}

func CdddrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "ddd"), nil
}

func CaaaarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "aaaa"), nil
}

func CaaadrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "aaad"), nil
}

func CaadarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "aada"), nil
}

func CaaddrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "aadd"), nil
}

func CadaarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "adaa"), nil
}

func CadadrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "adad"), nil
}

func CaddarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "adda"), nil
}

func CadddrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "addd"), nil
}

func CdaaarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "daaa"), nil
}

func CdaadrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "daad"), nil
}

func CdadarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "dada"), nil
}

func CdaddrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "dadd"), nil
}

func CddaarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "ddaa"), nil
}

func CddadrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "ddad"), nil
}

func CdddarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "ddda"), nil
}

func CddddrImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return WalkList(a, "dddd"), nil
}

func FirstImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return First(a), nil
}

func SecondImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Second(a), nil
}

func ThirdImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Third(a), nil
}

func FourthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Fourth(a), nil
}

func FifthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Fifth(a), nil
}

func SixthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Sixth(a), nil
}

func SeventhImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Seventh(a), nil
}

func EighthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Eighth(a), nil
}

func NinthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Ninth(a), nil
}

func TenthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    a, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    return Tenth(a), nil
}

func NthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    col, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    if !PairP(col) {
        err = errors.New("First arg to nth must be a list")
        return
    }
    count, err := Eval(Cadr(args), env)
    if err != nil {
        return
    }
    if !IntegerP(count) {
        err = errors.New("Second arg to nth must be a number")
        return
    }

    return Nth(col, int(IntegerValue(count))), nil
}

func TakeImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    n, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    if !IntegerP(n) {
        err = errors.New("take requires a number as it's first argument.")
    }
    size := int(IntegerValue(n))

    l, err := Eval(Cadr(args), env)
    if err != nil {
        return
    }
    if !ListP(l) {
        err = errors.New("take requires a list as it's second argument.")
    }

    var items []*Data = make([]*Data, 0, Length(args))
    for i, cell := 0, l; i < size && NotNilP(cell); i, cell = i+1, Cdr(cell) {
        items = append(items, Car(cell))
    }
    result = ArrayToList(items)
    return
}

func DropImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    n, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    if !IntegerP(n) {
        err = errors.New("drop requires a number as it's first argument.")
    }
    size := int(IntegerValue(n))

    l, err := Eval(Cadr(args), env)
    if err != nil {
        return
    }
    if !ListP(l) {
        err = errors.New("drop requires a list as it's second argument.")
    }

    var cell *Data
    var i int
    for i, cell = 0, l; i < size && NotNilP(cell); i, cell = i+1, Cdr(cell) {
    }
    result = cell
    return
}