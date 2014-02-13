// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the association list primitive functions.

package golisp

import (
    "errors"
)

func RegisterAListPrimitives() {
    MakePrimitiveFunction("acons", -1, AconsImpl)
    MakePrimitiveFunction("pairlis", -1, PairlisImpl)
    MakePrimitiveFunction("assoc", 2, AssocImpl)
    MakePrimitiveFunction("dissoc", 2, DissocImpl)
    MakePrimitiveFunction("rassoc", 2, RassocImpl)
    MakePrimitiveFunction("alist", 1, AlistImpl)
}

func AlistImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    l, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    result = Alist(l)
    return
}

func AconsImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var key *Data
    var value *Data
    var alist *Data

    if Length(args) < 2 || Length(args) > 3 {
        err = errors.New("acons must have 2 or 3 arguments")
        return
    }

    key, err = Eval(First(args), env)
    if err != nil {
        return
    }

    if PairP(key) {
        err = errors.New("Alist key can not be a list")
        return
    }

    value, err = Eval(Second(args), env)
    if err != nil {
        return
    }

    if Length(args) == 3 {
        alist, err = Eval(Third(args), env)
        if err != nil {
            return
        }
    }

    result = Acons(key, value, alist)
    return
}

func PairlisImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var keys *Data
    var values *Data
    if Length(args) > 3 {
        err = errors.New("pairlis takes at most three arguments")
        return
    }

    keys, err = Eval(Car(args), env)
    if err != nil {
        return
    }
    if !PairP(keys) {
        err = errors.New("First arg of pairlis must be a list")
        return
    }

    values, err = Eval(Cadr(args), env)
    if err != nil {
        return
    }

    if !PairP(values) {
        err = errors.New("Second arg of Pairlis must be a list")
        return
    }

    if Length(keys) != Length(values) {
        err = errors.New("Pairlis requires the same number of keys and values")
        return
    }

    result, err = Eval(Third(args), env)
    if err != nil {
        return
    }

    if NotNilP(result) {
        if !PairP(result) {
            err = errors.New("Third arg of pairlis must be an association list (if provided)")
            return
        }
    }

    for keyCell, valueCell := keys, values; NotNilP(keyCell); keyCell, valueCell = Cdr(keyCell), Cdr(valueCell) {
        key := Car(keyCell)
        if NilP(keyCell) {
            err = errors.New("Assoc list keys can not be nil")
        }
        value := Car(valueCell)
        result = Acons(key, value, result)
    }

    return
}

func AssocImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var key *Data
    var list *Data

    key, err = Eval(Car(args), env)
    if err != nil {
        return
    }

    list, err = Eval(Cadr(args), env)
    if err != nil {
        return
    }

    result, err = Assoc(key, list)
    return
}

func RassocImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var value *Data
    var list *Data

    value, err = Eval(Car(args), env)
    if err != nil {
        return
    }

    list, err = Eval(Cadr(args), env)
    if err != nil {
        return
    }

    for c := list; NotNilP(c); c = Cdr(c) {
        pair := Car(c)
        if !PairP(pair) && !DottedPairP(pair) {
            err = errors.New("Assoc list must consist of dotted pairs")
            return
        }
        if IsEqual(Cdr(pair), value) {
            result = pair
            return
        }
    }
    return
}

func DissocImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var key *Data
    var list *Data

    key, err = Eval(Car(args), env)
    if err != nil {
        return
    }

    list, err = Eval(Cadr(args), env)
    if err != nil {
        return
    }

    result, err = Dissoc(key, list)
    return
}

