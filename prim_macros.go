// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the macro primitive functions.

package golisp

import (
    "errors"
)

func RegisterMacroPrimitives() {
    MakePrimitiveFunction("quote", 1, QuoteImpl)
    MakePrimitiveFunction("quasiquote", 1, QuasiquoteImpl)
    MakePrimitiveFunction("unquote", 1, UnquoteImpl)
    MakePrimitiveFunction("unquote-splicing", 1, UnquoteSplicingImpl)
}

func QuoteImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    return Car(args), nil
}

func processQuasiquoted(sexpr *Data, env *SymbolTableFrame) (result *Data, err error) {
    if !ListP(sexpr) {
        return Cons(sexpr, nil), nil
    } else if SymbolP(Car(sexpr)) && StringValue(Car(sexpr)) == "quasiquote" {
        quasiquoteLevel += 1
        parts := make([]*Data, 0, Length(Cdr(sexpr)))
        for _, exp := range ToArray(Cdr(sexpr)) {
            processed, err := processQuasiquoted(exp, env)
            if err != nil {
                return nil, err
            }
            parts = append(parts, Car(processed))
        }
        return Cons(ArrayToList(parts), nil), nil
    } else if SymbolP(Car(sexpr)) && StringValue(Car(sexpr)) == "unquote" {
        if quasiquoteLevel == 1 {
            processed, err := processQuasiquoted(Cadr(sexpr), env)
            if err != nil {
                return nil, err
            }
            r, err := Eval(Car(processed), env)
            if err != nil {
                return nil, err
            }
            return Cons(r, nil), nil
        } else {
            quasiquoteLevel -= 1
        }
    } else if SymbolP(Car(sexpr)) && StringValue(Car(sexpr)) == "unquote-splicing" {
        if quasiquoteLevel == 1 {
            processed, err := processQuasiquoted(Cadr(sexpr), env)
            if err != nil {
                return nil, err
            }
            r, err := Eval(Car(processed), env)
            if err != nil {
                return nil, err
            }
            return r, nil
        } else {
            quasiquoteLevel -= 1
        }
    } else {
        parts := make([]*Data, 0, Length(Cdr(sexpr)))
        for _, exp := range ToArray(sexpr) {
            processed, err := processQuasiquoted(exp, env)
            if err != nil {
                return nil, err
            }
            parts = append(parts, processed)
        }
        flat, err := Flatten(ArrayToList(parts))
        if err != nil {
            return nil, err
        }
        return Cons(flat, nil), nil
    }
    return
}

func QuasiquoteImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    quasiquoteLevel = 1
    r, err := processQuasiquoted(Car(args), env)
    if err != nil {
        return nil, err
    }
    return Car(r), nil
}

func UnquoteImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    err = errors.New("unquote should not be used outside of a quasiquoted expression.")
    return
}

func UnquoteSplicingImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    err = errors.New("unquote-splicing should not be used outside of a quasiquoted expression.")
    return
}
