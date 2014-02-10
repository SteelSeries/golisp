// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the macro primitive functions.

package golisp

import (
    "errors"
    "fmt"
)

func RegisterMacroPrimitives() {
    MakePrimitiveFunction("quote", 1, QuoteImpl)
    MakePrimitiveFunction("quasiquote", 1, QuasiquoteImpl)
    MakePrimitiveFunction("unquote", 1, UnquoteImpl)
    MakePrimitiveFunction("unquote-splicing", 1, UnquoteSplicingImpl)
    MakePrimitiveFunction("expand", -1, ExpandImpl)
}

func QuoteImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    return Car(args), nil
}

func processQuasiquoted(sexpr *Data, level int, env *SymbolTableFrame) (result *Data, err error) {
    if !ListP(sexpr) {
        return Cons(sexpr, nil), nil
    } else if SymbolP(Car(sexpr)) && StringValue(Car(sexpr)) == "quasiquote" {
        processed, err := processQuasiquoted(Cadr(sexpr), level + 1, env)
        if err != nil {
            return nil, err
        }
        return Cons(Cons(SymbolWithName("quasiquote"), processed), nil), nil
    } else if SymbolP(Car(sexpr)) && StringValue(Car(sexpr)) == "unquote" {
        if level == 1 {
            processed, err := processQuasiquoted(Cadr(sexpr), level, env)
            if err != nil {
                return nil, err
            }
            r, err := Eval(Car(processed), env)
            if err != nil {
                return nil, err
            }
            return Cons(r, nil), nil
        } else {
            processed, err := processQuasiquoted(Cadr(sexpr), level - 1, env)
            if err != nil {
                return nil, err
            }
            return Cons(Cons(SymbolWithName("unquote"), processed), nil), nil
        }
    } else if SymbolP(Car(sexpr)) && StringValue(Car(sexpr)) == "unquote-splicing" {
        if level == 1 {
            processed, err := processQuasiquoted(Cadr(sexpr), level, env)
            if err != nil {
                return nil, err
            }
            r, err := Eval(Car(processed), env)
            if err != nil {
                return nil, err
            }
            return r, nil
        } else {
            processed, err := processQuasiquoted(Cadr(sexpr), level - 1, env)
            if err != nil {
                return nil, err
            }
            return Cons(Cons(SymbolWithName("unquote-splicing"), processed), nil), nil
        }
    } else {
        parts := make([]*Data, 0, Length(Cdr(sexpr)))
        for _, exp := range ToArray(sexpr) {
            processed, err := processQuasiquoted(exp, level, env)
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
    r, err := processQuasiquoted(Car(args), 1, env)
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

func ExpandImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    n, err := Eval(Car(args), env)
    if err != nil {
        return
    }
    if !MacroP(n) {
        err = errors.New(fmt.Sprintf("expand expected a macro, received %s", String(n)))
        return
    }
    return n.Mac.Expand(Cdr(args), env)
}

