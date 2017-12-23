// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpreter for embedding in a go program for scripting.
// This file contains the macro primitive functions.

package golisp

import (
	"fmt"
)

func RegisterMacroPrimitives() {
	MakeSpecialForm("quote", "1", QuoteImpl)
	MakeSpecialForm("quasiquote", "1", QuasiquoteImpl)
	MakeSpecialForm("unquote", "1", UnquoteImpl)
	MakeSpecialForm("unquote-splicing", "1", UnquoteSplicingImpl)
	MakeSpecialForm("expand", ">=1", ExpandImpl)
}

func QuoteImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if Car(args) == nil {
		return EmptyCons(), nil
	} else {
		return Car(args), nil
	}
}

func processQuasiquoted(sexpr *Data, level int, env *SymbolTableFrame) (result *Data, err error) {
	if !ListP(sexpr) {
		return Cons(sexpr, nil), nil
	} else if SymbolP(Car(sexpr)) && StringValue(Car(sexpr)) == "quasiquote" {
		processed, err := processQuasiquoted(Cadr(sexpr), level+1, env)
		if err != nil {
			return nil, err
		}
		return Cons(Cons(Intern("quasiquote"), processed), nil), nil
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
			processed, err := processQuasiquoted(Cadr(sexpr), level-1, env)
			if err != nil {
				return nil, err
			}
			return Cons(Cons(Intern("unquote"), processed), nil), nil
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
			if NilP(r) {
				return nil, nil
			} else {
				return r, nil
			}
		} else {
			processed, err := processQuasiquoted(Cadr(sexpr), level-1, env)
			if err != nil {
				return nil, err
			}
			return Cons(Cons(Intern("unquote-splicing"), processed), nil), nil
		}
	} else {
		parts := make([]*Data, 0, Length(Cdr(sexpr)))
		for _, exp := range ToArray(sexpr) {
			processed, err := processQuasiquoted(exp, level, env)
			if err != nil {
				return nil, err
			}
			if processed != nil {
				parts = append(parts, processed)
			}
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
	err = ProcessError("unquote should not be used outside of a quasiquoted expression.", env)
	return
}

func UnquoteSplicingImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	err = ProcessError("unquote-splicing should not be used outside of a quasiquoted expression.", env)
	return
}

func ExpandImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	n, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !MacroP(n) {
		err = ProcessError(fmt.Sprintf("expand expected a macro, received %s", String(n)), env)
		return
	}
	return MacroValue(n).Expand(Cdr(args), env)
}
