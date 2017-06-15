// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the macro primitive functions.

package golisp

import (
	"fmt"
)

var quasiquoteSymbol = Intern("quasiquote")
var unquoteSymbol = Intern("unquote")
var unquoteSplicingSymbol = Intern("unquote-splicing")

func RegisterMacroPrimitives() {
	// MakeSpecialForm("quote", "1", QuoteImpl)
	MakeSpecialForm("quasiquote", "1", QuasiquoteImpl)
	MakeSpecialForm("unquote", "1", UnquoteImpl)
	MakeSpecialForm("unquote-splicing", "1", UnquoteSplicingImpl)
	MakeSpecialForm("expand", ">=1", ExpandImpl)
}

func processQuasiquoted(sexpr *Data, level int, env *SymbolTableFrame) (result *Data, err error) {
	if !ListP(sexpr) {
		return Cons(sexpr, nil), nil
	} else {
		firstExpr := First(sexpr)
		isSymbolicCar := SymbolP(firstExpr)

		if isSymbolicCar && firstExpr == quasiquoteSymbol {
			processed, err := processQuasiquoted(Second(sexpr), level+1, env)
			if err != nil {
				return nil, err
			}
			return Cons(Cons(quasiquoteSymbol, processed), nil), nil
		} else if isSymbolicCar && firstExpr == unquoteSymbol {
			if level == 1 {
				processed, err := processQuasiquoted(Second(sexpr), level, env)
				if err != nil {
					return nil, err
				}
				r, err := Eval(First(processed), env)
				if err != nil {
					return nil, err
				}
				return Cons(r, nil), nil
			} else {
				processed, err := processQuasiquoted(Second(sexpr), level-1, env)
				if err != nil {
					return nil, err
				}
				return Cons(Cons(unquoteSymbol, processed), nil), nil
			}
		} else if isSymbolicCar && firstExpr == unquoteSplicingSymbol {
			if level == 1 {
				processed, err := processQuasiquoted(Second(sexpr), level, env)
				if err != nil {
					return nil, err
				}
				r, err := Eval(First(processed), env)
				if err != nil {
					return nil, err
				}
				if NilP(r) {
					return nil, nil
				} else {
					return r, nil
				}
			} else {
				processed, err := processQuasiquoted(Second(sexpr), level-1, env)
				if err != nil {
					return nil, err
				}
				return Cons(Cons(unquoteSplicingSymbol, processed), nil), nil
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
	}
	return
}

// func QuoteImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
// 	// if First(args) == nil {
// 	// 	return EmptyCons(), nil
// 	// } else {
// 	return First(args), nil
// 	// }
// }

func QuasiquoteImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	r, err := processQuasiquoted(First(args), 1, env)
	if err != nil {
		return nil, err
	}
	return First(r), nil
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
	n, err := Eval(First(args), env)
	if err != nil {
		return
	}
	if !MacroP(n) {
		err = ProcessError(fmt.Sprintf("expand expected a macro, received %s", String(n)), env)
		return
	}
	return MacroValue(n).Expand(Cdr(args), env)
}
