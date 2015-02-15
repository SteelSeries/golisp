// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the built-in primitive functions.

package golisp

import (
	"fmt"
)

func RegisterSpecialFormPrimitives() {
	MakePrimitiveFunction("cond", -1, CondImpl)
	MakePrimitiveFunction("case", -1, CaseImpl)
	MakePrimitiveFunction("if", -1, IfImpl)
	MakePrimitiveFunction("lambda", -1, LambdaImpl)
	MakePrimitiveFunction("define", -1, DefineImpl)
	MakePrimitiveFunction("defmacro", -1, DefmacroImpl)
	MakePrimitiveFunction("let", -1, LetImpl)
	MakePrimitiveFunction("begin", -1, BeginImpl)
	MakePrimitiveFunction("do", -1, DoImpl)
	MakePrimitiveFunction("apply", -1, ApplyImpl)
	MakePrimitiveFunction("eval", 1, EvalImpl)
	MakePrimitiveFunction("global-eval", 1, GlobalEvalImpl)
	MakePrimitiveFunction("->", -1, ChainImpl)
	MakePrimitiveFunction("=>", -1, TapImpl)
	MakePrimitiveFunction("definition-of", 1, DefinitionOfImpl)
}

func CondImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var condition *Data
	for c := args; NotNilP(c); c = Cdr(c) {
		clause := Car(c)
		if !PairP(clause) {
			err = ProcessError("Cond expect a sequence of clauses that are lists", env)
			return
		}
		condition, err = Eval(Car(clause), env)
		if err != nil {
			return
		}
		if BooleanValue(condition) || StringValue(Car(clause)) == "else" {
			for e := Cdr(clause); NotNilP(e); e = Cdr(e) {
				result, err = Eval(Car(e), env)
				if err != nil {
					return
				}
			}
			return
		}
	}
	return
}

func evalList(l *Data, env *SymbolTableFrame) (result *Data, err error) {
	for sexpr := l; NotNilP(sexpr); sexpr = Cdr(sexpr) {
		result, err = Eval(Car(sexpr), env)
		if err != nil {
			return
		}
	}
	return
}

func CaseImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var keyValue *Data

	keyValue, err = Eval(Car(args), env)
	if err != nil {
		return
	}

	for clauseCell := Cdr(args); NotNilP(clauseCell); clauseCell = Cdr(clauseCell) {
		clause := Car(clauseCell)
		if !PairP(clause) {
			err = ProcessError("Case requires non-atomic clauses", env)
			return
		}
		if ListP(Car(clause)) {
			for v := Car(clause); NotNilP(v); v = Cdr(v) {
				if IsEqual(Car(v), keyValue) {
					return evalList(Cdr(clause), env)
				}
			}
		} else if IsEqual(Car(clause), SymbolWithName("else")) {
			return evalList(Cdr(clause), env)
		}
	}

	return
}

func IfImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if Length(args) < 2 || Length(args) > 3 {
		err = ProcessError(fmt.Sprintf("IF requires 2 or 3 arguments. Received %d.", Length(args)), env)
		return
	}

	c, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	condition := BooleanValue(c)
	thenClause := Second(args)
	elseClause := Third(args)

	if condition {
		return Eval(thenClause, env)
	} else {
		return Eval(elseClause, env)
	}
}

func LambdaImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	params := Car(args)
	body := Cdr(args)
	return FunctionWithNameParamsBodyAndParent("anonymous", params, body, env), nil
}

func DefineImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var value *Data
	thing := Car(args)
	if SymbolP(thing) {
		value, err = Eval(Cadr(args), env)
		if err != nil {
			return
		}
	} else if PairP(thing) {
		name := Car(thing)
		params := Cdr(thing)
		thing = name
		if !SymbolP(name) {
			err = ProcessError("Function name has to be a symbol", env)
			return
		}
		body := Cdr(args)
		value = FunctionWithNameParamsBodyAndParent(StringValue(name), params, body, env)
	} else {
		err = ProcessError("Invalid definition", env)
		return
	}
	env.BindLocallyTo(thing, value)
	return value, nil
}

func DefmacroImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var value *Data
	thing := Car(args)
	if PairP(thing) {
		name := Car(thing)
		params := Cdr(thing)
		thing = name
		if !SymbolP(name) {
			err = ProcessError("Macro name has to be a symbol", env)
			return
		}
		body := Cadr(args)
		value = MacroWithNameParamsBodyAndParent(StringValue(name), params, body, env)
	} else {
		err = ProcessError("Invalid macro definition", env)
		return
	}
	env.BindLocallyTo(thing, value)
	return value, nil
}

func bindLetLocals(bindingForms *Data, env *SymbolTableFrame) (err error) {
	var name *Data
	var value *Data

	for cell := bindingForms; NotNilP(cell); cell = Cdr(cell) {
		bindingPair := Car(cell)
		if !PairP(bindingPair) {
			err = ProcessError("Let requires a list of bindings (with are pairs) as it's first argument", env)
			return
		}
		name = Car(bindingPair)
		if !SymbolP(name) {
			err = ProcessError("First part of a let binding pair must be a symbol", env)
		}
		value, err = Eval(Cadr(bindingPair), env)
		if err != nil {
			return
		}
		env.BindLocallyTo(name, value)
	}
	return
}

func LetImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if Length(args) < 1 {
		err = ProcessError("Let requires at least a list of bindings", env)
		return
	}

	if !PairP(Car(args)) {
		err = ProcessError("Let requires a list of bindings as it's first argument", env)
		return
	}

	localEnv := NewSymbolTableFrameBelow(env)
	localEnv.Previous = env
	bindLetLocals(Car(args), localEnv)

	for cell := Cdr(args); NotNilP(cell); cell = Cdr(cell) {
		sexpr := Car(cell)
		result, err = Eval(sexpr, localEnv)
		if err != nil {
			return
		}
	}

	return
}

func BeginImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	for cell := args; NotNilP(cell); cell = Cdr(cell) {
		sexpr := Car(cell)
		result, err = Eval(sexpr, env)
		if err != nil {
			return
		}
	}
	return
}

func rebindDoLocals(bindingForms *Data, env *SymbolTableFrame) (err error) {
	var name *Data
	var value *Data

	for cell := bindingForms; NotNilP(cell); cell = Cdr(cell) {
		bindingTuple := Car(cell)
		name = First(bindingTuple)
		if NotNilP(Third(bindingTuple)) {
			value, err = Eval(Third(bindingTuple), env)
			if err != nil {
				return
			}
			env.BindLocallyTo(name, value)
		}
	}
	return
}

func DoImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if Length(args) < 2 {
		err = ProcessError("Do requires at least a list of bindings and a test clause", env)
		return
	}

	bindings := Car(args)
	if !PairP(bindings) {
		err = ProcessError("Do requires a list of bindings as it's first argument", env)
		return
	}

	testClause := Cadr(args)
	if !PairP(testClause) {
		err = ProcessError("Do requires a list as it's second argument", env)
		return
	}

	localEnv := NewSymbolTableFrameBelow(env)
	localEnv.Previous = env
	bindLetLocals(bindings, localEnv)

	body := Cddr(args)

	var shouldExit *Data

	for true {
		shouldExit, err = Eval(Car(testClause), localEnv)
		if err != nil {
			return
		}

		if BooleanValue(shouldExit) {
			for cell := Cdr(testClause); NotNilP(cell); cell = Cdr(cell) {
				sexpr := Car(cell)
				result, err = Eval(sexpr, localEnv)
				if err != nil {
					return
				}
			}
			return
		}

		for cell := body; NotNilP(cell); cell = Cdr(cell) {
			sexpr := Car(cell)
			result, err = Eval(sexpr, localEnv)
			if err != nil {
				return
			}
		}

		rebindDoLocals(bindings, localEnv)
	}
	return
}

func ApplyImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if Length(args) < 1 {
		err = ProcessError("apply requires at least one argument", env)
		return
	}

	f, err := Eval(Car(args), env)
	if err != nil {
		return
	}

	if !FunctionP(f) {
		err = ProcessError(fmt.Sprintf("apply requires a function as it's first argument, but got %s.", String(f)), env)
	}

	ary := make([]*Data, 0, Length(args)-1)

	var v *Data
	for c := Cdr(args); NotNilP(c); c = Cdr(c) {
		v, err = Eval(Car(c), env)
		if err != nil {
			return
		}
		ary = append(ary, v)
	}

	var argList *Data
	if ListP(ary[len(ary)-1]) {
		if len(ary) > 1 {
			argList = ArrayToListWithTail(ary[0:len(ary)-1], ary[len(ary)-1])
		} else {
			argList = ary[0]
		}
	} else {
		err = ProcessError("The last argument to apply must be a list", env)
		return
	}

	return Apply(f, argList, env)
}

func EvalImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	sexpr, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !ListP(sexpr) {
		err = ProcessError(fmt.Sprintf("eval expect a list argument, received a %s.", TypeName(TypeOf(sexpr))), env)
	}
	return Eval(sexpr, env)
}

func GlobalEvalImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	sexpr, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !ListP(sexpr) {
		err = ProcessError(fmt.Sprintf("eval expect a list argument, received a %s.", TypeName(TypeOf(sexpr))), env)
	}
	return Eval(sexpr, Global)
}

func ChainImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if Length(args) == 0 {
		err = ProcessError("-> requires at least an initial value.", env)
		return
	}

	var value *Data

	value, err = Eval(Car(args), env)
	if err != nil {
		return
	}

	for cell := Cdr(args); NotNilP(cell); cell = Cdr(cell) {
		sexpr := Car(cell)
		var newExpr *Data
		if ListP(sexpr) {
			newExpr = Cons(Car(sexpr), Cons(value, Cdr(sexpr)))
		} else {
			newExpr = Cons(sexpr, Cons(value, nil))
		}
		value, err = Eval(newExpr, env)
		if err != nil {
			return
		}
	}
	result = value
	return
}

func TapImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if Length(args) == 0 {
		err = ProcessError("tap requires at least an initial value.", env)
		return
	}

	var value *Data

	value, err = Eval(Car(args), env)
	if err != nil {
		return
	}
	result = value

	for cell := Cdr(args); NotNilP(cell); cell = Cdr(cell) {
		sexpr := Car(cell)
		var newExpr *Data
		if ListP(sexpr) {
			newExpr = Cons(Car(sexpr), Cons(value, Cdr(sexpr)))
		} else {
			newExpr = Cons(sexpr, Cons(value, nil))
		}
		_, err = Eval(newExpr, env)
		if err != nil {
			return
		}
	}
	return
}

func DefinitionOfImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var name *Data = nil
	if SymbolP(Car(args)) {
		name = Car(args)
	} else {
		name = SymbolWithName("anonymous")
	}

	f, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !FunctionP(f) {
		err = ProcessError(fmt.Sprintf("code requires a function argument, but received a %s.", TypeName(TypeOf(f))), env)
		return
	}

	function := FunctionValue(f)
	if function.Name == "anonymous" {
		return Cons(SymbolWithName("define"), Cons(name, Cons(Cons(SymbolWithName("lambda"), Cons(function.Params, function.Body)), nil))), nil
	} else {
		return Cons(SymbolWithName("define"), Cons(Cons(SymbolWithName(function.Name), function.Params), function.Body)), nil
	}
}
