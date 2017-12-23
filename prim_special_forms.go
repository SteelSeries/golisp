// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpreter for embedding in a go program for scripting.
// This file contains the built-in primitive functions.

package golisp

import (
	"fmt"
)

func RegisterSpecialFormPrimitives() {
	MakeSpecialForm("cond", "*", CondImpl)
	MakeSpecialForm("case", ">=1", CaseImpl)
	MakeSpecialForm("if", "2|3", IfImpl)
	MakeSpecialForm("when", ">=2", WhenImpl)
	MakeSpecialForm("unless", ">=2", UnlessImpl)
	MakeSpecialForm("lambda", ">=1", LambdaImpl)
	MakeSpecialForm("named-lambda", ">=1", NamedLambdaImpl)
	MakeSpecialForm("define", ">=1", DefineImpl)
	MakeSpecialForm("defmacro", ">=1", DefmacroImpl)
	MakeSpecialForm("let", ">=1", LetImpl)
	MakeSpecialForm("let*", ">=1", LetStarImpl)
	MakeSpecialForm("letrec", ">=1", LetRecImpl)
	MakeSpecialForm("begin", "*", BeginImpl)
	MakeSpecialForm("do", ">=2", DoImpl)
	MakePrimitiveFunction("apply", ">=1", ApplyImpl)
	MakeSpecialForm("->", ">=1", ChainImpl)
	MakeSpecialForm("=>", ">=1", TapImpl)
	MakeSpecialForm("definition-of", "1", DefinitionOfImpl)
}

func evaluateBody(sexprs *Data, env *SymbolTableFrame) (result *Data, err error) {
	for e := sexprs; NotNilP(e); e = Cdr(e) {
		result, err = Eval(Car(e), env)
		if err != nil {
			return
		}
	}
	return
}

func CondImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var condition *Data
	for c := args; NotNilP(c); c = Cdr(c) {
		clause := Car(c)
		if !PairP(clause) {
			err = ProcessError("Cond expect a sequence of clauses that are lists", env)
			return
		}
		if IsEqual(Car(clause), Intern("else")) {
			return evaluateBody(Cdr(clause), env)
		} else {
			condition, err = Eval(Car(clause), env)
			if err != nil {
				return
			}
			if BooleanValue(condition) {
				return evaluateBody(Cdr(clause), env)
			}
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
			err = ProcessError("Case expectes a sequence of clauses that are lists", env)
			return
		}
		if IsEqual(Car(clause), Intern("else")) {
			return evaluateBody(Cdr(clause), env)
		} else if ListP(Car(clause)) {
			for v := Car(clause); NotNilP(v); v = Cdr(v) {
				if IsEqual(Car(v), keyValue) {
					return evaluateBody(Cdr(clause), env)
				}
			}
		} else {
			err = ProcessError("Case the condition part of clauses to be lists of 'else", env)
			return
		}
	}

	return
}

func IfImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	c, err := Eval(Car(args), env)
	if err != nil {
		return
	}

	if BooleanValue(c) {
		return Eval(Second(args), env)
	} else {
		return Eval(Third(args), env)
	}
}

func WhenImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	c, err := Eval(Car(args), env)
	if err != nil {
		return
	}

	if BooleanValue(c) {
		for cell := Cdr(args); NotNilP(cell); cell = Cdr(cell) {
			sexpr := Car(cell)
			result, err = Eval(sexpr, env)
			if err != nil {
				return
			}
		}
	}
	return
}

func UnlessImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	c, err := Eval(Car(args), env)
	if err != nil {
		return
	}

	if !BooleanValue(c) {
		for cell := Cdr(args); NotNilP(cell); cell = Cdr(cell) {
			sexpr := Car(cell)
			result, err = Eval(sexpr, env)
			if err != nil {
				return
			}
		}
	}
	return
}

func LambdaImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if !PairP(Car(args)) {
		err = ProcessError("A lambda requires a parameter list", env)
		return
	}
	params := Car(args)
	body := Cdr(args)
	return FunctionWithNameParamsBodyAndParent("unnamed", params, body, env), nil
}

func NamedLambdaImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if !PairP(Car(args)) {
		err = ProcessError("A lambda requires a name/parameter list", env)
		return
	}
	name := Caar(args)
	if !SymbolP(name) {
		err = ProcessError("A named lambda requires a name that is a symbol", env)
		return
	}
	params := Cdar(args)
	body := Cdr(args)
	return FunctionWithNameParamsBodyAndParent(StringValue(name), params, body, env), nil
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
		existingValueOrNil := env.ValueOf(name)
		if PrimitiveP(existingValueOrNil) {
			err = ProcessError(fmt.Sprintf("Primitive function %s can not be redefined.", StringValue(name)), env)
			return
		}
		body := Cdr(args)
		value = FunctionWithNameParamsBodyAndParent(StringValue(name), params, body, env)
	} else {
		err = ProcessError("Invalid definition", env)
		return
	}
	_, err = env.BindLocallyTo(thing, value)
	return value, err
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
	_, err = env.BindLocallyTo(thing, value)
	return value, err
}

func bindLetLocals(bindingForms *Data, rec bool, localEnv *SymbolTableFrame, evalEnv *SymbolTableFrame) (err error) {
	var name *Data
	var value *Data

	for cell := bindingForms; NotNilP(cell); cell = Cdr(cell) {
		bindingPair := Car(cell)
		if !PairP(bindingPair) {
			err = ProcessError("Let requires a list of bindings (with are pairs) as it's first argument", evalEnv)
			return
		}
		name = Car(bindingPair)
		if !SymbolP(name) {
			err = ProcessError("First part of a let binding pair must be a symbol", evalEnv)
			return
		}

		if rec {
			_, err = localEnv.BindLocallyTo(name, nil)
			if err != nil {
				return
			}
		}
	}

	for cell := bindingForms; NotNilP(cell); cell = Cdr(cell) {
		bindingPair := Car(cell)
		name = Car(bindingPair)
		value, err = Eval(Cadr(bindingPair), evalEnv)
		if err != nil {
			return
		}
		_, err = localEnv.BindLocallyTo(name, value)
		if err != nil {
			return
		}
	}
	return
}

func LetCommon(args *Data, env *SymbolTableFrame, star bool, rec bool) (result *Data, err error) {
	if !PairP(Car(args)) {
		err = ProcessError("Let requires a list of bindings as it's first argument", env)
		return
	}

	localEnv := NewSymbolTableFrameBelow(env, "let")
	localEnv.Previous = env
	var evalEnv *SymbolTableFrame
	if star || rec {
		evalEnv = localEnv
	} else {
		evalEnv = env
	}
	err = bindLetLocals(Car(args), rec, localEnv, evalEnv)
	if err != nil {
		return
	}

	for cell := Cdr(args); NotNilP(cell); cell = Cdr(cell) {
		sexpr := Car(cell)
		result, err = Eval(sexpr, localEnv)
		if err != nil {
			return
		}
	}

	return
}

func namedLetImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	name := Car(args)
	if !SymbolP(name) {
		err = ProcessError("A named let requires a symbol name as its first argument", env)
		return
	}

	bindings := Cadr(args)
	if !PairP(bindings) {
		err = ProcessError("A named let requires a list of bindings as it's second argument", env)
		return
	}
	body := Cddr(args)
	vars := make([]*Data, 0, Length(bindings))
	initials := make([]*Data, 0, Length(bindings))
	for remainingBindings := bindings; NotNilP(remainingBindings); remainingBindings = Cdr(remainingBindings) {
		binding := Car(remainingBindings)
		if !SymbolP(Car(binding)) {
			err = ProcessError("The first element of a binding must be a symbol", env)
			return
		}
		vars = append(vars, Car(binding))
		initials = append(initials, Cadr(binding))
	}
	varsList := ArrayToList(vars)
	initialsList := ArrayToList(initials)
	localEnv := NewSymbolTableFrameBelow(env, StringValue(name))
	localEnv.Previous = env
	_, err = localEnv.BindLocallyTo(name, nil)
	if err != nil {
		return
	}
	namedLetProc := FunctionWithNameParamsBodyAndParent(StringValue(name), varsList, body, localEnv)
	_, err = localEnv.BindLocallyTo(name, namedLetProc)
	if err != nil {
		return
	}
	return FunctionValue(namedLetProc).Apply(initialsList, env)
}

func LetImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if SymbolP(Car(args)) {
		return namedLetImpl(args, env)
	} else {
		return LetCommon(args, env, false, false)
	}
}

func LetStarImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return LetCommon(args, env, true, false)
}

func LetRecImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return LetCommon(args, env, false, true)
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
	var names []*Data
	var values []*Data
	var value *Data
	var name *Data

	for cell := bindingForms; NotNilP(cell); cell = Cdr(cell) {
		bindingTuple := Car(cell)
		name = First(bindingTuple)
		names = append(names, name)
		if NotNilP(Third(bindingTuple)) {
			value, err = Eval(Third(bindingTuple), env)
			if err != nil {
				return
			}
		} else {
			value = env.ValueOf(name)
		}
		values = append(values, value)
	}

	for i := 0; i < len(names); i++ {
		_, err = env.BindLocallyTo(names[i], values[i])
		if err != nil {
			return
		}
	}
	return
}

func DoImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

	localEnv := NewSymbolTableFrameBelow(env, "do")
	localEnv.Previous = env
	err = bindLetLocals(bindings, false, localEnv, env)
	if err != nil {
		return
	}

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

		if rebindDoLocals(bindings, localEnv) != nil {
			return
		}
	}
	return
}

func ApplyImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := Car(args)

	if !FunctionOrPrimitiveP(f) {
		err = ProcessError(fmt.Sprintf("apply requires a function as it's first argument, but got %s.", String(f)), env)
		return
	}

	ary := ToArray(Cdr(args))
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

	return ApplyWithoutEval(f, argList, env)
}

func ChainImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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
		name = Intern("anonymous")
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
	if function.Name == "unnamed" {
		return Cons(Intern("define"), Cons(name, Cons(Cons(Intern("lambda"), Cons(function.Params, function.Body)), nil))), nil
	} else {
		return Cons(Intern("define"), Cons(Cons(Intern(function.Name), function.Params), function.Body)), nil
	}
}
