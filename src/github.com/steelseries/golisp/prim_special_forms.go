// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the built-in primitive functions.

package golisp

import (
    "errors"
    "fmt"
)

func RegisterSpecialFormPrimitives() {
    MakePrimitiveFunction("cond", -1, CondImpl)
    MakePrimitiveFunction("case", -1, CaseImpl)
    MakePrimitiveFunction("if", -1, IfImpl)
    MakePrimitiveFunction("lambda", -1, LambdaImpl)
    MakePrimitiveFunction("define", -1, DefineImpl)
    MakePrimitiveFunction("defun", -1, DefunImpl)
    MakePrimitiveFunction("defmacro", -1, DefmacroImpl)
    MakePrimitiveFunction("map", 2, MapImpl)
    MakePrimitiveFunction("let", -1, LetImpl)
    MakePrimitiveFunction("begin", -1, BeginImpl)
    MakePrimitiveFunction("do", -1, DoImpl)
    MakePrimitiveFunction("apply", 2, ApplyImpl)
    MakePrimitiveFunction("eval", 1, EvalImpl)
    MakePrimitiveFunction("->", -1, ChainImpl)
    MakePrimitiveFunction("=>", -1, TapImpl)
}

func CondImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var condition *Data
    for c := args; NotNilP(c); c = Cdr(c) {
        clause := Car(c)
        if !PairP(clause) {
            err = errors.New("Cond expect a sequence of clauses that are lists")
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
    var targetValue *Data

    keyValue, err = Eval(Car(args), env)
    if err != nil {
        return
    }

    for clauseCell := Cdr(args); NotNilP(clauseCell); clauseCell = Cdr(clauseCell) {
        clause := Car(clauseCell)
        if PairP(clause) {
            if IsEqual(Car(clause), SymbolWithName("else")) {
                return evalList(Cdr(clause), env)
            } else {
                targetValue, err = Eval(Car(clause), env)
                if IsEqual(targetValue, keyValue) {
                    return evalList(Cdr(clause), env)
                }
            }
        } else {
            err = errors.New("Case requires non-atomic clauses")
            return
        }
    }

    return
}

func IfImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    if Length(args) < 2 || Length(args) > 3 {
        err = errors.New(fmt.Sprintf("IF requires 2 or 3 arguments. Received %d.", Length(args)))
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
            err = errors.New("Function name has to be a symbol")
            return
        }
        body := Cdr(args)
        value = FunctionWithNameParamsBodyAndParent(StringValue(name), params, body, env)
    } else {
        err = errors.New("Invalid definition")
        return
    }
    env.BindLocallyTo(thing, value)
    return value, nil
}

func DefunImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    var value *Data
    thing := Car(args)
    if PairP(thing) {
        name := Car(thing)
        params := Cdr(thing)
        thing = name
        if !SymbolP(name) {
            err = errors.New("Function name has to be a symbol")
            return
        }
        body := Cdr(args)
        value = FunctionWithNameParamsBodyAndParent(StringValue(name), params, body, env)
    } else {
        err = errors.New("Invalid function definition")
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
            err = errors.New("Macro name has to be a symbol")
            return
        }
        body := Cadr(args)
        value = MacroWithNameParamsBodyAndParent(StringValue(name), params, body, env)
    } else {
        err = errors.New("Invalid macro definition")
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
            err = errors.New("Let requires a list of bindings (with are pairs) as it's first argument")
            return
        }
        name = Car(bindingPair)
        if !SymbolP(name) {
            err = errors.New("First part of a let binding pair must be a symbol")
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
        err = errors.New("Let requires at least a list of bindings")
        return
    }

    if !PairP(Car(args)) {
        err = errors.New("Let requires a list of bindings as it's first argument")
        return
    }

    localFrame := NewSymbolTableFrameBelow(env)
    bindLetLocals(Car(args), localFrame)

    for cell := Cdr(args); NotNilP(cell); cell = Cdr(cell) {
        sexpr := Car(cell)
        result, err = Eval(sexpr, localFrame)
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
        err = errors.New("Do requires at least a list of bindings and a test clause")
        return
    }

    bindings := Car(args)
    if !PairP(bindings) {
        err = errors.New("Do requires a list of bindings as it's first argument")
        return
    }

    testClause := Cadr(args)
    if !PairP(testClause) {
        err = errors.New("Do requires a list as it's second argument")
        return
    }

    localFrame := NewSymbolTableFrameBelow(env)
    bindLetLocals(bindings, localFrame)

    body := Cddr(args)

    var shouldExit *Data

    for true {
        shouldExit, err = Eval(Car(testClause), localFrame)
        if err != nil {
            return
        }

        if BooleanValue(shouldExit) {
            for cell := Cdr(testClause); NotNilP(cell); cell = Cdr(cell) {
                sexpr := Car(cell)
                result, err = Eval(sexpr, localFrame)
                if err != nil {
                    return
                }
            }
            return
        }

        for cell := body; NotNilP(cell); cell = Cdr(cell) {
            sexpr := Car(cell)
            result, err = Eval(sexpr, localFrame)
            if err != nil {
                return
            }
        }

        rebindDoLocals(bindings, localFrame)
    }
    return
}

func ApplyImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    f, err := Eval(Car(args), env)
    if err != nil {
        return
    }

    vals, err := Eval(Cadr(args), env)
    if err != nil {
        return
    }

    return Apply(f, vals, env)
}

func EvalImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    val, err := Eval(Car(args), env)
    if err != nil {
        return
    }

    return Eval(val, env)
}

func ChainImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
    if Length(args) == 0 {
        err = errors.New("-> requires at least an initial value.")
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
        err = errors.New("tap requires at least an initial value.")
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
