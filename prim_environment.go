// Copyright 2015 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the environment frame functions/forms.

package golisp

import ()

func RegisterEnvironmentPrimitives() {
	MakePrimitiveFunction("environment?", 1, EnvironmentPImpl)
	MakePrimitiveFunction("environment-has-parent?", 1, EnvironmentParentPImpl)
	MakePrimitiveFunction("environment-parent", 1, EnvironmentParentImpl)
	MakePrimitiveFunction("environment-bound-names", 1, EnvironmentBoundNamesImpl)
	MakePrimitiveFunction("environment-macro-names", 1, EnvironmentMacroNamesImpl)
	MakePrimitiveFunction("environment-bindings", 1, EnvironmentBindingsImpl)
	MakePrimitiveFunction("environment-reference-type", 2, EnvironmentReferenceTypeImpl)
	MakePrimitiveFunction("environment-bound?", 2, EnvironmentBoundPImpl)
	MakePrimitiveFunction("environment-assigned?", 2, EnvironmentAssignedPImpl)
	MakePrimitiveFunction("environment-lookup", 2, EnvironmentLookupImpl)
	MakePrimitiveFunction("environment-lookup-macro", 2, EnvironmentLookupMacroImpl)
	MakePrimitiveFunction("environment-assignable?", 2, EnvironmentAssignablePImpl)
	MakePrimitiveFunction("environment-assign!", 3, EnvironmentAssignBangImpl)
	MakePrimitiveFunction("environment-definable?", 2, EnvironmentDefinablePImpl)
	MakePrimitiveFunction("environment-define", 3, EnvironmentDefineImpl)
	MakePrimitiveFunction("system-global-environment", 0, SystemGlobalEnvironmentImpl)
	MakePrimitiveFunction("the-environment", 0, TheEnvironmentImpl)
	MakePrimitiveFunction("make-top-level-environment", -1, MakeTopLevelEnvironmentImpl)
	MakePrimitiveFunction("find-top-level-environment", 1, FindTopLevelEnvironmentImpl)
}

func EnvironmentPImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(EnvironmentP(Car(args))), nil
}

func EnvironmentParentPImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	eo, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !EnvironmentP(eo) {
		err = ProcessError("environment-has-parent? requires an environment as it's argument", env)
		return
	}
	e := EnvironmentValue(eo)
	return BooleanWithValue(e.Parent != nil), nil
}

func EnvironmentParentImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	eo, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !EnvironmentP(eo) {
		err = ProcessError("environment-parent requires an environment as it's argument", env)
		return
	}
	e := EnvironmentValue(eo)
	if e.Parent == nil {
		return
	} else {
		return EnvironmentWithValue(e.Parent), nil
	}
}

func EnvironmentBoundNamesImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	eo, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !EnvironmentP(eo) {
		err = ProcessError("environment-bound-names requires an environment as it's argument", env)
		return
	}
	e := EnvironmentValue(eo)
	keys := make([]*Data, 0, 0)
	for _, val := range e.Bindings {
		keys = append(keys, val.Sym)
	}
	return ArrayToList(keys), nil
}

func EnvironmentMacroNamesImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	eo, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !EnvironmentP(eo) {
		err = ProcessError("environment-macro-names requires an environment as it's argument", env)
		return
	}
	e := EnvironmentValue(eo)
	keys := make([]*Data, 0, 0)
	for _, val := range e.Bindings {
		if MacroP(val.Val) {
			keys = append(keys, val.Sym)
		}
	}
	return ArrayToList(keys), nil
}

func EnvironmentBindingsImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	eo, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !EnvironmentP(eo) {
		err = ProcessError("environment-bindings requires an environment as it's argument", env)
		return
	}
	e := EnvironmentValue(eo)
	keys := make([]*Data, 0, 0)
	for _, val := range e.Bindings {
		if NilP(val.Val) {
			keys = append(keys, InternalMakeList(val.Sym))
		} else {
			keys = append(keys, InternalMakeList(val.Sym, val.Val))
		}
	}
	return ArrayToList(keys), nil
}

func EnvironmentReferenceTypeImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	eo, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !EnvironmentP(eo) {
		err = ProcessError("environment-reference-type? requires an environment as it's first argument", env)
		return
	}

	sym, err := Eval(Cadr(args), env)
	if err != nil {
		return
	}
	if !SymbolP(Cadr(args)) {
		err = ProcessError("environment-reference-type? requires a symbol as it's second argument", env)
		return
	}

	localEnv := EnvironmentValue(eo)
	binding, found := localEnv.FindBindingFor(sym)
	if !found {
		result = Intern("unbound")
	} else if binding.Val == nil {
		result = Intern("unassigned")
	} else if MacroP(binding.Val) {
		result = Intern("macro")
	} else {
		result = Intern("normal")
	}

	return
}

func EnvironmentBoundPImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	eo, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !EnvironmentP(eo) {
		err = ProcessError("environment-bound? requires an environment as it's first argument", env)
		return
	}

	sym, err := Eval(Cadr(args), env)
	if err != nil {
		return
	}
	if !SymbolP(sym) {
		err = ProcessError("environment-bound? requires a symbol as it's second argument", env)
		return
	}

	localEnv := EnvironmentValue(eo)
	_, found := localEnv.FindBindingFor(sym)
	return BooleanWithValue(found), nil
}

func EnvironmentAssignedPImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	eo, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !EnvironmentP(eo) {
		err = ProcessError("environment-assigned? requires an environment as it's first argument", env)
		return
	}

	sym, err := Eval(Cadr(args), env)
	if err != nil {
		return
	}
	if !SymbolP(sym) {
		err = ProcessError("environment-assigned? requires a symbol as it's second argument", env)
		return
	}

	localEnv := EnvironmentValue(eo)
	binding, found := localEnv.FindBindingFor(sym)
	if found {
		if binding.Val == nil {
			result = LispFalse
		} else if MacroP(binding.Val) {
			err = ProcessError("environment-assigned?: name is bound to a macro", env)
			return
		} else {
			result = LispTrue
		}
	} else {
		err = ProcessError("environment-assigned?: name is unbound", env)
		return
	}
	return
}

func EnvironmentLookupImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	eo, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !EnvironmentP(eo) {
		err = ProcessError("environment-lookup requires an environment as it's first argument", env)
		return
	}

	sym, err := Eval(Cadr(args), env)
	if err != nil {
		return
	}
	if !SymbolP(sym) {
		err = ProcessError("environment-lookup requires a symbol as it's second argument", env)
		return
	}

	localEnv := EnvironmentValue(eo)
	binding, found := localEnv.FindBindingFor(sym)
	if found {
		if binding.Val == nil {
			err = ProcessError("environment-lookup: name is unassigned", env)
			return
		} else if MacroP(binding.Val) {
			err = ProcessError("environment-lookup: name is bound to a macro", env)
			return
		} else {
			return binding.Val, nil
		}
	} else {
		err = ProcessError("environment-lookup: name is unbound", env)
		return
	}
}

func EnvironmentLookupMacroImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	eo, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !EnvironmentP(eo) {
		err = ProcessError("environment-lookup-macro requires an environment as it's first argument", env)
		return
	}

	sym, err := Eval(Cadr(args), env)
	if err != nil {
		return
	}
	if !SymbolP(sym) {
		err = ProcessError("environment-lookup-macro requires a symbol as it's second argument", env)
		return
	}

	localEnv := EnvironmentValue(eo)
	binding, found := localEnv.FindBindingFor(sym)
	if found && MacroP(binding.Val) {
		result = binding.Val
	} else {
		result = LispFalse
	}
	return
}

func EnvironmentAssignablePImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	eo, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !EnvironmentP(eo) {
		err = ProcessError("environment-assignable? requires an environment as it's first argument", env)
		return
	}

	sym, err := Eval(Cadr(args), env)
	if err != nil {
		return
	}
	if !SymbolP(sym) {
		err = ProcessError("environment-assignable? requires a symbol as it's second argument", env)
		return
	}

	localEnv := EnvironmentValue(eo)
	_, found := localEnv.FindBindingFor(sym)
	return BooleanWithValue(found), nil
}

func EnvironmentAssignBangImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	eo, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !EnvironmentP(eo) {
		err = ProcessError("environment-assign! requires an environment as it's first argument", env)
		return
	}

	sym, err := Eval(Cadr(args), env)
	if err != nil {
		return
	}
	if !SymbolP(sym) {
		err = ProcessError("environment-assign! requires a symbol as it's second argument", env)
		return
	}

	value, err := Eval(Caddr(args), env)
	if err != nil {
		return
	}

	localEnv := EnvironmentValue(eo)
	binding, found := localEnv.FindBindingFor(sym)
	if found {
		result = value
		binding.Val = result
	}
	return
}

func EnvironmentDefinablePImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	eo, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !EnvironmentP(eo) {
		err = ProcessError("environment-definable? requires an environment as it's first argument", env)
		return
	}

	sym, err := Eval(Cadr(args), env)
	if err != nil {
		return
	}
	if !SymbolP(sym) {
		err = ProcessError("environment-definable? requires a symbol as it's second argument", env)
		return
	}

	return LispTrue, nil
}

func EnvironmentDefineImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	eo, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !EnvironmentP(eo) {
		err = ProcessError("environment-define requires an environment as it's first argument", env)
		return
	}

	sym, err := Eval(Cadr(args), env)
	if err != nil {
		return
	}
	if !SymbolP(sym) {
		err = ProcessError("environment-define requires a symbol as it's second argument", env)
		return
	}

	value, err := Eval(Caddr(args), env)
	if err != nil {
		return
	}

	EnvironmentValue(eo).BindLocallyTo(sym, value)
	return value, nil
}

func SystemGlobalEnvironmentImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return EnvironmentWithValue(Global), nil
}

func TheEnvironmentImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if env == Global || env.Parent == Global {
		return EnvironmentWithValue(env), nil
	} else {
		err = ProcessError("the-environment can only be called from a top-level environment", env)
		return
	}
}

func MakeTopLevelEnvironmentImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var name string

	n, err := Eval(Car(args), env)
	if err != nil {
		return
	}

	if StringP(n) {
		name = StringValue(n)
		args = Cdr(args)
	} else {
		name = "anonymous top level"
	}
	newEnv := NewSymbolTableFrameBelow(Global, name)
	if Length(args) > 2 {
		err = ProcessError("make-top-level-environment can have at most 3 arguments", env)
		return
	}
	if Length(args) == 1 {
		names, nerr := Eval(Car(args), env)
		if nerr != nil {
			return
		}
		if !ListP(names) {
			err = ProcessError("make-top-level-environment expects binding names to be a list", env)
			return
		}
		for cell := names; NotNilP(cell); cell = Cdr(cell) {
			if !SymbolP(Car(cell)) {
				err = ProcessError("make-top-level-environment expects binding names to be symbols", env)
				return
			}
			newEnv.BindLocallyTo(Car(cell), nil)
		}
	} else if Length(args) == 2 {
		names, nerr := Eval(Car(args), env)
		if nerr != nil {
			return
		}
		if !ListP(names) {
			err = ProcessError("make-top-level-environment expects binding names to be a list", env)
			return
		}
		values, verr := Eval(Cadr(args), env)
		if verr != nil {
			return
		}
		if !ListP(values) {
			err = ProcessError("make-top-level-environment expects binding values to be a list", env)
			return
		}
		if Length(names) != Length(values) {
			err = ProcessError("make-top-level-environment expects binding names and values lists to be the same length", env)
			return
		}
		for cell, valcell := names, values; NotNilP(cell); cell, valcell = Cdr(cell), Cdr(valcell) {
			if !SymbolP(Car(cell)) {
				err = ProcessError("make-top-level-environment expects binding names to be symbols", env)
				return
			}
			newEnv.BindLocallyTo(Car(cell), Car(valcell))
		}
	}

	return EnvironmentWithValue(newEnv), nil
}

func FindTopLevelEnvironmentImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	name, err := Eval(Car(args), env)
	if err != nil {
		return
	}

	if !StringP(name) && !SymbolP(name) {
		err = ProcessError("find-top-level-environment expects a symbol or string environment name", env)
		return
	}
	e := TopLevelEnvironments[StringValue(name)]
	if e == nil {
		return nil, nil
	} else {
		return EnvironmentWithValue(e), nil
	}
}
