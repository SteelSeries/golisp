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
	MakePrimitiveFunction("environment-define-macro", 3, EnvironmentDefineMacroImpl)
	MakePrimitiveFunction("system-global-environment", 0, SystemGlobalEnvironmentImpl)
	MakePrimitiveFunction("the-environment", 0, TheEnvironmentImpl)
	MakePrimitiveFunction("make-top-level-environment", -1, MakeTopLevelEnvironmentImpl)
}

func EnvironmentPImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(EnvironmentP(Car(args))), nil
}

func EnvironmentParentPImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if !EnvironmentP(Car(args)) {
		err = ProcessError("environment-has-parent? requires an environment as it's argument", env)
		return
	}
	e := EnvironmentValue(Car(args))
	return BooleanWithValue(e.Parent != nil), nil
}

func EnvironmentParentImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if !EnvironmentP(Car(args)) {
		err = ProcessError("environment-parent requires an environment as it's argument", env)
		return
	}
	e := EnvironmentValue(Car(args))
	if e.Parent == nil {
		return
	} else {
		return EnvironmentWithValue(e.Parent), nil
	}
}

func EnvironmentBoundNamesImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if !EnvironmentP(Car(args)) {
		err = ProcessError("environment-bound-names requires an environment as it's argument", env)
		return
	}
	e := EnvironmentValue(Car(args))
	keys := make([]*Data, 0, 0)
	for _, val := range e.Bindings {
		keys = append(keys, val.Sym)
	}
	return ArrayToList(keys), nil
}

func EnvironmentMacroNamesImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if !EnvironmentP(Car(args)) {
		err = ProcessError("environment-macro-names requires an environment as it's argument", env)
		return
	}
	e := EnvironmentValue(Car(args))
	keys := make([]*Data, 0, 0)
	for _, val := range e.Bindings {
		if MacroP(val.Val) {
			keys = append(keys, val.Sym)
		}
	}
	return ArrayToList(keys), nil
}

func EnvironmentBindingsImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if !EnvironmentP(Car(args)) {
		err = ProcessError("environment-bindings requires an environment as it's argument", env)
		return
	}
	e := EnvironmentValue(Car(args))
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
	if !EnvironmentP(Car(args)) {
		err = ProcessError("environment-has-parent? requires an environment as it's argument", env)
		return
	}
	return
}

func EnvironmentBoundPImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if !EnvironmentP(Car(args)) {
		err = ProcessError("environment-has-parent? requires an environment as it's argument", env)
		return
	}
	return
}

func EnvironmentAssignedPImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if !EnvironmentP(Car(args)) {
		err = ProcessError("environment-has-parent? requires an environment as it's argument", env)
		return
	}
	return
}

func EnvironmentLookupImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if !EnvironmentP(Car(args)) {
		err = ProcessError("environment-has-parent? requires an environment as it's argument", env)
		return
	}
	return
}

func EnvironmentLookupMacroImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if !EnvironmentP(Car(args)) {
		err = ProcessError("environment-has-parent? requires an environment as it's argument", env)
		return
	}
	return
}

func EnvironmentAssignablePImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if !EnvironmentP(Car(args)) {
		err = ProcessError("environment-has-parent? requires an environment as it's argument", env)
		return
	}
	return
}

func EnvironmentAssignBangImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if !EnvironmentP(Car(args)) {
		err = ProcessError("environment-has-parent? requires an environment as it's argument", env)
		return
	}
	return
}

func EnvironmentDefinablePImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if !EnvironmentP(Car(args)) {
		err = ProcessError("environment-has-parent? requires an environment as it's argument", env)
		return
	}
	return
}

func EnvironmentDefineImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if !EnvironmentP(Car(args)) {
		err = ProcessError("environment-has-parent? requires an environment as it's argument", env)
		return
	}
	return
}

func EnvironmentDefineMacroImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if !EnvironmentP(Car(args)) {
		err = ProcessError("environment-has-parent? requires an environment as it's argument", env)
		return
	}
	return
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

	if StringP(Car(args)) {
		name = StringValue(Car(args))
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
		if !ListP(Car(args)) {
			err = ProcessError("make-top-level-environment expects binding names to be a list", env)
			return
		}
		for cell := Car(args); NotNilP(cell); cell = Cdr(cell) {
			if !SymbolP(Car(cell)) {
				err = ProcessError("make-top-level-environment expects binding names to be symbols", env)
				return
			}
			newEnv.BindLocallyTo(Car(cell), nil)
		}
	} else if Length(args) == 2 {
		if !ListP(Car(args)) {
			err = ProcessError("make-top-level-environment expects binding names to be a list", env)
			return
		}
		if !ListP(Cadr(args)) {
			err = ProcessError("make-top-level-environment expects binding values to be a list", env)
			return
		}
		if Length(Car(args)) != Length(Cadr(args)) {
			err = ProcessError("make-top-level-environment expects binding names and values lists to be the same length", env)
			return
		}
		for cell, valcell := Car(args), Cadr(args); NotNilP(cell); cell, valcell = Cdr(cell), Cdr(valcell) {
			if !SymbolP(Car(cell)) {
				err = ProcessError("make-top-level-environment expects binding names to be symbols", env)
				return
			}
			newEnv.BindLocallyTo(Car(cell), Car(valcell))
		}
	}

	return EnvironmentWithValue(newEnv), nil
}
