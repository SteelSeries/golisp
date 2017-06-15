// Copyright 2015 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the environment frame functions/forms.

package golisp

import ()

func RegisterEnvironmentPrimitives() {
	MakePrimitiveFunction("environment?", "1", environmentPImpl)
	MakeTypedPrimitiveFunction("environment-has-parent?", "1", environmentParentPImpl, []uint32{EnvironmentType})
	MakeTypedPrimitiveFunction("environment-parent", "1", environmentParentImpl, []uint32{EnvironmentType})
	MakeTypedPrimitiveFunction("environment-bound-names", "1", environmentBoundNamesImpl, []uint32{EnvironmentType})
	MakeTypedPrimitiveFunction("environment-macro-names", "1", environmentMacroNamesImpl, []uint32{EnvironmentType})
	MakeTypedPrimitiveFunction("environment-bindings", "1", environmentBindingsImpl, []uint32{EnvironmentType})
	MakeTypedPrimitiveFunction("environment-reference-type", "2", environmentReferenceTypeImpl, []uint32{EnvironmentType, SymbolType})
	MakeTypedPrimitiveFunction("environment-bound?", "2", environmentBoundPImpl, []uint32{EnvironmentType, SymbolType})
	MakeTypedPrimitiveFunction("environment-assigned?", "2", environmentAssignedPImpl, []uint32{EnvironmentType, SymbolType})
	MakeTypedPrimitiveFunction("environment-lookup", "2", environmentLookupImpl, []uint32{EnvironmentType, SymbolType})
	MakeTypedPrimitiveFunction("environment-lookup-macro", "2", environmentLookupMacroImpl, []uint32{EnvironmentType, SymbolType})
	MakeTypedPrimitiveFunction("environment-assignable?", "2", environmentAssignablePImpl, []uint32{EnvironmentType, SymbolType})
	MakeTypedPrimitiveFunction("environment-assign!", "3", environmentAssignBangImpl, []uint32{EnvironmentType, SymbolType})
	MakeTypedPrimitiveFunction("environment-definable?", "2", environmentDefinablePImpl, []uint32{EnvironmentType, SymbolType})
	MakeTypedPrimitiveFunction("environment-define", "3", environmentDefineImpl, []uint32{EnvironmentType, SymbolType, AnyType})
	MakePrimitiveFunction("system-global-environment", "0", systemGlobalEnvironmentImpl)
	MakePrimitiveFunction("the-environment", "0", theEnvironmentImpl)
	MakeTypedPrimitiveFunction("procedure-environment", "1", procedureEnvironmentImpl, []uint32{FunctionType})
	MakeTypedPrimitiveFunction("make-top-level-environment", "1|2|3", makeTopLevelEnvironmentImpl, []uint32{StringType | ConsCellType, ConsCellType})
	MakeTypedPrimitiveFunction("find-top-level-environment", "1", findTopLevelEnvironmentImpl, []uint32{StringType | SymbolType})
}

func environmentPImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(EnvironmentP(Car(args))), nil
}

func environmentParentPImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	e := EnvironmentValue(First(args))
	return BooleanWithValue(e.Parent != nil), nil
}

func environmentParentImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	e := EnvironmentValue(First(args))
	if e.Parent == nil {
		return
	} else {
		return EnvironmentWithValue(e.Parent), nil
	}
}

func environmentBoundNamesImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	e := EnvironmentValue(First(args))
	keys := make([]*Data, 0, 0)
	for _, val := range e.Bindings {
		keys = append(keys, val.Sym)
	}
	return ArrayToList(keys), nil
}

func environmentMacroNamesImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	e := EnvironmentValue(First(args))
	keys := make([]*Data, 0, 0)
	for _, val := range e.Bindings {
		if MacroP(val.Val) {
			keys = append(keys, val.Sym)
		}
	}
	return ArrayToList(keys), nil
}

func environmentBindingsImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	e := EnvironmentValue(First(args))
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

func environmentReferenceTypeImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	localEnv := EnvironmentValue(First(args))
	binding, found := localEnv.FindBindingFor(Second(args))
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

func environmentBoundPImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	localEnv := EnvironmentValue(First(args))
	_, found := localEnv.FindBindingFor(Second(args))
	return BooleanWithValue(found), nil
}

func environmentAssignedPImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	localEnv := EnvironmentValue(First(args))
	binding, found := localEnv.FindBindingFor(Second(args))
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

func environmentLookupImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	localEnv := EnvironmentValue(First(args))
	binding, found := localEnv.FindBindingFor(Second(args))
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

func environmentLookupMacroImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	localEnv := EnvironmentValue(First(args))
	binding, found := localEnv.FindBindingFor(Second(args))
	if found && MacroP(binding.Val) {
		result = binding.Val
	} else {
		result = LispFalse
	}
	return
}

func environmentAssignablePImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	localEnv := EnvironmentValue(First(args))
	_, found := localEnv.FindBindingFor(Second(args))
	return BooleanWithValue(found), nil
}

func environmentAssignBangImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	localEnv := EnvironmentValue(First(args))
	binding, found := localEnv.FindBindingFor(Second(args))
	if found {
		result = Caddr(args)
		binding.Val = result
	}
	return
}

func environmentDefinablePImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return LispTrue, nil
}

func environmentDefineImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	EnvironmentValue(First(args)).BindLocallyTo(Second(args), Third(args))
	return Third(args), nil
}

func systemGlobalEnvironmentImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return EnvironmentWithValue(Global), nil
}

func theEnvironmentImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if env == Global || env.Parent == Global {
		return EnvironmentWithValue(env), nil
	} else {
		err = ProcessError("the-environment can only be called from a top-level environment", env)
		return
	}
}

func procedureEnvironmentImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return EnvironmentWithValue(FunctionValue(First(args)).Env), nil
}

func makeTopLevelEnvironmentImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	var name string

	if StringP(Car(args)) {
		name = StringValue(Car(args))
		args = Cdr(args)
	} else {
		name = "anonymous top level"
	}
	newEnv := NewSymbolTableFrameBelow(Global, name)
	if Length(args) == 1 {
		if !ListP(First(args)) {
			err = ProcessError("make-top-level-environment expects binding names to be a list", env)
			return
		}
		for cell := First(args); NotNilP(cell); cell = Cdr(cell) {
			if !SymbolP(Car(cell)) {
				err = ProcessError("make-top-level-environment expects binding names to be symbols", env)
				return
			}
			newEnv.BindLocallyTo(Car(cell), nil)
		}
	} else if Length(args) == 2 {
		if !ListP(First(args)) {
			err = ProcessError("make-top-level-environment expects binding names to be a list", env)
			return
		}
		if !ListP(Second(args)) {
			err = ProcessError("make-top-level-environment expects binding values to be a list", env)
			return
		}
		if Length(First(args)) != Length(Second(args)) {
			err = ProcessError("make-top-level-environment expects binding names and values lists to be the same length", env)
			return
		}
		for cell, valcell := First(args), Second(args); NotNilP(cell); cell, valcell = Cdr(cell), Cdr(valcell) {
			if !SymbolP(Car(cell)) {
				err = ProcessError("make-top-level-environment expects binding names to be symbols", env)
				return
			}
			newEnv.BindLocallyTo(Car(cell), Car(valcell))
		}
	}

	return EnvironmentWithValue(newEnv), nil
}

func findTopLevelEnvironmentImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	TopLevelEnvironments.Mutex.RLock()
	defer TopLevelEnvironments.Mutex.RUnlock()
	e := TopLevelEnvironments.Environments[StringValue(First(args))]
	if e != nil {
		result = EnvironmentWithValue(e)
	}
	return
}
