// Copyright 2015 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the environment frame functions/forms.

package golisp

func RegisterEnvironmentPrimitives() {
	MakePrimitiveFunction("environment?", "1", EnvironmentPImpl)
	MakeTypedPrimitiveFunction("environment-has-parent?", "1", EnvironmentParentPImpl, []uint32{EnvironmentType})
	MakeTypedPrimitiveFunction("environment-parent", "1", EnvironmentParentImpl, []uint32{EnvironmentType})
	MakeTypedPrimitiveFunction("environment-bound-names", "1", EnvironmentBoundNamesImpl, []uint32{EnvironmentType})
	MakeTypedPrimitiveFunction("environment-macro-names", "1", EnvironmentMacroNamesImpl, []uint32{EnvironmentType})
	MakeTypedPrimitiveFunction("environment-bindings", "1", EnvironmentBindingsImpl, []uint32{EnvironmentType})
	MakeTypedPrimitiveFunction("environment-reference-type", "2", EnvironmentReferenceTypeImpl, []uint32{EnvironmentType, SymbolType})
	MakeTypedPrimitiveFunction("environment-bound?", "2", EnvironmentBoundPImpl, []uint32{EnvironmentType, SymbolType})
	MakeTypedPrimitiveFunction("environment-assigned?", "2", EnvironmentAssignedPImpl, []uint32{EnvironmentType, SymbolType})
	MakeTypedPrimitiveFunction("environment-lookup", "2", EnvironmentLookupImpl, []uint32{EnvironmentType, SymbolType})
	MakeTypedPrimitiveFunction("environment-lookup-macro", "2", EnvironmentLookupMacroImpl, []uint32{EnvironmentType, SymbolType})
	MakeTypedPrimitiveFunction("environment-assignable?", "2", EnvironmentAssignablePImpl, []uint32{EnvironmentType, SymbolType})
	MakeTypedPrimitiveFunction("environment-assign!", "3", EnvironmentAssignBangImpl, []uint32{EnvironmentType, SymbolType})
	MakeTypedPrimitiveFunction("environment-definable?", "2", EnvironmentDefinablePImpl, []uint32{EnvironmentType, SymbolType})
	MakeTypedPrimitiveFunction("environment-define", "3", EnvironmentDefineImpl, []uint32{EnvironmentType, SymbolType, AnyType})
	MakePrimitiveFunction("system-global-environment", "0", SystemGlobalEnvironmentImpl)
	MakePrimitiveFunction("the-environment", "0", TheEnvironmentImpl)
	MakeTypedPrimitiveFunction("procedure-environment", "1", ProcedureEnvironmentImpl, []uint32{FunctionType})
	MakeTypedPrimitiveFunction("make-top-level-environment", "1|2|3", MakeTopLevelEnvironmentImpl, []uint32{StringType | ConsCellType, ConsCellType})
	MakeTypedPrimitiveFunction("find-top-level-environment", "1", FindTopLevelEnvironmentImpl, []uint32{StringType | SymbolType})
}

func EnvironmentPImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return BooleanWithValue(EnvironmentP(Car(args))), nil
}

func EnvironmentParentPImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	e := EnvironmentValue(First(args))
	return BooleanWithValue(e.Parent != nil), nil
}

func EnvironmentParentImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	e := EnvironmentValue(First(args))
	if e.Parent == nil {
		return
	} else {
		return EnvironmentWithValue(e.Parent), nil
	}
}

func EnvironmentBoundNamesImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	e := EnvironmentValue(First(args))
	keys := make([]*Data, 0, 0)
	for _, val := range e.Bindings {
		keys = append(keys, val.Sym)
	}
	return ArrayToList(keys), nil
}

func EnvironmentMacroNamesImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	e := EnvironmentValue(First(args))
	keys := make([]*Data, 0, 0)
	for _, val := range e.Bindings {
		if MacroP(val.Val) {
			keys = append(keys, val.Sym)
		}
	}
	return ArrayToList(keys), nil
}

func EnvironmentBindingsImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func EnvironmentReferenceTypeImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func EnvironmentBoundPImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	localEnv := EnvironmentValue(First(args))
	_, found := localEnv.FindBindingFor(Second(args))
	return BooleanWithValue(found), nil
}

func EnvironmentAssignedPImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func EnvironmentLookupImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
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

func EnvironmentLookupMacroImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	localEnv := EnvironmentValue(First(args))
	binding, found := localEnv.FindBindingFor(Second(args))
	if found && MacroP(binding.Val) {
		result = binding.Val
	} else {
		result = LispFalse
	}
	return
}

func EnvironmentAssignablePImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	localEnv := EnvironmentValue(First(args))
	_, found := localEnv.FindBindingFor(Second(args))
	return BooleanWithValue(found), nil
}

func EnvironmentAssignBangImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	localEnv := EnvironmentValue(First(args))
	binding, found := localEnv.FindBindingFor(Second(args))
	if found {
		result = Caddr(args)
		binding.Val = result
	}
	return
}

func EnvironmentDefinablePImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return LispTrue, nil
}

func EnvironmentDefineImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	_, err = EnvironmentValue(First(args)).BindLocallyTo(Second(args), Third(args))
	return Third(args), err
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

func ProcedureEnvironmentImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return EnvironmentWithValue(FunctionValue(First(args)).Env), nil
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
			_, err = newEnv.BindLocallyTo(Car(cell), nil)
			if err != nil {
				return
			}
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
			_, err = newEnv.BindLocallyTo(Car(cell), Car(valcell))
			if err != nil {
				return
			}
		}
	}

	return EnvironmentWithValue(newEnv), nil
}

func FindTopLevelEnvironmentImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	TopLevelEnvironments.Mutex.RLock()
	defer TopLevelEnvironments.Mutex.RUnlock()
	e := TopLevelEnvironments.Environments[StringValue(First(args))]
	if e != nil {
		result = EnvironmentWithValue(e)
	}
	return
}
