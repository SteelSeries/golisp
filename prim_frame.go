// Copyright "2"014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the frame primitive functions.

package golisp

import (
	"fmt"
)

func RegisterFramePrimitives() {
	MakePrimitiveFunction("make-frame", "*", MakeFrameImpl)
	MakePrimitiveFunction("has-slot?", "2", HasSlotImpl)
	MakePrimitiveFunction("get-slot", "2", GetSlotImpl)
	MakePrimitiveFunction("get-slot-or-nil", "2", GetSlotOrNilImpl)
	MakePrimitiveFunction("remove-slot!", "2", RemoveSlotImpl)
	MakePrimitiveFunction("set-slot!", "3", SetSlotImpl)
	MakePrimitiveFunction("send", ">=2", SendImpl)
	MakePrimitiveFunction("send-super", ">=1", SendSuperImpl)
	MakeSpecialForm("apply-slot", ">=3", ApplySlotImpl)
	MakeSpecialForm("apply-slot-super", ">=2", ApplySlotSuperImpl)
	MakePrimitiveFunction("clone", "1", CloneImpl)
	MakePrimitiveFunction("json->lisp", "1", JsonToLispImpl)
	MakePrimitiveFunction("lisp->json", "1", LispToJsonImpl)
	MakePrimitiveFunction("frame-keys", "1", FrameKeysImpl)
	MakePrimitiveFunction("frame-values", "1", FrameValuesImpl)
}

func MakeFrameImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if Length(args)%2 != 0 {
		err = ProcessError("Frames must be initialized with an even number of arguments.", env)
		return
	}
	m := FrameMap{}
	m.Data = make(FrameMapData)
	for c := args; NotNilP(c); c = Cddr(c) {
		k := Car(c)
		if !NakedP(k) {
			err = ProcessError(fmt.Sprintf("Frame keys must be naked symbols, but was given %s.", String(k)), env)
			return
		}
		v := Cadr(c)
		m.Data[StringValue(k)] = v
	}
	return FrameWithValue(&m), nil
}

func HasSlotImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := Car(args)
	if !FrameP(f) {
		err = ProcessError(fmt.Sprintf("has-slot? requires a frame as it's first argument, but was given %s.", String(f)), env)
		return
	}

	k := Cadr(args)
	if !NakedP(k) {
		err = ProcessError(fmt.Sprintf("has-slot? requires a naked symbol as it's second argument, but was given %s.", String(k)), env)
		return
	}

	return BooleanWithValue(FrameValue(f).HasSlot(StringValue(k))), nil
}

func GetSlotImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := Car(args)
	if !FrameP(f) {
		err = ProcessError(fmt.Sprintf("get-slot requires a frame as it's first argument, but was given %s.", String(f)), env)
		return
	}

	if FrameValue(f) == nil {
		err = ProcessError("get-slot received a nil frame.", env)
		return
	}

	k := Cadr(args)
	if !NakedP(k) {
		err = ProcessError(fmt.Sprintf("get-slot requires a naked symbol as it's second argument, but was given %s.", String(k)), env)
		return
	}

	if !FrameValue(f).HasSlot(StringValue(k)) {
		err = ProcessError(fmt.Sprintf("get-slot requires an existing slot, but was given %s.", String(k)), env)
		return
	}

	return FrameValue(f).Get(StringValue(k)), nil
}

func GetSlotOrNilImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := Car(args)
	if !FrameP(f) {
		err = ProcessError(fmt.Sprintf("get-slot-or-nil requires a frame as it's first argument, but was given %s.", String(f)), env)
		return
	}

	k := Cadr(args)
	if !NakedP(k) {
		err = ProcessError(fmt.Sprintf("get-slot-or-nil requires a naked symbol as it's second argument, but was given %s.", String(k)), env)
		return
	}

	return FrameValue(f).Get(StringValue(k)), nil
}

func RemoveSlotImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := Car(args)

	if NilP(f) {
		return LispFalse, nil
	}

	if !FrameP(f) {
		err = ProcessError(fmt.Sprintf("remove-slot! requires a frame as it's first argument, but was given %s.", String(f)), env)
		return
	}

	k := Cadr(args)
	if !NakedP(k) {
		err = ProcessError(fmt.Sprintf("remove-slot! requires a naked symbol as it's second argument, but was given %s.", String(k)), env)
		return
	}

	return BooleanWithValue(FrameValue(f).Remove(StringValue(k))), nil
}

func SetSlotImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := Car(args)
	if !FrameP(f) {
		err = ProcessError(fmt.Sprintf("set-slot! requires a frame as it's first argument, but was given %s.", String(f)), env)
		return
	}

	k := Cadr(args)
	if !NakedP(k) {
		err = ProcessError(fmt.Sprintf("set-slot! requires a naked symbol as it's second argument, but was given %s.", String(k)), env)
		return
	}

	v := Caddr(args)

	return FrameValue(f).Set(StringValue(k), v), nil
}

func SendImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := Car(args)
	if !FrameP(f) {
		err = ProcessError(fmt.Sprintf("send requires a frame as it's first argument, but was given %s.", String(f)), env)
		return
	}

	k := Cadr(args)
	if !NakedP(k) {
		err = ProcessError(fmt.Sprintf("send requires a naked symbol as it's second argument, but was given %s.", String(k)), env)
		return
	}

	if !FrameValue(f).HasSlot(StringValue(k)) {
		err = ProcessError(fmt.Sprintf("send requires an existing slot, but was given %s.", String(k)), env)
		return
	}

	fun := FrameValue(f).Get(StringValue(k))
	if !FunctionP(fun) {
		err = ProcessError(fmt.Sprintf("send requires a function slot, but was given a slot containing a %s.", TypeName(TypeOf(fun))), env)
		return
	}

	params := Cddr(args)
	return FunctionValue(fun).ApplyWithoutEvalWithFrame(params, env, FrameValue(f))
}

func getSuperFunction(selector string, env *SymbolTableFrame) *Data {
	f := env.Frame
	if f == nil {
		return nil
	}

	for _, p := range f.Parents() {
		fun := p.Get(selector)
		if fun != nil {
			return fun
		}
	}

	return nil
}

func SendSuperImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if !env.HasFrame() {
		err = ProcessError("send-super can only be used within the context of a frame.", env)
		return
	}

	selector := Car(args)
	if !NakedP(selector) {
		err = ProcessError(fmt.Sprintf("Selector must be a naked symbol but was %s.", TypeName(TypeOf(selector))), env)
		return
	}

	fun := getSuperFunction(StringValue(selector), env)
	if fun == nil || !FunctionP(fun) {
		err = ProcessError(fmt.Sprintf("Message sent must select a function slot but was %s.", TypeName(TypeOf(fun))), env)
		return
	}

	params := Cdr(args)
	frameEnv := NewSymbolTableFrameBelowWithFrame(env, env.Frame, fmt.Sprintf("%s'", env.Name))
	_, err = frameEnv.BindLocallyTo(Intern("self"), FrameWithValue(env.Frame))
	if err != nil {
		return
	}
	return FunctionValue(fun).ApplyWithoutEval(params, frameEnv)
}

func ApplySlotImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !FrameP(f) {
		err = ProcessError(fmt.Sprintf("apply-slot requires a frame as it's first argument, but was given %s.", String(f)), env)
		return
	}

	k, err := Eval(Cadr(args), env)
	if err != nil {
		return
	}
	if !NakedP(k) {
		err = ProcessError(fmt.Sprintf("apply-slot requires a naked symbol as it's second argument, but was given %s.", String(k)), env)
		return
	}

	if !FrameValue(f).HasSlot(StringValue(k)) {
		err = ProcessError(fmt.Sprintf("apply-slot requires an existing slot, but was given %s.", String(k)), env)
		return
	}

	fun := FrameValue(f).Get(StringValue(k))
	if !FunctionP(fun) {
		err = ProcessError(fmt.Sprintf("apply-slot requires a function slot, but was given a slot containing a %s.", TypeName(TypeOf(fun))), env)
		return
	}

	ary := make([]*Data, 0, Length(args)-2)

	var v *Data
	for c := Cddr(args); NotNilP(c); c = Cdr(c) {
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

	return FunctionValue(fun).ApplyWithoutEvalWithFrame(argList, env, FrameValue(f))
}

func ApplySlotSuperImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if !env.HasFrame() {
		err = ProcessError("apply-slot-super can only be used within the context of a frame.", env)
		return
	}

	selector, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !NakedP(selector) {
		err = ProcessError(fmt.Sprintf("Selector must be a naked symbol but was %s.", TypeName(TypeOf(selector))), env)
		return
	}

	fun := getSuperFunction(StringValue(selector), env)
	if fun == nil || !FunctionP(fun) {
		err = ProcessError(fmt.Sprintf("Message sent must select a function slot but was %s.", TypeName(TypeOf(fun))), env)
		return
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

	frameEnv := NewSymbolTableFrameBelowWithFrame(env, env.Frame, fmt.Sprintf("%s'", env.Name))
	_, err = frameEnv.BindLocallyTo(Intern("self"), FrameWithValue(env.Frame))
	if err != nil {
		return
	}
	return FunctionValue(fun).ApplyWithoutEval(argList, frameEnv)
}

func CloneImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := Car(args)
	if !FrameP(f) {
		err = ProcessError(fmt.Sprintf("clone requires a frame as it's argument, but was given %s.", String(f)), env)
		return
	}

	return FrameWithValue(FrameValue(f).Clone()), nil
}

func JsonToLispImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	j := Car(args)
	if !StringP(j) {
		err = ProcessError(fmt.Sprintf("json->lisp requires a string as it's argument, but was given %s.", String(j)), env)
		return
	}

	return JsonStringToLispWithFrames(StringValue(j)), nil
}

func LispToJsonImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l := Car(args)

	return StringWithValue(LispWithFramesToJsonString(l)), nil
}

func FrameKeysImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := Car(args)
	if !FrameP(f) {
		err = ProcessError(fmt.Sprintf("frame-keys requires a frame as it's argument, but was given %s.", String(f)), env)
		return
	}

	return ArrayToList(FrameValue(f).Keys()), nil
}

func FrameValuesImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := Car(args)
	if !FrameP(f) {
		err = ProcessError(fmt.Sprintf("frame-values requires a frame as it's argument, but was given %s.", String(f)), env)
		return
	}

	return ArrayToList(FrameValue(f).Values()), nil
}
