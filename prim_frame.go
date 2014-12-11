// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file contains the frame primitive functions.

package golisp

import (
	"fmt"
)

func RegisterFramePrimitives() {
	MakePrimitiveFunction("make-frame", -1, MakeFrameImpl)
	MakePrimitiveFunction("has-slot?", 2, HasSlotImpl)
	MakePrimitiveFunction("get-slot", 2, GetSlotImpl)
	MakePrimitiveFunction("get-slot-or-nil", 2, GetSlotOrNilImpl)
	MakePrimitiveFunction("remove-slot!", 2, RemoveSlotImpl)
	MakePrimitiveFunction("set-slot!", 3, SetSlotImpl)
	MakePrimitiveFunction("send", -1, SendImpl)
	MakePrimitiveFunction("send-super", -1, SendSuperImpl)
	MakePrimitiveFunction("clone", 1, CloneImpl)
	MakePrimitiveFunction("json->lisp", 1, JsonToLispImpl)
	MakePrimitiveFunction("lisp->json", 1, LispToJsonImpl)
	MakePrimitiveFunction("keys", 1, KeysImpl)
}

func MakeFrameImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if Length(args)%2 != 0 {
		err = ProcessError("Frames must be initialized with an even number of arguments.", env)
	}
	m := make(FrameMap)
	for c := args; NotNilP(c); c = Cddr(c) {
		k := Car(c)
		if !NakedP(k) {
			err = ProcessError(fmt.Sprintf("Frame keys must be naked symbols, but was given %s.", String(k)), env)
			return
		}
		v, e := Eval(Cadr(c), env)
		if e != nil {
			return nil, e
		}
		m[StringValue(k)] = v
	}
	return FrameWithValue(&m), nil
}

func HasSlotImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !FrameP(f) {
		err = ProcessError(fmt.Sprintf("has-slot? requires a frame as it's first argument, but was given %s.", String(f)), env)
	}

	k, err := Eval(Cadr(args), env)
	if err != nil {
		return
	}
	if !SymbolP(k) {
		err = ProcessError(fmt.Sprintf("has-slot? requires a symbol as it's second argument, but was given %s.", String(k)), env)
		return
	}

	return BooleanWithValue(f.Frame.HasSlot(StringValue(k))), nil
}

func GetSlotImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !FrameP(f) {
		err = ProcessError(fmt.Sprintf("get-slot requires a frame as it's first argument, but was given %s.", String(f)), env)
	}

	k, err := Eval(Cadr(args), env)
	if err != nil {
		return
	}
	if !SymbolP(k) {
		err = ProcessError(fmt.Sprintf("get-slot requires a symbol as it's second argument, but was given %s.", String(k)), env)
		return
	}

	if !f.Frame.HasSlot(StringValue(k)) {
		err = ProcessError(fmt.Sprintf("get-slot requires an existing slot, but was given %s.", String(k)), env)
		return
	}

	return f.Frame.Get(StringValue(k)), nil
}

func GetSlotOrNilImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !FrameP(f) {
		err = ProcessError(fmt.Sprintf("get-slot-or-nil requires a frame as it's first argument, but was given %s.", String(f)), env)
	}

	k, err := Eval(Cadr(args), env)
	if err != nil {
		return
	}
	if !SymbolP(k) {
		err = ProcessError(fmt.Sprintf("get-slot-or-nil requires a symbol as it's second argument, but was given %s.", String(k)), env)
		return
	}

	return f.Frame.Get(StringValue(k)), nil
}

func RemoveSlotImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f, err := Eval(Car(args), env)
	if err != nil {
		return
	}

	if NilP(f) {
		return False, nil
	}

	if !FrameP(f) {
		err = ProcessError(fmt.Sprintf("remove-slot! requires a frame as it's first argument, but was given %s.", String(f)), env)
	}

	k, err := Eval(Cadr(args), env)
	if err != nil {
		return
	}
	if !SymbolP(k) {
		err = ProcessError(fmt.Sprintf("remove-slot! requires a symbol as it's second argument, but was given %s.", String(k)), env)
		return
	}

	return BooleanWithValue(f.Frame.Remove(StringValue(k))), nil
}

func SetSlotImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !FrameP(f) {
		err = ProcessError(fmt.Sprintf("set-slot! requires a frame as it's first argument, but was given %s.", String(f)), env)
	}

	k, err := Eval(Cadr(args), env)
	if err != nil {
		return
	}
	if !NakedP(k) {
		err = ProcessError(fmt.Sprintf("set-slot! requires a naked symbol as it's second argument, but was given %s.", String(k)), env)
		return
	}

	v, err := Eval(Caddr(args), env)
	if err != nil {
		return
	}

	return f.Frame.Set(StringValue(k), v), nil
}

func SendImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !FrameP(f) {
		err = ProcessError(fmt.Sprintf("send requires a frame as it's first argument, but was given %s.", String(f)), env)
	}

	k, err := Eval(Cadr(args), env)
	if err != nil {
		return
	}
	if !SymbolP(k) {
		err = ProcessError(fmt.Sprintf("send requires a symbol as it's second argument, but was given %s.", String(k)), env)
		return
	}

	if !f.Frame.HasSlot(StringValue(k)) {
		err = ProcessError(fmt.Sprintf("send requires an existing slot, but was given %s.", String(k)), env)
		return
	}

	fun := f.Frame.Get(StringValue(k))
	if !FunctionP(fun) {
		err = ProcessError(fmt.Sprintf("send requires a function slot, but was given a slot containing a %s.", TypeName(TypeOf(fun))), env)
	}

	params := Cddr(args)
	return fun.Func.ApplyWithFrame(params, env, f.Frame)
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

	selector, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !SymbolP(selector) {
		err = ProcessError(fmt.Sprintf("Selector must be a symbol but was %s.", TypeName(TypeOf(selector))), env)
		return
	}

	fun := getSuperFunction(StringValue(selector), env)
	if fun == nil || !FunctionP(fun) {
		err = ProcessError(fmt.Sprintf("Message sent must select a function slot but was %s.", TypeName(TypeOf(fun))), env)
		return
	}

	params := Cdr(args)
	frameEnv := NewSymbolTableFrameBelowWithFrame(env, env.Frame)
	frameEnv.BindLocallyTo(SymbolWithName("self"), FrameWithValue(env.Frame))
	return fun.Func.Apply(params, frameEnv)
}

func CloneImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !FrameP(f) {
		err = ProcessError(fmt.Sprintf("clone requires a frame as it's argument, but was given %s.", String(f)), env)
		return
	}

	return FrameWithValue(f.Frame.Clone()), nil
}

func JsonToLispImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	j, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !StringP(j) {
		err = ProcessError(fmt.Sprintf("json->lisp requires a string as it's argument, but was given %s.", String(j)), env)
		return
	}

	return JsonStringToLispWithFrames(StringValue(j)), nil
}

func LispToJsonImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l, err := Eval(Car(args), env)
	if err != nil {
		return
	}

	return StringWithValue(LispWithFramesToJsonString(l)), nil
}

func KeysImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f, err := Eval(Car(args), env)
	if err != nil {
		return
	}
	if !FrameP(f) {
		err = ProcessError(fmt.Sprintf("keys requires a frame as it's argument, but was given %s.", String(f)), env)
		return
	}

	return ArrayToList(FrameValue(f).Keys()), nil
}
