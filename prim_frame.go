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
	MakeTypedPrimitiveFunction("make-slotname", "1", MakeSlotnameImpl, []uint32{StringType | SymbolType})
	MakePrimitiveFunction("make-frame", "*", MakeFrameImpl)
	MakeTypedPrimitiveFunction("has-slot?", "2", HasSlotImpl, []uint32{FrameType, SymbolType})
	MakeTypedPrimitiveFunction("get-slot", "2", GetSlotImpl, []uint32{FrameType, SymbolType})
	MakeTypedPrimitiveFunction("get-slot-or-nil", "2", GetSlotOrNilImpl, []uint32{FrameType, SymbolType})
	MakeTypedPrimitiveFunction("remove-slot!", "2", RemoveSlotImpl, []uint32{FrameType, SymbolType})
	MakeTypedPrimitiveFunction("set-slot!", "3", SetSlotImpl, []uint32{FrameType, SymbolType, AnyType})
	MakeTypedPrimitiveFunction("send", ">=2", SendImpl, []uint32{FrameType, SymbolType, AnyType})
	MakeTypedPrimitiveFunction("send-super", ">=1", SendSuperImpl, []uint32{SymbolType, AnyType})
	MakeTypedPrimitiveFunction("apply-slot", ">=3", ApplySlotImpl, []uint32{FrameType, SymbolType, AnyType})
	MakeTypedPrimitiveFunction("apply-slot-super", ">=2", ApplySlotSuperImpl, []uint32{SymbolType, AnyType})
	MakeTypedPrimitiveFunction("clone", "1", CloneImpl, []uint32{FrameType})
	MakeTypedPrimitiveFunction("json->lisp", "1", JsonToLispImpl, []uint32{StringType})
	MakeTypedPrimitiveFunction("lisp->json", "1", LispToJsonImpl, []uint32{NilType | FrameType | ConsCellType | IntegerType | FloatType | StringType | SymbolType | BooleanType})
	MakeTypedPrimitiveFunction("frame-keys", "1", FrameKeysImpl, []uint32{FrameType})
	MakeTypedPrimitiveFunction("frame-values", "1", FrameValuesImpl, []uint32{FrameType})
	MakeTypedPrimitiveFunction("frame-length", "1", FrameLengthImpl, []uint32{FrameType})
}

func MakeSlotnameImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	base := First(args)
	if NakedP(base) {
		result = base
	} else {
		result = NakedSymbolFrom(base)
	}
	return
}

func MakeFrameImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if Length(args)%2 != 0 {
		err = ProcessError("Frames must be initialized with an even number of arguments.", env)
		return
	}
	m := make(FrameMap)
	for c := args; NotNilP(c); c = Cddr(c) {
		k := Car(c)
		if !NakedP(k) {
			err = ProcessError(fmt.Sprintf("Frame keys must be naked symbols, but was given %s.", String(k)), env)
			return
		}
		v := Cadr(c)
		m[StringValue(k)] = v
	}
	return FrameWithValue(&m), nil
}

func HasSlotImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	k := Second(args)
	if !NakedP(k) {
		err = ProcessError(fmt.Sprintf("has-slot? requires a naked symbol as it's second argument, but was given %s.", String(k)), env)
		return
	}

	return BooleanWithValue(FrameValue(f).HasSlot(StringValue(k))), nil
}

func GetSlotImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	if FrameValue(f) == nil {
		err = ProcessError("get-slot received a nil frame.", env)
		return
	}

	k := Second(args)
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
	f := First(args)
	k := Second(args)
	if !NakedP(k) {
		err = ProcessError(fmt.Sprintf("get-slot-or-nil requires a naked symbol as it's second argument, but was given %s.", String(k)), env)
	} else {
		result = FrameValue(f).Get(StringValue(k))
	}
	return
}

func RemoveSlotImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	k := Second(args)
	if !NakedP(k) {
		err = ProcessError(fmt.Sprintf("remove-slot! requires a naked symbol as it's second argument, but was given %s.", String(k)), env)
	} else {
		result = BooleanWithValue(FrameValue(f).Remove(StringValue(k)))
	}
	return
}

func SetSlotImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	k := Second(args)
	if !NakedP(k) {
		err = ProcessError(fmt.Sprintf("set-slot! requires a naked symbol as it's second argument, but was given %s.", String(k)), env)
	} else {
		result = FrameValue(f).Set(StringValue(k), Third(args))
	}
	return
}

func SendImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	k := Second(args)
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

	selector := First(args)
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
	frameEnv.BindLocallyTo(Intern("self"), FrameWithValue(env.Frame))
	return FunctionValue(fun).ApplyWithoutEval(params, frameEnv)
}

func ApplySlotImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	k := Second(args)
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

	ary := ToArray(Cddr(args))

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

	selector := First(args)
	if err != nil {
		return
	}
	if !NakedP(selector) {
		err = ProcessError(fmt.Sprintf("apply-slot-super requirs a naked symbol as it's first argumentbut was %s.", TypeName(TypeOf(selector))), env)
		return
	}

	fun := getSuperFunction(StringValue(selector), env)
	if fun == nil || !FunctionP(fun) {
		err = ProcessError(fmt.Sprintf("apply-slot-super must select a function slot but was %s.", TypeName(TypeOf(fun))), env)
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

	frameEnv := NewSymbolTableFrameBelowWithFrame(env, env.Frame, fmt.Sprintf("%s'", env.Name))
	frameEnv.BindLocallyTo(Intern("self"), FrameWithValue(env.Frame))
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
	j := First(args)
	return JsonStringToLispWithFrames(StringValue(j)), nil
}

func LispToJsonImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	l := First(args)
	return StringWithValue(LispWithFramesToJsonString(l)), nil
}

func FrameKeysImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	return ArrayToList(FrameValue(f).Keys()), nil
}

func FrameValuesImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	return ArrayToList(FrameValue(f).Values()), nil
}

func FrameLengthImpl(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	f := First(args)
	return IntegerWithValue(int64(len(*FrameValue(f)))), nil
}
