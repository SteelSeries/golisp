// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpreter for embedding in a go program for scripting.
// This file implements user defined functions.

package golisp

import (
	"errors"
	"fmt"
	"sync/atomic"
	"unsafe"
)

type Function struct {
	Name             string
	Params           *Data
	VarArgs          bool
	RequiredArgCount int
	Body             *Data
	Env              *SymbolTableFrame
	DebugOnEntry     bool
	SlotFunction     int32
	ParentProcess    *Process
}

func computeRequiredArgumentCount(args *Data) (requiredArgumentCount int, varArgs bool) {
	requiredArgumentCount = 0
	varArgs = false
	for a := args; NotNilP(a); a = Cdr(a) {
		if SymbolP(a) {
			varArgs = true
			return
		} else {
			requiredArgumentCount += 1
		}
	}
	return
}

func MakeFunction(name string, params *Data, body *Data, parentEnv *SymbolTableFrame) *Function {
	requiredArgs, varArgs := computeRequiredArgumentCount(params)
	return &Function{Name: name, Params: params, VarArgs: varArgs, RequiredArgCount: requiredArgs, Body: body, Env: parentEnv, SlotFunction: 0}
}

func (f *Function) String() string {
	return fmt.Sprintf("<func: %s>", f.Name)
}

func (f *Function) makeLocalBindings(args *Data, argEnv *SymbolTableFrame, localEnv *SymbolTableFrame, eval bool) (err error) {
	if f.VarArgs {
		if Length(args) < f.RequiredArgCount {
			return errors.New(fmt.Sprintf("%s expected at least %d parameters, received %d.", f.Name, f.RequiredArgCount, Length(args)))
		}
	} else {
		if Length(args) != f.RequiredArgCount {
			return errors.New(fmt.Sprintf("%s expected %d parameters, received %d.", f.Name, f.RequiredArgCount, Length(args)))
		}
	}

	var argValue *Data
	var accumulatingParam *Data = nil
	accumulatedArgs := make([]*Data, 0)
	for p, a := f.Params, args; NotNilP(a); a = Cdr(a) {
		if eval {
			argValue, err = Eval(Car(a), argEnv)
			if err != nil {
				return
			}
		} else {
			argValue = Car(a)
		}

		if accumulatingParam != nil {
			accumulatedArgs = append(accumulatedArgs, argValue)
		} else {
			_, err = localEnv.BindLocallyTo(Car(p), argValue)
			if err != nil {
				return
			}
		}
		if accumulatingParam == nil {
			p = Cdr(p)
		}
		if SymbolP(p) {
			accumulatingParam = p
		}
	}
	if accumulatingParam != nil {
		_, err = localEnv.BindLocallyTo(accumulatingParam, ArrayToList(accumulatedArgs))
		if err != nil {
			return
		}
	}
	return nil
}

func (f *Function) internalApply(args *Data, argEnv *SymbolTableFrame, frame *FrameMap, eval bool) (result *Data, err error) {
	localEnv := NewSymbolTableFrameBelowWithFrame(f.Env, frame, f.Name)
	localEnv.Previous = argEnv
	selfSym := Intern("f")
	if frame != nil {
		_, err = localEnv.BindLocallyTo(selfSym, FrameWithValue(frame))
		if err != nil {
			return
		}
	} else if atomic.LoadInt32(&f.SlotFunction) == 1 {
		selfBinding, found := argEnv.findBindingInLocalFrameFor(selfSym)
		if found {
			_, err = localEnv.BindLocallyTo(selfSym, selfBinding.Val)
			if err != nil {
				return
			}
		}
	}

	parentProcSym := Intern("parentProcess")
	if f.ParentProcess != nil {
		procObj := ObjectWithTypeAndValue("Process", unsafe.Pointer(f.ParentProcess))
		_, err = localEnv.BindLocallyTo(parentProcSym, procObj)
		if err != nil {
			return
		}
	}

	err = f.makeLocalBindings(args, argEnv, localEnv, eval)
	if err != nil {
		return
	}

	localGuid := atomic.AddInt64(&ProfileGUID, 1) - 1

	ProfileEnter("func", f.Name, localGuid)

	for s := f.Body; NotNilP(s); s = Cdr(s) {
		result, err = Eval(Car(s), localEnv)
		if err != nil {
			result, err = nil, errors.New(fmt.Sprintf("In '%s': %s", f.Name, err))
			break
		}
	}

	ProfileExit("func", f.Name, localGuid)

	return
}

func (f *Function) Apply(args *Data, argEnv *SymbolTableFrame) (result *Data, err error) {
	return f.internalApply(args, argEnv, nil, true)
}

func (f *Function) ApplyWithFrame(args *Data, argEnv *SymbolTableFrame, frame *FrameMap) (result *Data, err error) {
	return f.internalApply(args, argEnv, frame, true)
}

func (f *Function) ApplyWithoutEval(args *Data, argEnv *SymbolTableFrame) (result *Data, err error) {
	return f.internalApply(args, argEnv, nil, false)
}

func (f *Function) ApplyWithoutEvalWithFrame(args *Data, argEnv *SymbolTableFrame, frame *FrameMap) (result *Data, err error) {
	return f.internalApply(args, argEnv, frame, false)
}

func (f *Function) ApplyOveriddingEnvironment(args *Data, argEnv *SymbolTableFrame) (result *Data, err error) {
	localEnv := NewSymbolTableFrameBelow(argEnv, f.Name)
	err = f.makeLocalBindings(args, argEnv, localEnv, true)
	if err != nil {
		return
	}

	localGuid := atomic.AddInt64(&ProfileGUID, 1) - 1

	ProfileEnter("func", f.Name, localGuid)

	for s := f.Body; NotNilP(s); s = Cdr(s) {
		result, err = Eval(Car(s), localEnv)
		if err != nil {
			result, err = nil, errors.New(fmt.Sprintf("In '%s': %s", f.Name, err))
			break
		}
	}

	ProfileExit("func", f.Name, localGuid)

	return
}
