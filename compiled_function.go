// Copyright 2015 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file implements compiled user defined functions.

package golisp

import (
	"errors"
	"fmt"
)

type CompiledFunction struct {
	Name             string
	Params           *Data
	VarArgs          bool
	RequiredArgCount int
	Body             []uint32
	ProgramCounter   uint32
	Env              *SymbolTableFrame
}

func MakeCompiledFunction(name string, params *Data, body []uint32, parentEnv *SymbolTableFrame) *CompiledFunction {
	requiredArgs, varArgs := ComputeRequiredArgumentCount(params)
	return &CompiledFunction{Name: name, Params: params, VarArgs: varArgs, RequiredArgCount: requiredArgs, Body: body, Env: parentEnv}
}

func (self *CompiledFunction) String() string {
	return fmt.Sprintf("<compiled func: %s>", self.Name)
}

func (self *CompiledFunction) makeLocalBindings(args *Data, argEnv *SymbolTableFrame, localEnv *SymbolTableFrame, eval bool) (err error) {
	if self.VarArgs {
		if Length(args) < self.RequiredArgCount {
			return errors.New(fmt.Sprintf("%s expected at least %d parameters, received %d.", self.Name, self.RequiredArgCount, Length(args)))
		}
	} else {
		if Length(args) != self.RequiredArgCount {
			return errors.New(fmt.Sprintf("%s expected %d parameters, received %d.", self.Name, self.RequiredArgCount, Length(args)))
		}
	}

	var argValue *Data
	var accumulatingParam *Data = nil
	accumulatedArgs := make([]*Data, 0)
	for p, a := self.Params, args; NotNilP(a); a = Cdr(a) {
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
			localEnv.BindLocallyTo(Car(p), argValue)
		}
		if accumulatingParam == nil {
			p = Cdr(p)
		}
		if SymbolP(p) {
			accumulatingParam = p
		}
	}
	if accumulatingParam != nil {
		localEnv.BindLocallyTo(accumulatingParam, ArrayToList(accumulatedArgs))
	}
	return nil
}

func (self *CompiledFunction) internalApply(args *Data, argEnv *SymbolTableFrame, frame *FrameMap, eval bool) (result *Data, err error) {
	localEnv := NewSymbolTableFrameBelowWithFrame(self.Env, frame)
	localEnv.Previous = argEnv
	selfSym := SymbolWithName("self")
	if frame != nil {
		localEnv.BindLocallyTo(selfSym, FrameWithValue(frame))
	} else {
		selfBinding, found := argEnv.findBindingInLocalFrameFor(selfSym)
		if found {
			localEnv.BindLocallyTo(selfSym, selfBinding.Val)
		}
	}
	err = self.makeLocalBindings(args, argEnv, localEnv, eval)
	if err != nil {
		return
	}

	// Now we have to execute the bytecodes

	// for s := self.Body; NotNilP(s); s = Cdr(s) {
	// 	result, err = Eval(Car(s), localEnv)
	// 	if err != nil {
	// 		return nil, errors.New(fmt.Sprintf("In '%s': %s", self.Name, err))
	// 	}
	// }
	return
}

func (self *CompiledFunction) Apply(args *Data, argEnv *SymbolTableFrame) (result *Data, err error) {
	return self.internalApply(args, argEnv, nil, true)
}

func (self *CompiledFunction) ApplyWithFrame(args *Data, argEnv *SymbolTableFrame, frame *FrameMap) (result *Data, err error) {
	return self.internalApply(args, argEnv, frame, true)
}

func (self *CompiledFunction) ApplyWithoutEval(args *Data, argEnv *SymbolTableFrame) (result *Data, err error) {
	return self.internalApply(args, argEnv, nil, false)
}
