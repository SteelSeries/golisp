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

func (self *ComppiledFunction) String() string {
	return fmt.Sprintf("<compiled func: %s>", self.Name)
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

	for s := self.Body; NotNilP(s); s = Cdr(s) {
		result, err = Eval(Car(s), localEnv)
		if err != nil {
			return nil, errors.New(fmt.Sprintf("In '%s': %s", self.Name, err))
		}
	}
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
