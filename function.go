// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file implements user defined functions.

package golisp

import (
	"errors"
	"fmt"
	"strings"
	"sync/atomic"
	"unsafe"
)

type FunctonTypeSignature struct {
	ArgumentTypes []uint32
	ReturnType    uint32
}

type Function struct {
	Name             string
	Params           *Data
	VarArgs          bool
	RequiredArgCount int
	TypeSignature    *FunctonTypeSignature
	DocString        string
	Body             *Data
	Env              *SymbolTableFrame
	DebugOnEntry     bool
	SlotFunction     bool
	ParentProcess    *Process
}

var functionTypeSignatures map[string]*FunctonTypeSignature = make(map[string]*FunctonTypeSignature, 20)

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

func AddTypesForFunction(name string, argTypes []uint32, retType uint32) {
	functionTypeSignatures[name] = &FunctonTypeSignature{ArgumentTypes: argTypes, ReturnType: retType}
}

func MakeFunction(name string, params *Data, doc string, body *Data, parentEnv *SymbolTableFrame) *Function {
	requiredArgs, varArgs := computeRequiredArgumentCount(params)
	f := &Function{Name: name, Params: params, VarArgs: varArgs, RequiredArgCount: requiredArgs, TypeSignature: functionTypeSignatures[name], DocString: doc, Body: body, Env: parentEnv, SlotFunction: false}
	functionTypeSignatures[name] = nil
	return f
}

func (self *Function) String() string {
	return fmt.Sprintf("<func: %s>", self.Name)
}

func typeNameFor(value uint32) string {
	types := make([]string, 0, 3)
	for name, mask := range TypeMap {
		if mask&value != 0 {
			types = append(types, name)
		}
	}
	return strings.Join(types, " or ")
}

func (self *Function) makeLocalBindings(args *Data, argEnv *SymbolTableFrame, localEnv *SymbolTableFrame, eval bool) (err error) {
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
	for i, p, a := 0, self.Params, args; NotNilP(a); i, a = i+1, Cdr(a) {
		if eval {
			argValue, err = Eval(Car(a), argEnv)
			if err != nil {
				return
			}
		} else {
			argValue = Car(a)
		}

		if self.TypeSignature != nil && self.TypeSignature.ArgumentTypes[i]&TypeOf(argValue) == 0 {
			return errors.New(fmt.Sprintf("%s argument %d has the wrong type, expected %s but was given %s", self.Name, i, typeNameFor(self.TypeSignature.ArgumentTypes[i]), typeNameFor(TypeOf(argValue))))
		}

		if SymbolP(p) {
			accumulatingParam = p
		}
		if accumulatingParam != nil {
			accumulatedArgs = append(accumulatedArgs, argValue)
		} else {
			localEnv.BindLocallyTo(Car(p), argValue)
		}
		if accumulatingParam == nil {
			p = Cdr(p)
		}
	}
	if accumulatingParam != nil {
		localEnv.BindLocallyTo(accumulatingParam, ArrayToList(accumulatedArgs))
	}
	return nil
}

func (self *Function) internalApply(args *Data, argEnv *SymbolTableFrame, frame *FrameMap, eval bool) (result *Data, err error) {
	localEnv := NewSymbolTableFrameBelowWithFrame(self.Env, frame, self.Name)
	localEnv.Previous = argEnv
	selfSym := Intern("self")
	if frame != nil {
		localEnv.BindLocallyTo(selfSym, FrameWithValue(frame))
	} else if self.SlotFunction {
		selfBinding, found := argEnv.findBindingInLocalFrameFor(selfSym)
		if found {
			localEnv.BindLocallyTo(selfSym, selfBinding.Val)
		}
	}

	parentProcSym := Intern("parentProcess")
	if self.ParentProcess != nil {
		procObj := ObjectWithTypeAndValue("Process", unsafe.Pointer(self.ParentProcess))
		localEnv.BindLocallyTo(parentProcSym, procObj)
	}

	err = self.makeLocalBindings(args, argEnv, localEnv, eval)
	if err != nil {
		return
	}

	localGuid := atomic.AddInt64(&ProfileGUID, 1) - 1

	ProfileEnter("func", self.Name, localGuid)

	for s := self.Body; NotNilP(s); s = Cdr(s) {
		result, err = Eval(Car(s), localEnv)
		if err != nil {
			result, err = nil, errors.New(fmt.Sprintf("In '%s': %s", self.Name, err))
			break
		}
	}

	if err == nil {
		if self.TypeSignature != nil && self.TypeSignature.ReturnType&TypeOf(result) == 0 {
			result, err = nil, errors.New(fmt.Sprintf("%s returns the wrong type, expected %s but returned %s", self.Name, typeNameFor(self.TypeSignature.ReturnType), typeNameFor(TypeOf(result))))
		}
	}

	ProfileExit("func", self.Name, localGuid)

	return
}

func (self *Function) Apply(args *Data, argEnv *SymbolTableFrame) (result *Data, err error) {
	return self.internalApply(args, argEnv, nil, true)
}

func (self *Function) ApplyWithFrame(args *Data, argEnv *SymbolTableFrame, frame *FrameMap) (result *Data, err error) {
	return self.internalApply(args, argEnv, frame, true)
}

func (self *Function) ApplyWithoutEval(args *Data, argEnv *SymbolTableFrame) (result *Data, err error) {
	return self.internalApply(args, argEnv, nil, false)
}

func (self *Function) ApplyWithoutEvalWithFrame(args *Data, argEnv *SymbolTableFrame, frame *FrameMap) (result *Data, err error) {
	return self.internalApply(args, argEnv, frame, false)
}

func (self *Function) ApplyOveriddingEnvironment(args *Data, argEnv *SymbolTableFrame) (result *Data, err error) {
	localEnv := NewSymbolTableFrameBelow(argEnv, self.Name)
	err = self.makeLocalBindings(args, argEnv, localEnv, true)
	if err != nil {
		return
	}

	localGuid := atomic.AddInt64(&ProfileGUID, 1) - 1

	ProfileEnter("func", self.Name, localGuid)

	for s := self.Body; NotNilP(s); s = Cdr(s) {
		result, err = Eval(Car(s), localEnv)
		if err != nil {
			result, err = nil, errors.New(fmt.Sprintf("In '%s': %s", self.Name, err))
			break
		}
	}

	ProfileExit("func", self.Name, localGuid)

	return
}
