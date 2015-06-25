// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file implements user defined functions.

package golisp

import (
	"errors"
	"fmt"
	"time"
)

type Function struct {
	Name             string
	Params           *Data
	VarArgs          bool
	RequiredArgCount int
	Body             *Data
	Env              *SymbolTableFrame
	DebugOnEntry     bool
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
	return &Function{Name: name, Params: params, VarArgs: varArgs, RequiredArgCount: requiredArgs, Body: body, Env: parentEnv}
}

func (self *Function) String() string {
	return fmt.Sprintf("<func: %s>", self.Name)
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

func (self *Function) internalApply(args *Data, argEnv *SymbolTableFrame, frame *FrameMap, eval bool) (result *Data, err error) {
	localEnv := NewSymbolTableFrameBelowWithFrame(self.Env, frame, self.Name)
	localEnv.Previous = argEnv
	selfSym := Intern("self")
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

	ProfileEnter("func", self.Name)

	for s := self.Body; NotNilP(s); s = Cdr(s) {
		result, err = Eval(Car(s), localEnv)
		if err != nil {
			result, err = nil, errors.New(fmt.Sprintf("In '%s': %s", self.Name, err))
			break
		}
	}

	ProfileExit("func", self.Name)

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

	ProfileEnter("func", self.Name)

	for s := self.Body; NotNilP(s); s = Cdr(s) {
		result, err = Eval(Car(s), localEnv)
		if err != nil {
			result, err = nil, errors.New(fmt.Sprintf("In '%s': %s", self.Name, err))
			break
		}
	}

	ProfileExit("func", self.Name)

	return
}
