// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file implements user defined macros.

package golisp

import (
	"errors"
	"fmt"
)

type Macro struct {
	Name             string
	Params           *Data
	VarArgs          bool
	RequiredArgCount int
	Body             *Data
	Env              *SymbolTableFrame
}

func MakeMacro(name string, params *Data, body *Data, parentEnv *SymbolTableFrame) *Macro {
	requiredArgs, varArgs := computeRequiredArgumentCount(params)
	return &Macro{Name: name, Params: params, VarArgs: varArgs, RequiredArgCount: requiredArgs, Body: body, Env: parentEnv}
}

func (self *Macro) String() string {
	return fmt.Sprintf("<macro: %s>", self.Name)
}

func (self *Macro) makeLocalBindings(args *Data, argEnv *SymbolTableFrame, localEnv *SymbolTableFrame, eval bool) (err error) {
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

func (self *Macro) Expand(args *Data, argEnv *SymbolTableFrame) (result *Data, err error) {
	localEnv := NewSymbolTableFrameBelow(self.Env, self.Name)
	err = self.makeLocalBindings(args, argEnv, localEnv, false)
	if err != nil {
		return
	}

	return Eval(self.Body, localEnv)
}

func (self *Macro) internalApply(args *Data, argEnv *SymbolTableFrame, eval bool) (result *Data, err error) {
	expandedMacro, err := self.Expand(args, argEnv)
	if err != nil {
		return
	}

	return Eval(expandedMacro, argEnv)
}

func (self *Macro) Apply(args *Data, argEnv *SymbolTableFrame) (result *Data, err error) {
	return self.internalApply(args, argEnv, false)
}

func (self *Macro) ApplyWithoutEval(args *Data, argEnv *SymbolTableFrame) (result *Data, err error) {
	return self.internalApply(args, argEnv, false)
}
