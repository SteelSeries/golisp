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
	Name   string
	Params *Data
	Body   *Data
	Env    *SymbolTableFrame
}

func MakeMacro(name string, params *Data, body *Data, parentEnv *SymbolTableFrame) *Macro {
	return &Macro{Name: name, Params: params, Body: body, Env: parentEnv}
}

func (self *Macro) String() string {
	return fmt.Sprintf("<macro: %s>", self.Name)
}

func (self *Macro) makeLocalBindings(args *Data, argEnv *SymbolTableFrame, localEnv *SymbolTableFrame, eval bool) (err error) {
	if Length(args) != Length(self.Params) {
		return errors.New("Number of args must equal number of params")
	}

	var data *Data
	for p, a := self.Params, args; NotNilP(p); p, a = Cdr(p), Cdr(a) {
		if eval {
			data, err = Eval(Car(a), argEnv)
			if err != nil {
				return
			}
		} else {
			data = Car(a)
		}
		localEnv.BindLocallyTo(Car(p), data)
	}
	return nil
}

func (self *Macro) Expand(args *Data, argEnv *SymbolTableFrame) (result *Data, err error) {
	localEnv := NewSymbolTableFrameBelow(self.Env, self.Name)
	err = self.makeLocalBindings(args, argEnv, localEnv, true)
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
	return self.internalApply(args, argEnv, true)
}

func (self *Macro) ApplyWithoutEval(args *Data, argEnv *SymbolTableFrame) (result *Data, err error) {
	return self.internalApply(args, argEnv, false)
}
