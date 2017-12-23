// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpreter for embedding in a go program for scripting.
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

func (m *Macro) String() string {
	return fmt.Sprintf("<macro: %s>", m.Name)
}

func (m *Macro) makeLocalBindings(args *Data, argEnv *SymbolTableFrame, localEnv *SymbolTableFrame, eval bool) (err error) {
	if m.VarArgs {
		if Length(args) < m.RequiredArgCount {
			return errors.New(fmt.Sprintf("%s expected at least %d parameters, received %d.", m.Name, m.RequiredArgCount, Length(args)))
		}
	} else {
		if Length(args) != m.RequiredArgCount {
			return errors.New(fmt.Sprintf("%s expected %d parameters, received %d.", m.Name, m.RequiredArgCount, Length(args)))
		}
	}

	var argValue *Data
	var accumulatingParam *Data = nil
	accumulatedArgs := make([]*Data, 0)
	for p, a := m.Params, args; NotNilP(a); a = Cdr(a) {
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

func (m *Macro) Expand(args *Data, argEnv *SymbolTableFrame) (result *Data, err error) {
	localEnv := NewSymbolTableFrameBelow(m.Env, m.Name)
	err = m.makeLocalBindings(args, argEnv, localEnv, false)
	if err != nil {
		return
	}

	return Eval(m.Body, localEnv)
}

func (m *Macro) internalApply(args *Data, argEnv *SymbolTableFrame, eval bool) (result *Data, err error) {
	expandedMacro, err := m.Expand(args, argEnv)
	if err != nil {
		return
	}

	return Eval(expandedMacro, argEnv)
}

func (m *Macro) Apply(args *Data, argEnv *SymbolTableFrame) (result *Data, err error) {
	return m.internalApply(args, argEnv, false)
}

func (m *Macro) ApplyWithoutEval(args *Data, argEnv *SymbolTableFrame) (result *Data, err error) {
	return m.internalApply(args, argEnv, false)
}
