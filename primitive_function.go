// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file implements primitive functions.

package golisp

import (
	"fmt"
	"strings"
	"sync/atomic"
)

type PrimitiveFunction struct {
	Name         string
	Special      bool
	NumberOfArgs string
	Body         func(d *Data, env *SymbolTableFrame) (*Data, error)
	IsRestricted bool
}

func MakePrimitiveFunction(name string, argCount string, function func(*Data, *SymbolTableFrame) (*Data, error)) {
	f := &PrimitiveFunction{Name: name, Special: false, NumberOfArgs: argCount, Body: function, IsRestricted: false}
	Global.BindToProtected(Intern(name), PrimitiveWithNameAndFunc(name, f))
}

func MakeRestrictedPrimitiveFunction(name string, argCount string, function func(*Data, *SymbolTableFrame) (*Data, error)) {
	f := &PrimitiveFunction{Name: name, Special: false, NumberOfArgs: argCount, Body: function, IsRestricted: true}
	Global.BindToProtected(Intern(name), PrimitiveWithNameAndFunc(name, f))
}

func MakeSpecialForm(name string, argCount string, function func(*Data, *SymbolTableFrame) (*Data, error)) {
	f := &PrimitiveFunction{Name: name, Special: true, NumberOfArgs: argCount, Body: function, IsRestricted: false}
	Global.BindToProtected(Intern(name), PrimitiveWithNameAndFunc(name, f))
}

func MakeRestrictedSpecialForm(name string, argCount string, function func(*Data, *SymbolTableFrame) (*Data, error)) {
	f := &PrimitiveFunction{Name: name, Special: true, NumberOfArgs: argCount, Body: function, IsRestricted: true}
	Global.BindToProtected(Intern(name), PrimitiveWithNameAndFunc(name, f))
}

func (self *PrimitiveFunction) String() string {
	return fmt.Sprintf("<prim: %s, %v>", self.Name, self.Body)
}

func (self *PrimitiveFunction) checkArgumentCount(argCount int) bool {
	if self.NumberOfArgs == "*" {
		return true
	}

	for _, term := range strings.Split(self.NumberOfArgs, "|") {
		var intTerm int
		n, _ := fmt.Sscanf(term, "%d", &intTerm)
		if n == 1 && argCount == intTerm {
			return true
		}
		n, _ = fmt.Sscanf(term, ">=%d", &intTerm)
		if n == 1 && argCount >= intTerm {
			return true
		}
		var lo int
		var hi int
		n, _ = fmt.Sscanf(term, "(%d,%d)", &lo, &hi)
		if n == 2 && lo <= argCount && argCount <= hi {
			return true
		}
	}
	return false
}

func (self *PrimitiveFunction) Apply(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if self.IsRestricted && env.IsRestricted {
		err = fmt.Errorf("The %s primitive is restricted from execution in this environment\n", self.Name)
		return
	}

	if !self.checkArgumentCount(Length(args)) {
		err = fmt.Errorf("Wrong number of args to %s. Expected %s but got %d.\n", self.Name, self.NumberOfArgs, Length(args))
		return
	}

	argArray := make([]*Data, 0)
	var argValue *Data
	for a := args; NotNilP(a); a = Cdr(a) {
		if self.Special {
			argValue = Car(a)
		} else {
			argValue, err = Eval(Car(a), env)
			if err != nil {
				return
			}
		}

		argArray = append(argArray, argValue)
	}

	localGuid := atomic.AddInt64(&ProfileGUID, 1) - 1

	fType := "prim"
	if self.Special {
		fType = "form"
	}

	ProfileEnter(fType, self.Name, localGuid)

	result, err = (self.Body)(ArrayToList(argArray), env)

	ProfileExit(fType, self.Name, localGuid)

	return
}

func (self *PrimitiveFunction) ApplyWithoutEval(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if self.Special {
		return self.Apply(args, env)
	} else {
		return self.Apply(QuoteAll(args), env)
	}
}
