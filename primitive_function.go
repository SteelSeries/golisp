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
	ArgTypes     []uint32
	Body         func(d *Data, env *SymbolTableFrame) (*Data, error)
}

func MakeTypedPrimitiveFunction(name string, argCount string, function func(*Data, *SymbolTableFrame) (*Data, error), types []uint32) {
	f := &PrimitiveFunction{Name: name, Special: false, NumberOfArgs: argCount, ArgTypes: types, Body: function}
	sym := Global.Intern(name)
	Global.BindTo(sym, PrimitiveWithNameAndFunc(name, f))
}

func MakePrimitiveFunction(name string, argCount string, function func(*Data, *SymbolTableFrame) (*Data, error)) {
	MakeTypedPrimitiveFunction(name, argCount, function, make([]uint32, 0, 0))
}

func MakeSpecialForm(name string, argCount string, function func(*Data, *SymbolTableFrame) (*Data, error)) {
	f := &PrimitiveFunction{Name: name, Special: true, NumberOfArgs: argCount, Body: function}
	sym := Global.Intern(name)
	Global.BindTo(sym, PrimitiveWithNameAndFunc(name, f))
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

func (self *PrimitiveFunction) checkArgumentTypes(args []*Data) int {
	for i := 0; i < len(self.ArgTypes) && i < len(args); i = i + 1 {
		arg := args[i]
		if (TypeOf(arg) & self.ArgTypes[i]) == 0 {
			return i
		}
	}
	return -1
}

func (self *PrimitiveFunction) typesToString(types uint32) string {
	typeNames := make([]string, 0, 8)
	for mask := uint32(0x00000001); mask != 0x80000000; mask <<= 1 {
		if (types & mask) != 0 {
			typeNames = append(typeNames, TypeName(mask))
		}
	}
	return strings.Join(typeNames, " or ")
}

func (self *PrimitiveFunction) Apply(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if !self.checkArgumentCount(Length(args)) {
		err = fmt.Errorf("Wrong number of args to %s, expected %s but got %d.", self.Name, self.NumberOfArgs, Length(args))
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

	argCheckResult := self.checkArgumentTypes(argArray)
	if argCheckResult != -1 {
		err = fmt.Errorf("Wrong argument type for argument %d; expected %s but got the %s: %s", argCheckResult, self.typesToString(self.ArgTypes[argCheckResult]), TypeName(TypeOf(argArray[argCheckResult])), String(argArray[argCheckResult]))
		return
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
