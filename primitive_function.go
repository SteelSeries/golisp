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

const (
	ARGS_ANY = iota
	ARGS_EQ
	ARGS_GTE
	ARGS_RANGE
)

type ArgRestriction struct {
	Type  int
	Range []int
}

type PrimitiveFunction struct {
	Name            string
	Special         bool
	ArgRestrictions []ArgRestriction
	ArgTypes        []uint32
	Body            func(d *Data, env *SymbolTableFrame) (*Data, error)
}

func MakeTypedPrimitiveFunction(name string, argCount string, function func(*Data, *SymbolTableFrame) (*Data, error), types []uint32) {
	f := &PrimitiveFunction{Name: name, Special: false, ArgTypes: types, Body: function}
	f.parseNumArgs(argCount)
	sym := Intern(name)
	Global.BindToProtected(sym, PrimitiveWithNameAndFunc(name, f))
}

func MakePrimitiveFunction(name string, argCount string, function func(*Data, *SymbolTableFrame) (*Data, error)) {
	MakeTypedPrimitiveFunction(name, argCount, function, make([]uint32, 0, 0))
}

func MakeSpecialForm(name string, argCount string, function func(*Data, *SymbolTableFrame) (*Data, error)) {
	f := &PrimitiveFunction{Name: name, Special: true, Body: function}
	f.parseNumArgs(argCount)
	sym := Intern(name)
	Global.BindToProtected(sym, PrimitiveWithNameAndFunc(name, f))
}

func (self *PrimitiveFunction) parseNumArgs(argCount string) {
	var argRestrictions []ArgRestriction

	for _, term := range strings.Split(argCount, "|") {
		if term == "*" {
			self.ArgRestrictions = []ArgRestriction{{Type: ARGS_ANY}}
			return
		}

		var intTerm int
		n, _ := fmt.Sscanf(term, "%d", &intTerm)
		if n == 1 {
			argRestrictions = append(argRestrictions, ArgRestriction{Type: ARGS_EQ, Range: []int{intTerm}})
			continue
		}
		n, _ = fmt.Sscanf(term, ">=%d", &intTerm)
		if n == 1 {
			argRestrictions = append(argRestrictions, ArgRestriction{Type: ARGS_GTE, Range: []int{intTerm}})
			continue
		}
		var lo int
		var hi int
		n, _ = fmt.Sscanf(term, "(%d,%d)", &lo, &hi)
		if n == 2 {
			//lo <= argCount && argCount <= hi
			argRestrictions = append(argRestrictions, ArgRestriction{Type: ARGS_RANGE, Range: []int{lo, hi}})
			continue
		}
	}

	self.ArgRestrictions = argRestrictions
}

func (self *PrimitiveFunction) argsString() string {
	parts := make([]string, len(self.ArgRestrictions))
	for i, term := range self.ArgRestrictions {
		switch term.Type {
		case ARGS_ANY:
			return "*"
		case ARGS_EQ:
			parts[i] = fmt.Sprintf("%d", term.Range[0])
		case ARGS_GTE:
			parts[i] = fmt.Sprintf(">=%d", term.Range[0])
		case ARGS_RANGE:
			parts[i] = fmt.Sprintf("(%d,%d)", term.Range[0], term.Range[1])
		}
	}
	return strings.Join(parts, "|")
}

func (self *PrimitiveFunction) String() string {
	return fmt.Sprintf("<prim: %s, %v>", self.Name, &self.Body)
}

func (self *PrimitiveFunction) checkArgumentCount(argCount int) bool {
	for _, term := range self.ArgRestrictions {
		switch term.Type {
		case ARGS_ANY:
			return true
		case ARGS_EQ:
			if argCount == term.Range[0] {
				return true
			}
		case ARGS_GTE:
			if argCount >= term.Range[0] {
				return true
			}
		case ARGS_RANGE:
			if term.Range[0] <= argCount && argCount <= term.Range[1] {
				return true
			}
		}
	}
	return false
}

func nextTypeIndex(typeIndex int, limit int) int {
	if typeIndex < limit {
		return typeIndex + 1
	} else {
		return typeIndex
	}
}

func (self *PrimitiveFunction) checkArgumentTypes(args []*Data) int {
	if len(self.ArgTypes) > 0 {
		numberOfArgs := len(args)
		numberOfTypesLimit := len(self.ArgTypes) - 1
		for argIndex, typeIndex := 0, 0; argIndex < numberOfArgs; argIndex, typeIndex = argIndex+1, nextTypeIndex(typeIndex, numberOfTypesLimit) {
			if (args[argIndex] != nil) && ((TypeOf(args[argIndex]) & self.ArgTypes[typeIndex]) == 0) {
				return argIndex
			}
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

func (self *PrimitiveFunction) argTypesFor(argIndex int) uint32 {
	if argIndex >= len(self.ArgTypes) {
		return self.ArgTypes[len(self.ArgTypes)-1]
	} else {
		return self.ArgTypes[argIndex]
	}
}

func (self *PrimitiveFunction) internalApply(args *Data, env *SymbolTableFrame, shouldEval bool) (result *Data, err error) {
	if !self.checkArgumentCount(Length(args)) {
		err = ProcessError(fmt.Sprintf("Wrong number of args to %s, expected %s but got %d.", self.Name, self.argsString(), Length(args)), env)
		return
	}

	argArray := make([]*Data, 0)
	var argValue *Data
	for a := args; NotNilP(a); a = Cdr(a) {
		if self.Special || !shouldEval {
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
		err = ProcessError(fmt.Sprintf("Wrong argument type for argument %d of %s; expected %s but got the %s: %s", argCheckResult, self.Name, self.typesToString(self.argTypesFor(argCheckResult)), TypeName(TypeOf(argArray[argCheckResult])), String(argArray[argCheckResult])), env)
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

func (self *PrimitiveFunction) Apply(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return self.internalApply(args, env, true)
}

func (self *PrimitiveFunction) ApplyWithoutEval(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	return self.internalApply(args, env, false)
}
