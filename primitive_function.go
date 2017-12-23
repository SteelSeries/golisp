// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpreter for embedding in a go program for scripting.
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
	Type        int
	Restriction interface{}
}

type RangeRestriction struct {
	Hi int
	Lo int
}

type PrimitiveFunction struct {
	Name            string
	Special         bool
	ArgRestrictions []ArgRestriction
	ArgTypes        []uint32
	Body            func(d *Data, env *SymbolTableFrame) (*Data, error)
	IsRestricted    bool
}

func MakePrimitiveFunction(name string, argCount string, function func(*Data, *SymbolTableFrame) (*Data, error)) {
	f := &PrimitiveFunction{Name: name, Special: false, Body: function, IsRestricted: false}
	f.parseNumArgs(argCount)
	sym := Intern(name)
	Global.BindToProtected(sym, PrimitiveWithNameAndFunc(name, f))
}

func MakeRestrictedPrimitiveFunction(name string, argCount string, function func(*Data, *SymbolTableFrame) (*Data, error)) {
	f := &PrimitiveFunction{Name: name, Special: false, Body: function, IsRestricted: true}
	f.parseNumArgs(argCount)
	sym := Intern(name)
	Global.BindToProtected(sym, PrimitiveWithNameAndFunc(name, f))
}

func MakeSpecialForm(name string, argCount string, function func(*Data, *SymbolTableFrame) (*Data, error)) {
	f := &PrimitiveFunction{Name: name, Special: true, Body: function, IsRestricted: false}
	f.parseNumArgs(argCount)
	sym := Intern(name)
	Global.BindToProtected(sym, PrimitiveWithNameAndFunc(name, f))
}

func MakeRestrictedSpecialForm(name string, argCount string, function func(*Data, *SymbolTableFrame) (*Data, error)) {
	f := &PrimitiveFunction{Name: name, Special: true, Body: function, IsRestricted: true}
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
			argRestrictions = append(argRestrictions, ArgRestriction{Type: ARGS_EQ, Restriction: intTerm})
			continue
		}
		n, _ = fmt.Sscanf(term, ">=%d", &intTerm)
		if n == 1 {
			argRestrictions = append(argRestrictions, ArgRestriction{Type: ARGS_GTE, Restriction: intTerm})
			continue
		}
		var lo int
		var hi int
		n, _ = fmt.Sscanf(term, "(%d,%d)", &lo, &hi)
		if n == 2 {
			//lo <= argCount && argCount <= hi
			argRestrictions = append(argRestrictions, ArgRestriction{Type: ARGS_RANGE, Restriction: RangeRestriction{Lo: lo, Hi: hi}})
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
			parts[i] = fmt.Sprintf("%d", term.Restriction.(int))
		case ARGS_GTE:
			parts[i] = fmt.Sprintf(">=%d", term.Restriction.(int))
		case ARGS_RANGE:
			argRange := term.Restriction.(RangeRestriction)
			parts[i] = fmt.Sprintf("(%d,%d)", argRange.Lo, argRange.Hi)
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
			if argCount == term.Restriction.(int) {
				return true
			}
		case ARGS_GTE:
			if argCount >= term.Restriction.(int) {
				return true
			}
		case ARGS_RANGE:
			argRange := term.Restriction.(RangeRestriction)
			if argRange.Lo <= argCount && argCount <= argRange.Hi {
				return true
			}
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
		err = ProcessError(fmt.Sprintf("Wrong number of args to %s, expected %s but got %d.", self.Name, self.argsString(), Length(args)), env)
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
