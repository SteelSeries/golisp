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

		// Using `isJustNum`, `isGTE`, and `isRange` to check whether the subsequent calls to `fmt.Sscanf()` will fail.
		// This avoids a Golang runtime bug caused by `fmt.Sscanf()` using panic+recover internally to return an error.
		// The bug would clutter the stack trace reported by the race detector when run with `-race`.
		//
		// See the following Go github issues regarding the underlying runtime bug
		// 		https://github.com/golang/go/issues/46095
		// 		https://github.com/golang/go/issues/26813
		if isJustNum(term) {
			n, _ := fmt.Sscanf(term, "%d", &intTerm)
			if n == 1 {
				argRestrictions = append(argRestrictions, ArgRestriction{Type: ARGS_EQ, Restriction: intTerm})
				continue
			}
		}

		if isGTE(term) {
			n, _ := fmt.Sscanf(term, ">=%d", &intTerm)
			if n == 1 {
				argRestrictions = append(argRestrictions, ArgRestriction{Type: ARGS_GTE, Restriction: intTerm})
				continue
			}
		}
		if isRange(term) {
			var lo int
			var hi int
			n, _ := fmt.Sscanf(term, "(%d,%d)", &lo, &hi)
			if n == 2 {
				//lo <= argCount && argCount <= hi
				argRestrictions = append(argRestrictions, ArgRestriction{Type: ARGS_RANGE, Restriction: RangeRestriction{Lo: lo, Hi: hi}})
				continue
			}
		}
	}

	self.ArgRestrictions = argRestrictions
}

func isJustNum(str string) bool {
	// min length is 1 eg "0"
	if len(str) < 1 {
		return false
	}

	runes := []rune(str)
	for _, r := range runes {
		if !(r >= '0' && r <= '9') {
			return false
		}
	}

	return true
}

func isRange(str string) bool {
	// min length is 5 eg "(0,0)"
	if len(str) < 5 {
		return false
	}

	runes := []rune(str)

	// first and last must be open and close paren
	if runes[0] != '(' || runes[len(runes)-1] != ')' {
		return false
	}

	// other values must be number or comma (just one comma though)
	commaCount := 0
	for i, r := range runes[1 : len(runes)-1] {
		if i > 0 && i < len(runes)-3 && r == ',' {
			commaCount++
			continue
		}
		if !(r >= '0' && r <= '9') {
			return false
		}
	}

	return commaCount == 1
}

func isGTE(str string) bool {
	// min length is 3 eg ">=0"
	if len(str) < 3 {
		return false
	}

	runes := []rune(str)
	if runes[0] != '>' || runes[1] != '=' {
		return false
	}

	for _, r := range runes[2:] {
		if !(r >= '0' && r <= '9') {
			return false
		}
	}
	return true
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
