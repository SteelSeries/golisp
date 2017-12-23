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

func (pf *PrimitiveFunction) parseNumArgs(argCount string) {
	var argRestrictions []ArgRestriction

	for _, term := range strings.Split(argCount, "|") {
		if term == "*" {
			pf.ArgRestrictions = []ArgRestriction{{Type: ARGS_ANY}}
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

	pf.ArgRestrictions = argRestrictions
}

func (pf *PrimitiveFunction) argsString() string {
	parts := make([]string, len(pf.ArgRestrictions))
	for i, term := range pf.ArgRestrictions {
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

func (pf *PrimitiveFunction) String() string {
	return fmt.Sprintf("<prim: %s, %v>", pf.Name, &pf.Body)
}

func (pf *PrimitiveFunction) checkArgumentCount(argCount int) bool {
	for _, term := range pf.ArgRestrictions {
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

func (pf *PrimitiveFunction) Apply(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if pf.IsRestricted && env.IsRestricted {
		err = fmt.Errorf("The %s primitive is restricted from execution in this environment\n", pf.Name)
		return
	}

	if !pf.checkArgumentCount(Length(args)) {
		err = ProcessError(fmt.Sprintf("Wrong number of args to %s, expected %s but got %d.", pf.Name, pf.argsString(), Length(args)), env)
		return
	}

	argArray := make([]*Data, 0)
	var argValue *Data
	for a := args; NotNilP(a); a = Cdr(a) {
		if pf.Special {
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
	if pf.Special {
		fType = "form"
	}

	ProfileEnter(fType, pf.Name, localGuid)

	result, err = (pf.Body)(ArrayToList(argArray), env)

	ProfileExit(fType, pf.Name, localGuid)

	return
}

func (pf *PrimitiveFunction) ApplyWithoutEval(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if pf.Special {
		return pf.Apply(args, env)
	} else {
		return pf.Apply(QuoteAll(args), env)
	}
}
