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

type ArityChecker interface {
	Matches(actualArgumentCount int) bool
	AsString() string
}

type AnyNumberChecker struct {
}

func (self AnyNumberChecker) Matches(actualArgumentCount int) bool {
	return true
}

func (self AnyNumberChecker) AsString() string {
	return "*"
}

type SingleNumberChecker struct {
	Value int
}

func (self SingleNumberChecker) Matches(actualArgumentCount int) bool {
	return actualArgumentCount == self.Value
}

func (self SingleNumberChecker) AsString() string {
	return fmt.Sprintf("%d", self.Value)
}

type RangeChecker struct {
	Minimum int
	Maximum int
}

func (self RangeChecker) Matches(actualArgumentCount int) bool {
	return actualArgumentCount >= self.Minimum && actualArgumentCount <= self.Maximum
}

func (self RangeChecker) AsString() string {
	return fmt.Sprintf("(%d,%d)", self.Minimum, self.Maximum)
}

type MinimumChecker struct {
	Minimum int
}

func (self MinimumChecker) Matches(actualArgumentCount int) bool {
	return actualArgumentCount >= self.Minimum
}

func (self MinimumChecker) AsString() string {
	return fmt.Sprintf(">=%d", self.Minimum)
}

type AlternativesChecker struct {
	Values []int
}

func (self AlternativesChecker) Matches(actualArgumentCount int) bool {
	for _, c := range self.Values {
		if actualArgumentCount == c {
			return true
		}
	}
	return false
}

func (self AlternativesChecker) AsString() string {
	stringValues := make([]string, 0, len(self.Values))
	for _, v := range self.Values {
		stringValues = append(stringValues, fmt.Sprintf("%d", v))
	}
	return strings.Join(stringValues, "|")
}

func makeArityChecker(aritySpecifier string) ArityChecker {
	if aritySpecifier == "*" {
		return &AnyNumberChecker{}
	}

	var n int
	var intTerm int
	var lo int
	var hi int
	n, _ = fmt.Sscanf(aritySpecifier, "(%d,%d)", &lo, &hi)
	if n == 2 {
		return &RangeChecker{Minimum: lo, Maximum: hi}
	}

	n, _ = fmt.Sscanf(aritySpecifier, ">=%d", &intTerm)
	if n == 1 {
		return &MinimumChecker{Minimum: intTerm}
	}

	terms := strings.Split(aritySpecifier, "|")
	vals := make([]int, 0, len(terms))
	for _, term := range terms {
		var intTerm int
		n, _ := fmt.Sscanf(term, "%d", &intTerm)
		if n == 1 {
			vals = append(vals, intTerm)
		} else {
			panic(fmt.Sprintf("Invalid artity specifier: %d", aritySpecifier))
		}
	}
	if len(vals) == 1 {
		return &SingleNumberChecker{Value: vals[0]}
	} else {
		return &AlternativesChecker{Values: vals}
	}
}

type PrimitiveFunction struct {
	Name         string
	Special      bool
	Arity        ArityChecker
	Body         func(d *Data, env *SymbolTableFrame) (*Data, error)
	IsRestricted bool
}

func MakePrimitiveFunction(name string, argCount string, function func(*Data, *SymbolTableFrame) (*Data, error)) {
	f := &PrimitiveFunction{Name: name, Special: false, Arity: makeArityChecker(argCount), Body: function, IsRestricted: false}
	sym := Global.Intern(name)
	Global.BindTo(sym, PrimitiveWithNameAndFunc(name, f))
}

func MakeRestrictedPrimitiveFunction(name string, argCount string, function func(*Data, *SymbolTableFrame) (*Data, error)) {
	f := &PrimitiveFunction{Name: name, Special: false, Arity: makeArityChecker(argCount), Body: function, IsRestricted: true}
	sym := Global.Intern(name)
	Global.BindTo(sym, PrimitiveWithNameAndFunc(name, f))
}

func MakeSpecialForm(name string, argCount string, function func(*Data, *SymbolTableFrame) (*Data, error)) {
	f := &PrimitiveFunction{Name: name, Special: true, Arity: makeArityChecker(argCount), Body: function, IsRestricted: false}
	sym := Global.Intern(name)
	Global.BindTo(sym, PrimitiveWithNameAndFunc(name, f))
}

func MakeRestrictedSpecialForm(name string, argCount string, function func(*Data, *SymbolTableFrame) (*Data, error)) {
	f := &PrimitiveFunction{Name: name, Special: true, Arity: makeArityChecker(argCount), Body: function, IsRestricted: true}
	sym := Global.Intern(name)
	Global.BindTo(sym, PrimitiveWithNameAndFunc(name, f))
}

func (self *PrimitiveFunction) String() string {
	return fmt.Sprintf("<prim: %s, %v>", self.Name, self.Body)
}

func (self *PrimitiveFunction) Apply(args *Data, env *SymbolTableFrame) (result *Data, err error) {
	if self.IsRestricted && env.IsRestricted {
		err = fmt.Errorf("The %s primitive is restricted from execution in this environment\n", self.Name)
		return
	}

	if !self.Arity.Matches(Length(args)) {
		err = fmt.Errorf("Wrong number of args to %s. Expected %s but got %d.\n", self.Name, self.Arity.AsString(), Length(args))
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
