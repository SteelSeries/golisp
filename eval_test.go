// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file tests function evaluation.

package golisp

import (
	. "launchpad.net/gocheck"
)

type EvalSuite struct {
}

var _ = Suite(&EvalSuite{})

func (s *EvalSuite) SetUpSuite(c *C) {
	Global = NewSymbolTableFrameBelow(nil)
	InitBuiltins()
}

func (s *EvalSuite) TestEvalWithNilFunction(c *C) {
	code, _ := Parse("(no-function-named-this)")
	result, err := Eval(code, Global)
	c.Assert(err, NotNil)
	c.Assert(result, IsNil)
}

func (s *EvalSuite) TestEvalWithOverridingSearchEnv(c *C) {
	newEnv := NewSymbolTableFrameBelow(Global)
	sym := SymbolWithName("test")
	code, _ := Parse("test")
	Global.BindLocallyTo(sym, IntegerWithValue(10))
	newEnv.BindLocallyTo(sym, IntegerWithValue(42))
	globalVal, _ := Eval(code, Global)
	c.Assert(IntegerValue(globalVal), Equals, int64(10))
	newEnvVal, _ := Eval(code, Global, newEnv)
	c.Assert(IntegerValue(newEnvVal), Equals, int64(42))
}

func (s *EvalSuite) TestIndirectEvalWithOverridingSearchEnv(c *C) {
	newEnv := NewSymbolTableFrameBelow(Global)
	sym := SymbolWithName("test")
	code, _ := Parse("(+ (* test 2) 1)")
	Global.BindLocallyTo(sym, IntegerWithValue(10))
	newEnv.BindLocallyTo(sym, IntegerWithValue(42))
	globalVal, _ := Eval(code, Global)
	c.Assert(IntegerValue(globalVal), Equals, int64(21))
	newEnvVal, _ := Eval(code, Global, newEnv)
	c.Assert(IntegerValue(newEnvVal), Equals, int64(85))
}
