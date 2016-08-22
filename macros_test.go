// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file tests built-in primitive functions.

package golisp

import (
	. "gopkg.in/check.v1"
)

type MacrosSuite struct {
}

var _ = Suite(&BuiltinsSuite{})

func (s *MacrosSuite) SetUpSuite(c *C) {
	InitLisp()
}

func (s *BuiltinsSuite) TestNoUnquoting(c *C) {
	code, _ := Parse("`(+ a 1)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(String(result), Equals, "(+ a 1)")
}

func (s *BuiltinsSuite) TestUnquotingInteger(c *C) {
	code, _ := Parse("`(+ a ,1)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(String(result), Equals, "(+ a 1)")
}

func (s *BuiltinsSuite) TestUnquotingSymbol(c *C) {
	_, err := Global.BindTo(SymbolWithName("a"), IntegerWithValue(5))
	c.Assert(err, IsNil)
	code, _ := Parse("`(+ ,a 1)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(String(result), Equals, "(+ 5 1)")
}

func (s *BuiltinsSuite) TestUnquotingExpression(c *C) {
	_, err := Global.BindTo(SymbolWithName("a"), IntegerWithValue(5))
	c.Assert(err, IsNil)
	code, _ := Parse("`(+ ,(+ a 1) 1)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(String(result), Equals, "(+ 6 1)")
}

func (s *BuiltinsSuite) TestUnquoteSplicing(c *C) {
	_, err := Global.BindTo(SymbolWithName("a"), IntegerWithValue(5))
	c.Assert(err, IsNil)
	code, _ := Parse("`(+ ,@(list 1 2 3) 1)")
	result, err := Eval(code, Global)
	c.Assert(err, IsNil)
	c.Assert(result, NotNil)
	c.Assert(String(result), Equals, "(+ 1 2 3 1)")
}
