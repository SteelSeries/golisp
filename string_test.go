// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//This package implements a basic LISP interpreter for embedding in a go program for scripting.
// This file implements tests for string atoms.

package golisp

import (
	. "gopkg.in/check.v1"
)

type StringAtomSuite struct {
	atom *Data
}

var _ = Suite(&StringAtomSuite{})

func (s *StringAtomSuite) SetUpTest(c *C) {
	s.atom = StringWithValue("Hello, world.")
}

func (s *StringAtomSuite) TestIntegerValue(c *C) {
	c.Assert(IntegerValue(s.atom), Equals, int64(0))
}

func (s *StringAtomSuite) TestString(c *C) {
	c.Assert(String(s.atom), Equals, `"Hello, world."`)
}

func (s *StringAtomSuite) TestStringValue(c *C) {
	c.Assert(StringValue(s.atom), Equals, `Hello, world.`)
}

func (s *StringAtomSuite) TestStringValueOfNil(c *C) {
	c.Assert(StringValue(nil), Equals, ``)
}

func (s *StringAtomSuite) TestStringValueOfNonString(c *C) {
	c.Assert(StringValue(IntegerWithValue(42)), Equals, ``)
}

func (s *StringAtomSuite) TestEval(c *C) {
	d, err := Eval(s.atom, Global)
	c.Assert(err, IsNil)
	c.Assert(d, Equals, s.atom)
}

func (s *StringAtomSuite) TestBooleanValue(c *C) {
	c.Assert(BooleanValue(s.atom), Equals, true)
}
