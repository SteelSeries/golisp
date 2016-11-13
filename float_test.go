// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file tests the integer type.

package golisp

import (
	. "gopkg.in/check.v1"
)

type FloatAtomSuite struct {
	n    *Data
	neg  *Data
	zero *Data
}

var _ = Suite(&FloatAtomSuite{})

func (s *FloatAtomSuite) SetUpTest(c *C) {
	s.n = FloatWithValue(5.3)
	s.neg = FloatWithValue(-12.345)
	s.zero = FloatWithValue(0.0)
}

func (s *FloatAtomSuite) TestFloatValue(c *C) {
	c.Assert(FloatValue(s.n), Equals, float64(5.3))
}

func (s *FloatAtomSuite) TestFloatValueOfNil(c *C) {
	c.Assert(FloatValue(nil), Equals, float64(0.0))
}

func (s *FloatAtomSuite) TestFloatValueOfNonFloat(c *C) {
	c.Assert(FloatValue(StringWithValue("3.2")), Equals, float64(0.0))
}

func (s *FloatAtomSuite) TestNegativeFloatValue(c *C) {
	c.Assert(FloatValue(s.neg), Equals, float64(-12.345))
}

func (s *FloatAtomSuite) TestString(c *C) {
	c.Assert(String(s.n), Equals, "5.3")
}

func (s *FloatAtomSuite) TestNegativeString(c *C) {
	c.Assert(String(s.neg), Equals, "-12.345")
}

func (s *FloatAtomSuite) TestEval(c *C) {
	n, err := Eval(s.n, Global)
	c.Assert(err, IsNil)
	c.Assert(n, Equals, s.n)
}

func (s *FloatAtomSuite) TestNegativeEval(c *C) {
	n, err := Eval(s.neg, Global)
	c.Assert(err, IsNil)
	c.Assert(n, Equals, s.neg)
}

func (s *FloatAtomSuite) TestBooleanValue(c *C) {
	c.Assert(BooleanValue(s.n), Equals, true)
	c.Assert(BooleanValue(s.neg), Equals, true)
	c.Assert(BooleanValue(s.zero), Equals, true)
}
