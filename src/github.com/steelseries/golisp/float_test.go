// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
package golisp

import (
    . "launchpad.net/gocheck"
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
    c.Assert(FloatValue(s.n), Equals, float32(5.3))
}

func (s *FloatAtomSuite) TestNegativeFloatValue(c *C) {
    c.Assert(FloatValue(s.neg), Equals, float32(-12.345))
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
