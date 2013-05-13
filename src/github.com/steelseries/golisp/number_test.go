// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
package golisp

import (
    . "launchpad.net/gocheck"
)

type NumberAtomSuite struct {
    n *Data
}

var _ = Suite(&NumberAtomSuite{})

func (s *NumberAtomSuite) SetUpTest(c *C) {
    s.n = NumberWithValue(5)
}

func (s *NumberAtomSuite) TestNumericValue(c *C) {
    c.Assert(IntValue(s.n), Equals, 5)
}

func (s *NumberAtomSuite) TestString(c *C) {
    c.Assert(String(s.n), Equals, "5")
}

func (s *NumberAtomSuite) TestEval(c *C) {
    n, err := Eval(s.n, Global)
    c.Assert(err, IsNil)
    c.Assert(n, Equals, s.n)
}

func (s *NumberAtomSuite) TestBooleanValue(c *C) {
    c.Assert(BooleanValue(s.n), Equals, true)
}
