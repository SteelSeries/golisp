// Copyright 2013 SteelSeries ApS. All rights reserved.
// No license is given for the use of this source code.

// This package impliments a basic LISP interpretor for embedding in a go program for scripting.
package golisp

import (
    . "launchpad.net/gocheck"
)

type NumberAtomSuite struct {
    n Number
}

var _ = Suite(&NumberAtomSuite{})

func (s *NumberAtomSuite) SetUpTest(c *C) {
    s.n = NumberWithValue(5)
}

func (s *NumberAtomSuite) TestNumericValue(c *C) {
    c.Check(s.n.IntValue(), Equals, 5)
}

func (s *NumberAtomSuite) TestString(c *C) {
    c.Check(s.n.String(), Equals, "5")
}

func (s *NumberAtomSuite) TestEval(c *C) {
    c.Check(s.n.Eval(), Equals, s.n)
}

func (s *NumberAtomSuite) TestBooleanValue(c *C) {
    c.Check(s.n.BooleanValue(), Equals, true)
}
