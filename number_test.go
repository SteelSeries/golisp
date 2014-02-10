// Copyright 2014 SteelSeries ApS.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package implements a basic LISP interpretor for embedding in a go program for scripting.
// This file tests the integer type.

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
    c.Assert(NumericValue(s.n), Equals, uint32(5))
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
